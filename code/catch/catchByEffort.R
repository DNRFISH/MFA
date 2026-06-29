#' catchByEffort
#'
#' Creates summary of catches by species and efforts

#'
#' @param FISH_Data Data query from FISH_query function
#' @param Special_Legal_Sizes Optional: List of legal sizes by species e.g., c("Largemouth Bass" = 10, "Brook Trout" = 12)
#' @return Table of catches by species and efforts
#' @export
#'

#NOTE: avg length doesn't currently line up with SFRs because of data issues identifed during FISH development

catchByEffort <- function(FISH_Data,Special_Legal_Sizes = NULL) {
  
  #pull moduleIds from the data
  modIds <- unique(FISH_Data$ModuleId)

  #specify default legal sizes (commented species don't have MSLs)
  legalSizeTib<-c(
    #"Black Crappie"=7,
    "Brook Trout"=7,
    #"Bluegill"=6,
    "Brown Trout"=8,
    #"Brown Bullhead"=7,
    "Channel Catfish"=12,
    "Chinook Salmon"=8,
    "Coho Salmon"=8,
    "Flathead Catfish"=15,
    #"Green Sunfish"=6,
    #"Hybrid Sunfish"=6,
    "Lake Trout"=10,
    "Largemouth Bass"=14,
    #"Longear Sunfish"=6,
    #"Lake Whitefish"=8,
    "Muskellunge"=42,
    "Northern Pike"=24,
    #"Orangespotted Sunfish"=6,
    "Pink Salmon"=7,
    #"Pumpkinseed"=6,
    "Rainbow Trout"=8,
    #"Rock Bass"=6,
    #"Redear Sunfish"=6,
    "Smallmouth Bass"=14,
    "Splake"=10,
    #"Lake Sturgeon"=50,
    "Walleye"=15
    #"Warmouth"=6,
    #"White Crappie"=7,
    #"White Bass"=7,
    #"White Perch"=7,
    #"Yellow Perch"=7,
    #"Yellow Bullhead"=7
  )
  
  #convert the legal sizes to a tibble
  legalSizeTib <- tibble::tibble(
    Species = names(legalSizeTib),
    LegalSize = as.numeric(legalSizeTib)
  )
  
  #incorporate special legal sizes (e.g., if a waterbody has special regs)
  if (!is.null(Special_Legal_Sizes)) {
    override_tbl <- tibble::tibble(
      Species = names(Special_Legal_Sizes),
      LegalSize_override = as.numeric(Special_Legal_Sizes)
    )
    legalSizeTib <- legalSizeTib %>%
      left_join(override_tbl, by = "Species") %>%
      mutate(
        LegalSize = coalesce(LegalSize_override, LegalSize)
      ) %>%
      select(-LegalSize_override)
  }
  
  #join legal sizes to species str table
  SpeciesStrain<-tbl(con, "SpeciesStrain") %>%
    select(SpeciesStrainId,Species,Strain)%>%
    collect()
  
  legalSizeTibSpeciesStr<-SpeciesStrain%>%
    left_join(legalSizeTib,by="Species")
  
  
  #################################################################################
  #catch by species data ####
  #################################################################################
  catchSpecies <- tbl(con, "ModuleDataCatchBySpecies") %>%
    select(ModuleDataId,SpeciesStrainId,TotalNumberCaught,LengthAverage,LengthMaximum,LengthMinimum)%>%
    left_join(tbl(con, "ModuleData") %>%
                select(ModuleId,ModuleDataId),by = "ModuleDataId")%>%
    filter(ModuleId %in% !!modIds) %>% #this syntax is needed to force R code into SQL for lazy loading
    collect()
  
  if (nrow(catchSpecies)>0) {
    catchSpeciesSum<-catchSpecies%>%
      #add surveyId
      left_join(FISH_Data%>%select(SurveyId,ModuleId),by="ModuleId")%>%
      group_by(SurveyId,ModuleId,SpeciesStrainId) %>%
      summarise(
        TotalNumberCaught = sum(TotalNumberCaught, na.rm = TRUE),
        #calculated weighted mean lengths -- need to deal with some species not having length data
        LengthAverage = round({
          valid <- !is.na(LengthAverage) & !is.na(TotalNumberCaught) 
          if (any(valid)) {
            weighted.mean(LengthAverage[valid], w = TotalNumberCaught[valid])
          } else {
            NA_real_
          }
        }, 2),
        LengthMinimum = {
          x <- LengthMinimum[!is.na(LengthMinimum)]
          if (length(x) > 0) min(x) else NA_real_
        },
        LengthMaximum = {
          x <- LengthMaximum[!is.na(LengthMaximum)]
          if (length(x) > 0) max(x) else NA_real_
        },
        #Can't calculate # legal; don't have individual fish lengths
        N_legal = NA,
        LegalSize = NA,
        CatchTable = "Species",
        .groups = "drop"
      )
  }
  
  #################################################################################
  #inch group data  ####
  #################################################################################
  catchInch <- tbl(con, "ModuleDataCatchSampleByInchGroup") %>%
    select(ModuleDataId,SpeciesStrainId,InchGroup,NumberCaughtUnmarked,NumberCaughtMarked)%>%
    left_join(tbl(con, "ModuleData") %>%
                select(ModuleId,ModuleDataId),by = "ModuleDataId")%>%
    filter(ModuleId %in% !!modIds) %>%
    collect()
  
  if (nrow(catchInch)>0) {
    catchInchSum<-catchInch%>%
      #add surveyId
      left_join(FISH_Data%>%select(SurveyId,ModuleId),by="ModuleId")%>%
      mutate(
        count = NumberCaughtUnmarked,#only use unmarked here to avoid double counting fish
        lengthEst=InchGroup+0.5) %>%
      left_join(legalSizeTibSpeciesStr%>%select(SpeciesStrainId,LegalSize),by="SpeciesStrainId")%>%
      group_by(SurveyId,ModuleId,SpeciesStrainId,LegalSize) %>%
      summarise(
        TotalNumberCaught = sum(count, na.rm = TRUE),
        LengthAverage = round(weighted.mean(
          lengthEst,
          w = count,
          na.rm = TRUE
        ),2),
        LengthMinimum = min(lengthEst, na.rm = TRUE),
        LengthMaximum = max(lengthEst, na.rm = TRUE),
        N_legal = sum(count[lengthEst >= LegalSize], na.rm = TRUE),
        CatchTable = "InchGroup",
        .groups = "drop"
      )
  }
  
  #################################################################################
  #individual data 
  #################################################################################
  scaleEnvelope <- tbl(con, "ModuleDataScaleEnvelope") %>%
    select(ModuleDataId,EnvelopeSerialNumber,SpeciesStrainId,TotalLengthEntered)%>%
    left_join(tbl(con, "ModuleData") %>%
                select(ModuleId,ModuleDataId),by = "ModuleDataId")%>%
    filter(ModuleId %in% !!modIds) %>% 
    collect()
  
  #check to see if there was any data in the query
  #*NOTE: need to verify this with a survey that has some data (issue #5)
  if (nrow(scaleEnvelope)>0) {
    print("New ModuleDataScaleEnvelope data available; Contact MFA team.")
    # #summarize by survey and species
    # scaleEnvelopeSum <- scaleEnvelope %>%
    #   #had to deal with multiple agers- set it up as mean length by serial number
    #   group_by(SurveyId,SpeciesStrainId,EnvelopeSerialNumber)%>%
    #   summarise(TotalLengthEntered=mean(TotalLengthEntered,na.rm = T), .groups = "drop")%>%
    #   left_join(legalSizeTibSpeciesStr%>%select(SpeciesStrainId,LegalSize),by="SpeciesStrainId")%>%
    #   group_by(SurveyId,SpeciesStrainId,LegalSize) %>%
    #   summarise(
    #     TotalNumberCaught = sum(n(), na.rm = TRUE),
    #     LengthAverage = round(mean(TotalLengthEntered, na.rm = TRUE),2),
    #     LengthMinimum = min(TotalLengthEntered, na.rm = TRUE),
    #     LengthMaximum = max(TotalLengthEntered, na.rm = TRUE),
    #     N_legal = sum(count[TotalLengthEntered >= LegalSize], na.rm = TRUE),
    #     CatchTable = "ScaleEnvelope",
    #     .groups = "drop"
    #   )
  }
  
  
  #################################################################################
  # combine data
  #################################################################################
  #merge together - skip if there wasn't any data
  #*Note: for now keeping the three CatchTables separate to help with validation- could merge these in the future?
  tables_list <- list(
    species = if (exists("catchSpeciesSum")) catchSpeciesSum else NULL,
    inch    = if (exists("catchInchSum")) catchInchSum else NULL,
    scale   = if (exists("scaleEnvelopeSum")) scaleEnvelopeSum else NULL
  )
  
  
  #join legal sizes to species str table
  SpeciesStrain<-tbl(con, "SpeciesStrain") %>%
    select(SpeciesStrainId,Species,Strain)%>%
    collect()
  
  combinedSum <- bind_rows(tables_list[!sapply(tables_list, is.null)])%>%
    left_join(SpeciesStrain,by = "SpeciesStrainId")%>%
    select(SurveyId,ModuleId,Species,TotalNumberCaught,LengthAverage,LengthMinimum,LengthMaximum,LegalSize,N_legal,CatchTable)
    
  return(combinedSum)
}


