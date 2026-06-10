#' catch_summary_table
#'
#' Creates summary table of catches by species

#'
#' @param FCS_Data FCS data query from FCS_Query function
#' @param Special_Legal_Sizes Optional: List of legal sizes by species e.g., c("Largemouth Bass" = 10, "Brook Trout" = 12)
#' @return Summary Table
#' @export
#'

#NOTE: avg length doesn't currently line up with SFRs!!

catch_summary_table <- function(FCS_Data, Special_Legal_Sizes = NULL) {
  
  #do a check to see if multiple gears are included in the table
  if (length(unique(FCS_Data$GearType)) > 1) {
    message(paste0("Warning: ",
                   length(unique(FCS_Data$GearType)),
                   " gears included in catch summary data"))
  }
  
  #pull moduleIds from the data
  modIds <- unique(FCS_Data$ModuleId)

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
  
  #################################################################################
  #read in catch by species data ####
  #################################################################################
  catchSpecies <- tbl(con, "ModuleDataCatchBySpecies") %>%
    select(ModuleDataId,SpeciesStrainId,TotalNumberCaught,LengthAverage,LengthMaximum,LengthMinimum)%>%
    left_join(tbl(con, "ModuleData") %>%
                select(ModuleId,ModuleDataId),by = "ModuleDataId")%>%
    filter(ModuleId %in% !!modIds) %>% #this syntax is needed to force R code into SQL for lazy loading
    left_join(tbl(con, "SpeciesStrain") %>%
                select(SpeciesStrainId,Species,Strain),by = "SpeciesStrainId")%>%
    collect()
  
  #check to see if there was any data in the query
  #NOTE: need to verify this with a survey that has some data
  if (nrow(catchSpecies)>0) {
    #join with survey/effort data
    catchSpeciesSurvey<-FCS_Data%>%
      left_join(catchSpecies,by="ModuleId")%>%
      left_join(legalSizeTib,by="Species")
    
    #summarize by survey and species
    catchSpeciesSum <- catchSpeciesSurvey %>%
      filter(!is.na(Species))%>% #filter out blank rows (due to efforts with no catches)
      group_by(SurveyId, Species, LegalSize) %>%
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
        #Can't calculate pct legal; don't have individual fish lengths
        PctLegal = NA,
        Source = "Species",
        .groups = "drop"
      )
  }else{print("No ModuleDataCatchBySpecies data available")}
  
  
  #################################################################################
  #inch group data  ####
  #################################################################################
  catchInch <- tbl(con, "ModuleDataCatchSampleByInchGroup") %>%
    select(ModuleDataId,SpeciesStrainId,InchGroup,NumberCaughtUnmarked,NumberCaughtMarked)%>%
    left_join(tbl(con, "ModuleData") %>%
                select(ModuleId,ModuleDataId),by = "ModuleDataId")%>%
    filter(ModuleId %in% !!modIds) %>% #this syntax is needed to force R code into SQL for lazy loading
    left_join(tbl(con, "SpeciesStrain") %>%
                select(SpeciesStrainId,Species,Strain),by = "SpeciesStrainId")%>%
    collect()
  
  #check to see if there was any data in the query
  if (nrow(catchInch)>0) {
    #join with survey/effort data
    catchInchSurvey<-FCS_Data%>%
      left_join(catchInch,by="ModuleId")%>%
      left_join(legalSizeTib,by="Species")
    
    #summarize by survey and species
    #*note- this uses inch group as the "best estiamte" of length
    catchInchSum <- catchInchSurvey %>%
      filter(!is.na(Species))%>% #filter out blank rows (due to efforts with no catches)
      mutate(
        count = coalesce(NumberCaughtUnmarked, 0) +
          coalesce(NumberCaughtMarked, 0)) %>%
      group_by(SurveyId,Species,LegalSize) %>%
      summarise(
        TotalNumberCaught = sum(count, na.rm = TRUE),
        LengthAverage = round(weighted.mean(
          InchGroup,
          w = count,
          na.rm = TRUE
        ),2),
        LengthMinimum = min(InchGroup, na.rm = TRUE),
        LengthMaximum = max(InchGroup, na.rm = TRUE),
        PctLegal = if (is.na(first(LegalSize))) {
          NA_real_
        } else {
          round(100*sum(
            InchGroup >= first(LegalSize),
            na.rm = TRUE
          ) / sum(count, na.rm = TRUE),0)
        },
        Source = "InchGroup",
        .groups = "drop"
      )
  }else{print("No ModuleDataCatchSampleByInchGroup data available")}
  
  #################################################################################
  #individual data  ####
  #################################################################################
  scaleEnvelope <- tbl(con, "ModuleDataScaleEnvelope") %>%
    #*note filter is based on surveyID for this one- moduleID doesn't work
    filter(SurveyId %in% !!FCS_Data$SurveyId) %>% #this syntax is needed to force R code into SQL for lazy loading
    select(SurveyId,ModuleDataId,EnvelopeSerialNumber,SpeciesStrainId,TotalLengthEntered)%>%
    left_join(tbl(con, "ModuleData") %>%
                select(ModuleId,ModuleDataId),by = "ModuleDataId")%>%
    left_join(tbl(con, "SpeciesStrain") %>%
                select(SpeciesStrainId,Species,Strain),by = "SpeciesStrainId")%>%
    #had to deal with multiple agers- set it up as mean length by serial number 
    group_by(SurveyId,EnvelopeSerialNumber,SpeciesStrainId,Species,Strain,TotalLengthEntered)%>%
    summarise(TotalLengthEntered=mean(TotalLengthEntered,na.rm = T), .groups = "drop")%>%
    collect()
  
  #check to see if there was any data in the query
  #*NOTE: need to verify this with a survey that has some data
  if (nrow(scaleEnvelope)>0) {
    #join with survey/effort data
    scaleEnvelopeSurvey<-scaleEnvelope%>%
      left_join(legalSizeTib,by="Species")%>%
      left_join(FCS_Data%>%select(SurveyId)%>%distinct(),by="SurveyId") #*how do we filter this by gear/effort?
  
    
    #summarize by survey and species
    scaleEnvelopeSum <- scaleEnvelopeSurvey %>%
      group_by(SurveyId,Species,LegalSize) %>%
      summarise(
        TotalNumberCaught = sum(n(), na.rm = TRUE),
        LengthAverage = round(mean(TotalLengthEntered, na.rm = TRUE),2),
        LengthMinimum = min(TotalLengthEntered, na.rm = TRUE),
        LengthMaximum = max(TotalLengthEntered, na.rm = TRUE),
        PctLegal = if (is.na(first(LegalSize))) {
          NA_real_
        } else {
          round(100*sum(
            TotalLengthEntered >= first(LegalSize),
            na.rm = TRUE
          ) / sum(n(), na.rm = TRUE),0)
        },
        Source = "ScaleEnvelope",
        .groups = "drop"
      )
  }else{print("No ModuleDataScaleEnvelope data available")}
  
  #################################################################################
  # combine data
  #################################################################################
  #merge together - skip if there wasn't any data
  #*Note: for now keeping the three sources separate to help with validation- could merge these in the future?
  tables_list <- list(
    species = if (exists("catchSpeciesSum")) catchSpeciesSum else NULL,
    inch    = if (exists("catchInchSum")) catchInchSum else NULL,
    scale   = if (exists("scaleEnvelopeSum")) scaleEnvelopeSum else NULL
  )
  
  combinedSum <- bind_rows(tables_list[!sapply(tables_list, is.null)])%>%
    select(SurveyId,Species,TotalNumberCaught,LengthAverage,LengthMinimum,LengthMaximum,LegalSize,PctLegal,Source)
  
  return(combinedSum)
}


