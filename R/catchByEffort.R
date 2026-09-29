#' catchByEffort
#'
#' Internal function to calculate catch by effort
#'
#' This function is used internally by [FISH_query()] to retrieve and
#' summarize catch data associated with survey effort. It was split out
#' from [FISH_query()] because of the complicated join across the three
#' catch tables.
#'
#' @param con Database connection.
#' @param effortData Survey effort data.
#' @param Special_Legal_Sizes Optional named vector of species-specific
#'   legal sizes that overrides the default legal sizes.
#'
#' @return A data frame containing catch information summarized by survey,
#'   effort, and species.
#'
#' @keywords internal

#NOTE: avg length doesn't currently line up with SFRs because of data issues identifed during FISH development

catchByEffort <- function(con,effortData,Special_Legal_Sizes = NULL) {
  
  if(!"SurveyEffortId" %in% colnames(effortData)){
    stop("No survey data included. Need to provide effort data from FISH_query [QueryType='Efforts']")
  }
  
  if("TotalNumberCaught" %in% colnames(effortData)){
    stop("Catch data included. Need to provide effort data from FISH_query [QueryType='Efforts']") #could modify this to pull out efforts
  }

  #get moduleID data
  ModuleDataSubset <- tbl(con, "ModuleData") %>%
    select(ModuleId, ModuleDataId) %>%
    inner_join(
      effortData %>%
        select(ModuleId) %>%
        distinct(),
      by = "ModuleId"
    ) %>%
    select(ModuleId, ModuleDataId)
  
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
    select(ModuleDataId,SpeciesStrainId,TotalNumberCaught,LengthAverage,LengthMaximum,LengthMinimum) %>%
    inner_join(ModuleDataSubset,by = "ModuleDataId") %>%
    collect()

  if (nrow(catchSpecies)>0) {
    catchSpeciesSum<-catchSpecies%>%
      #add surveyId
      left_join(effortData%>%select(SurveyId,ModuleId)%>%collect(),by="ModuleId")%>%
      group_by(SurveyId,ModuleId,SpeciesStrainId) %>%
      summarize(
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
    select(ModuleDataId,SpeciesStrainId,InchGroup,NumberCaughtUnmarked,NumberCaughtMarked) %>%
    inner_join(ModuleDataSubset,by = "ModuleDataId") %>%
    collect()
  
  if (nrow(catchInch)>0) {
    catchInchSum<-catchInch%>%
      #add surveyId
      left_join(effortData%>%select(SurveyId,ModuleId)%>%collect(),by="ModuleId")%>%
      mutate(
        count = NumberCaughtUnmarked,#only use unmarked here to avoid double counting fish
        lengthEst=InchGroup+0.5) %>%
      left_join(legalSizeTibSpeciesStr%>%select(SpeciesStrainId,LegalSize),by="SpeciesStrainId")%>%
      group_by(SurveyId,ModuleId,SpeciesStrainId,LegalSize) %>%
      summarize(
        TotalNumberCaught = sum(count, na.rm = TRUE),
        LengthAverage = round(weighted.mean(
          lengthEst,
          w = count,
          na.rm = TRUE
        ),2),
        LengthMinimum = min(lengthEst, na.rm = TRUE),
        LengthMaximum = max(lengthEst, na.rm = TRUE),
        N_legal = sum(count[lengthEst >= LegalSize], na.rm = FALSE), #want this to stay NA if no legal size specified
        CatchTable = "InchGroup",
        .groups = "drop"
      )
  }
  
  #################################################################################
  #individual data 
  #################################################################################
  scaleEnvelope <- tbl(con, "ModuleDataScaleEnvelope") %>%
    select(ModuleDataId,EnvelopeSerialNumber,SpeciesStrainId,TotalLengthEntered) %>%
    inner_join(ModuleDataSubset,by = "ModuleDataId") %>%
    collect()

  #check to see if there was any data in the query
  #*NOTE: need to verify this with a survey that has some data (issue #5)
  if (nrow(scaleEnvelope)>0) {
    print("New ModuleDataScaleEnvelope data available; Contact MFA team.")
    # #summarize by survey and species
    # scaleEnvelopeSum <- scaleEnvelope %>%
    #   #had to deal with multiple agers- set it up as mean length by serial number
    #   group_by(SurveyId,SpeciesStrainId,EnvelopeSerialNumber)%>%
    #   summarize(TotalLengthEntered=mean(TotalLengthEntered,na.rm = T), .groups = "drop")%>%
    #   left_join(legalSizeTibSpeciesStr%>%select(SpeciesStrainId,LegalSize),by="SpeciesStrainId")%>%
    #   group_by(SurveyId,SpeciesStrainId,LegalSize) %>%
    #   summarize(
    #     TotalNumberCaught = sum(n(), na.rm = TRUE),
    #     LengthAverage = round(mean(TotalLengthEntered, na.rm = TRUE),2),
    #     LengthMinimum = min(TotalLengthEntered, na.rm = TRUE),
    #     LengthMaximum = max(TotalLengthEntered, na.rm = TRUE),
    #     N_legal = sum(count[TotalLengthEntered >= LegalSize], na.rm = FALSE), #want this to stay NA if no legal size specified
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
    inch    = if (exists("catchInchSum")) catchInchSum else NULL
    #scale   = if (exists("scaleEnvelopeSum")) scaleEnvelopeSum else NULL #add this back in whenever we have data in this table
  )
  
  #combine tables
  combinedSum <- bind_rows(tables_list[!sapply(tables_list, is.null)])
  
  #check to confirm there are data
  if (nrow(combinedSum) == 0) {
    stop("No data found. Check query inputs.")
  }

  outDat<-combinedSum%>%
    left_join(SpeciesStrain,by = "SpeciesStrainId")%>%
    select(SurveyId,ModuleId,Species,TotalNumberCaught,LengthAverage,LengthMinimum,LengthMaximum,LegalSize,N_legal,CatchTable)
    
  return(outDat)
}


