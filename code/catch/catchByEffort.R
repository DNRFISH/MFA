#' catchByEffort
#'
#' Creates table of catches by effort from the effort query (FISH_query)

#'
#' @param FISH_Data Data query from FISH_query function
#' @return Catch data table
#' @export
#'
#'


catch_summary_table <- function(FISH_Data) {
  
  #pull moduleIds from the data
  modIds <- unique(FISH_Data$ModuleId)

  
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
  
  surveyCatch<-FISH_Data%>%
    left_join(catchSpecies,by="ModuleId")
  
  #check to see if there was any data in the query
  #NOTE: need to verify this with a survey that has some data
  if (nrow(catchSpecies)>0) {
    #join with survey/effort data
    catchSpeciesSurvey<-FISH_Data%>%
      left_join(catchSpecies,by="ModuleId")
    
    #summarize by survey and species
    catchSpeciesSum <- catchSpeciesSurvey %>%
      filter(!is.na(Species))%>% #filter out blank rows (due to efforts with no catches)
      group_by(SurveyId, ModuleId,SurveyEffortId,ModuleDataId,Species) %>%
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
    filter(ModuleId %in% !!modIds) %>% 
    left_join(tbl(con, "SpeciesStrain") %>%
                select(SpeciesStrainId,Species,Strain),by = "SpeciesStrainId")%>%
    collect()
  
  #check to see if there was any data in the query
  if (nrow(catchInch)>0) {
    #join with survey/effort data
    catchInchSurvey<-FISH_Data%>%
      left_join(catchInch,by="ModuleId")
    
    #summarize by survey and species
    #*note- this uses inch group as the "best estimate" of length
    catchInchSum <- catchInchSurvey %>%
      filter(!is.na(Species))%>% #filter out blank rows (due to efforts with no catches)
      mutate(
        count = NumberCaughtUnmarked) %>% #only use unmarked here to avoid double counting fish
      group_by(SurveyId, ModuleId,SurveyEffortId,ModuleDataId,Species) %>%
      summarise(
        TotalNumberCaught = sum(count, na.rm = TRUE),
        LengthAverage = round(weighted.mean(
          InchGroup,
          w = count,
          na.rm = TRUE
        ),2),
        LengthMinimum = min(InchGroup, na.rm = TRUE),
        LengthMaximum = max(InchGroup, na.rm = TRUE),
        Source = "InchGroup",
        .groups = "drop"
      )
  }else{print("No ModuleDataCatchSampleByInchGroup data available")}
  
  #################################################################################
  #individual data  ####
  #################################################################################
  scaleEnvelope <- tbl(con, "ModuleDataScaleEnvelope") %>%
    select(SurveyId,ModuleDataId,EnvelopeSerialNumber,SpeciesStrainId,TotalLengthEntered)%>%
    left_join(tbl(con, "ModuleData") %>%
                select(ModuleId,ModuleDataId),by = "ModuleDataId")%>%
    filter(ModuleId %in% !!modIds) %>% 
    left_join(tbl(con, "SpeciesStrain") %>%
                select(SpeciesStrainId,Species,Strain),by = "SpeciesStrainId")%>%
    #had to deal with multiple agers- set it up as mean length by serial number 
    group_by(SurveyId,ModuleId,ModuleDataId,EnvelopeSerialNumber,SpeciesStrainId,Species,Strain,TotalLengthEntered)%>%
    summarise(TotalLengthEntered=mean(TotalLengthEntered,na.rm = T), .groups = "drop")%>%
    collect()
  
  #check to see if there was any data in the query
  #*NOTE: need to verify this with a survey that has some data
  if (nrow(scaleEnvelope)>0) {
    #join with survey/effort data
    scaleEnvelopeSurvey<-scaleEnvelope%>%
      left_join(FISH_Data%>%select(SurveyId)%>%distinct(),by="SurveyId") #*how do we filter this by gear/effort?
    
    
    #summarize by survey and species
    scaleEnvelopeSum <- scaleEnvelopeSurvey %>%
      group_by(SurveyId, ModuleId,SurveyEffortId,ModuleDataId,Species) %>%
      summarise(
        TotalNumberCaught = sum(n(), na.rm = TRUE),
        LengthAverage = round(mean(TotalLengthEntered, na.rm = TRUE),2),
        LengthMinimum = min(TotalLengthEntered, na.rm = TRUE),
        LengthMaximum = max(TotalLengthEntered, na.rm = TRUE),
        Source = "ScaleEnvelope",
        .groups = "drop"
      )
  }else{print("No ModuleDataScaleEnvelope data available; NOTE: data entered into FISH may be missing! Contact MFA team if issue is identifed.")}
  
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