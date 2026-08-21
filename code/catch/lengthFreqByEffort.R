#' lengthFreqByEffort
#'
#' Creates table of length frequency by effort
#'
#' @param SurveyEffortData "Effort" query from FISH_query function
#' @param OutputType "Table" or "Figure"
#' @return Summary Table or graph
#' @export
#'
#' @details Only pulls from ModuleDataCatchSampleByInchGroup and ModuleDataScaleEnvelope because ModuleDataCatchBySpecies doesn't have inch groups

lengthFreqByEffort<-function(SurveyEffortData,OutputType="Table"){
  
  #pull moduleIds from the data
  modIds <- unique(SurveyEffortData$ModuleId)
  
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
      left_join(SurveyEffortData%>%select(SurveyId,ModuleId),by="ModuleId")%>%
      mutate(
        count = NumberCaughtUnmarked,#only use unmarked here to avoid double counting fish
        lengthEst=InchGroup+0.5) %>%
      group_by(SurveyId,ModuleId,SpeciesStrainId,InchGroup) %>%
      summarise(
        TotalNumberCaught = sum(count, na.rm = TRUE),
        CatchTable = "InchGroup",
        .groups = "drop"
      )
  }
  
  #################################################################################
  #individual data 
  #################################################################################
  scaleEnvelope <- tbl(con, "ModuleDataScaleEnvelope") %>%
    select(ModuleDataId,EnvelopeSerialNumber,SpeciesStrainId,TotalLengthInInches )%>%
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
    #   summarise(TotalLengthInInches =mean(TotalLengthInInches ,na.rm = T), .groups = "drop")%>%
    #   group_by(SurveyId,SpeciesStrainId,LegalSize) %>%
    #   summarise(
    #     TotalNumberCaught = sum(n(), na.rm = TRUE),
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
    inch    = if (exists("catchInchSum")) catchInchSum else NULL,
    scale   = if (exists("scaleEnvelopeSum")) scaleEnvelopeSum else NULL
  )

  SpeciesStrain<-tbl(con, "SpeciesStrain") %>%
    select(SpeciesStrainId,Species,Strain)%>%
    collect()
  
  combinedSum <- bind_rows(tables_list[!sapply(tables_list, is.null)])%>%
    left_join(SpeciesStrain,by = "SpeciesStrainId")%>%
    select(SurveyId,ModuleId,Species,InchGroup,TotalNumberCaught,CatchTable)
  
  if(OutputType=="Table"){
    return(combinedSum)
  }
  if(OutputType=="Figure"){
    return(ggplot(combinedSum)+geom_histogram(aes(x=InchGroup,y=TotalNumberCaught,fill=as.factor(SurveyId)),stat="identity")+
             theme_classic()+
             facet_wrap(~Species)+
             labs(fill = "SurveyId"))
  }
}