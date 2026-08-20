#' age_length_summary
#'
#' Plot mean length by age by species
#'
#' @param SurveyEffortData "Effort" query from FISH_query function
#' @param OutputType "RawData", "Table" or "Figure"
#' @return ggplot
#' @export
#'

#' @details Pulls in survey level age-length data; weighted mean lengths estiamted using inch group catches; if not provided, it pulls in all efforts


age_length_summary<-function(SurveyEffortData,OutputType="RawData"){
  
  # #count the number of fish (SurveyId+EnvelopeSerialNumber) with multiple entries, subset out
  # #based on the initial reveiw there are 2,940 with multiple entires, but they are all identical age and length.. seems suspicious? #8
  # multiEntries <- tbl(con, "ModuleDataScaleEnvelope") %>%
  #   select(SurveyId, EnvelopeSerialNumber, TotalLengthInInches, AgeClassId) %>%
  #   group_by(SurveyId, EnvelopeSerialNumber) %>%
  #   summarise(
  #     numEntry = n(),
  #     n_lengths = n_distinct(TotalLengthInInches),
  #     n_ages=n_distinct(AgeClassId),
  #     .groups = "drop"
  #   ) %>%
  #   filter(numEntry > 1) %>%
  #   inner_join(tbl(con, "ModuleDataScaleEnvelope"),by = c("SurveyId", "EnvelopeSerialNumber"))%>%
  #   collect()
  
  
  #note- currently pulls all age data from surveys and inch group data from all efforts in query
  #pull moduleIds from the data; if not provided, pull by surveyIDs
  SurveyIds <- unique(SurveyEffortData$SurveyId)
  modIds <- unique(SurveyEffortData$ModuleId)
  

  
  #do a check for inconsistent lengths; this could happen when multiple ages are entered in FISHub
  #none found when the function was developed (see above multiEntries table)
  length_check <- tbl(con, "ModuleDataScaleEnvelope") %>%
    filter(SurveyId %in% !!SurveyIds) %>% 
    collect()%>%
    group_by(SurveyId, EnvelopeSerialNumber) %>%
    summarise(
      n_lengths = n_distinct(TotalLengthInInches, na.rm = TRUE),
      .groups = "drop"
    )%>%
    filter(n_lengths>1)
  
  if (nrow(length_check) > 0) {
    stop(paste0("ERROR: Inconsistent lengths detected. Check age/length data."))
  }
  
  #query data; currently set up to do mode by SurveyId,EnvelopeSerialNumber,Species,Strain,and TotalLengthInInches
  #circle back to this once we confirm the age data (#8)
  scaleEnvelope <- tbl(con, "ModuleDataScaleEnvelope") %>%
    filter(SurveyId %in% !!SurveyIds) %>% 
    filter(!is.na(TotalLengthInInches))%>% #do we want to filter these? revist after age data confirmation
    select(SurveyId,ModuleDataId,EnvelopeSerialNumber,SpeciesStrainId,TotalLengthInInches,AgeClassId)%>%
    left_join(tbl(con, "AgeClass")%>%
                select(AgeClassId,Descriptions),by = "AgeClassId")%>%
    left_join(tbl(con, "ModuleData") %>%
                select(ModuleId,ModuleDataId),by = "ModuleDataId")%>%
    left_join(tbl(con, "SpeciesStrain") %>%
                select(SpeciesStrainId,Species,Strain),by = "SpeciesStrainId")%>%
    group_by(SurveyId,EnvelopeSerialNumber,Species,Strain,TotalLengthInInches,Descriptions)%>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(SurveyId,EnvelopeSerialNumber,Species,Strain,TotalLengthInInches)%>%
    slice_max(n, n = 1, with_ties = FALSE) %>%
    select(SurveyId,EnvelopeSerialNumber,Species,Strain,Age = Descriptions,TotalLengthInInches)%>%
    collect()
  
  if(OutputType=="RawData"){
    return(scaleEnvelope)
    stop()
  }
  
  
  #Simple mean
  meanAgeDat<-scaleEnvelope%>%
    group_by(SurveyId,Species,Age)%>%
    summarise(N=length(TotalLengthInInches),
              Mean_Length=round(mean(TotalLengthInInches),2),
              Min_length=min(TotalLengthInInches),
              Max_Length=max(TotalLengthInInches),
              SD_Length=round(sd(TotalLengthInInches),2),
              .groups = "drop")
  
  
  #Weighted mean (FD protocol; chap 15 by Schneider)
  #get catch by inch data
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
    catchInchSurvey<-SurveyEffortData%>%
      select(SurveyId,ModuleId)%>%
      unique()%>%
      left_join(catchInch,by="ModuleId")
    
    #summarize by survey and species
    lengthFreq <- catchInchSurvey %>%
      filter(!is.na(Species))%>% #filter out blank rows (due to efforts with no catches)
      group_by(SurveyId,Species,InchGroup) %>%
      summarize(N.caught=sum(NumberCaughtUnmarked),.groups = "drop")#only use unmarked here to avoid double counting fish
      
    alk_prop<-scaleEnvelope%>%
      mutate(InchGroup=floor(TotalLengthInInches))%>%
      group_by(SurveyId,Species,InchGroup,Age)%>%
      summarize(N.aged=n(),
                meanLength=mean(TotalLengthInInches),
                .groups = "drop")%>%
      group_by(SurveyId, Species, InchGroup) %>%
      mutate(prop = N.aged / sum(N.aged)) %>%
      ungroup()
      
    ageLen <- lengthFreq %>%
      inner_join(alk_prop, by = c("SurveyId", "Species", "InchGroup")) %>%
      mutate(relNumber = N.caught * prop,
             relNumber_meanLength=relNumber*meanLength)%>%
       group_by(SurveyId, Species, Age) %>%
       summarise(
         Weighted_Mean_Length = round(sum(relNumber_meanLength)/sum(relNumber),2),
         .groups = "drop"
        )
    
    #combine weighted avg and simple
    outTab<-meanAgeDat%>%
      left_join(ageLen,by= c("SurveyId", "Species", "Age"))%>%
      mutate(Age=as.integer(Age))%>%
      arrange(Species,Age)
    
    if(OutputType=="Table"){
        return(outTab)
    }
  }else{
    warning("No catch-by-inch group data available; returning simple means only.")
    if(OutputType=="Table"){
      return(meanAgeDat)
    }
  }
  if(OutputType=="Figure"){
    outPlot<-ggplot(outTab)+
      geom_line(aes(x=Age,y=Mean_Length,group=SurveyId,color=as.factor(SurveyId)))+
      geom_errorbar(aes(x=Age,ymin=Mean_Length-SD_Length,ymax=Mean_Length+SD_Length,color=as.factor(SurveyId)),width=.5)+
      facet_wrap(~Species)+
      theme_classic()+
      ylab("Mean Length +/- 1 SD (in)")+xlab("Age (years)")+
      guides(color=guide_legend(title="Survey Number"))
    return(outPlot)
  }
}
