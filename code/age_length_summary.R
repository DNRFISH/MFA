#' age_length_summary
#'
#' Plot mean length by age by species
#'
#' @param FISH_Data Data query from FISH_query function
#' @param OutputType "RawData", "Table" or "Figure"
#' @return ggplot
#' @export
#'

age_length_summary<-function(FISH_Data,OutputType="RawData"){

  #pull moduleIds from the data
  SurveyIds <- unique(FISH_Data$SurveyId)
  modIds <- unique(FISH_Data$ModuleId)

  scaleEnvelope <- tbl(con, "ModuleDataScaleEnvelope") %>%
    filter(SurveyId %in% !!SurveyIds) %>% 
    select(SurveyId,ModuleDataId,EnvelopeSerialNumber,SpeciesStrainId,TotalLengthEntered,AgeClassId)%>%
    left_join(tbl(con, "AgeClass")%>%
                select(AgeClassId,Name),by = "AgeClassId")%>%
    left_join(tbl(con, "ModuleData") %>%
                select(ModuleId,ModuleDataId),by = "ModuleDataId")%>%
    left_join(tbl(con, "SpeciesStrain") %>%
                select(SpeciesStrainId,Species,Strain),by = "SpeciesStrainId")%>%
    # #had to deal with multiple agers- set it up as mode age- no baseline mode function
    group_by(SurveyId,EnvelopeSerialNumber,Species,Strain,TotalLengthEntered,Name)%>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(SurveyId,EnvelopeSerialNumber,Species,Strain,TotalLengthEntered)%>%
    slice_max(n, n = 1, with_ties = FALSE) %>%
    select(SurveyId,EnvelopeSerialNumber,Species,Strain,Age = Name,TotalLengthEntered)%>%
    collect()
  
  if(OutputType=="RawData"){
    return(scaleEnvelope)
  }
  
  
  #Simple mean
  meanAgeDat<-scaleEnvelope%>%
    group_by(SurveyId,Species,Age)%>%
    filter(!is.na(TotalLengthEntered))%>%
    summarise(N=length(TotalLengthEntered),
              Mean_Length=round(mean(TotalLengthEntered),2),
              Min_length=min(TotalLengthEntered),
              Max_Length=max(TotalLengthEntered),
              SD_Length=round(sd(TotalLengthEntered),2),
              .groups = "drop")
  
  
  #Weighted mean (FD protocol; chap 15 by Schneider)
  #NOTE- not complete! don't think it's calculating correctly
  
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
    catchInchSurvey<-FISH_Data%>%
      left_join(catchInch,by="ModuleId")
    
    #summarize by survey and species
    lengthFreq <- catchInchSurvey %>%
      filter(!is.na(Species))%>% #filter out blank rows (due to efforts with no catches)
      group_by(SurveyId,Species,InchGroup) %>%
      summarize(N.caught=sum(NumberCaughtUnmarked))#only use unmarked here to avoid double counting fish
      
    alk_prop<-scaleEnvelope%>%
      mutate(InchGroup=floor(TotalLengthEntered))%>%
      group_by(SurveyId,Species,InchGroup,Age)%>%
      summarize(N.aged=n(),
                meanLength=mean(TotalLengthEntered))%>%
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
    warning("No catch-at-length data available; returning simple means only.")
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