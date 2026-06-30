#' CPUE_byGear
#'
#' Creates summary table of CPUE by gear and species

#'
#' @param CatchEffortData "Catch" data query from FISH_query function
#' @return Summary Table or Figure
#' @export
#'
#'


CPUE_byGear<-function(CatchEffortData,OutputType=NULL){
  
  #get efforts with NAs in effort quantity column for removal (e.g., limno or missing data)
  effortsNA<-CatchEffortData%>%
    filter(is.na(EffortTotalQuantity)&is.na(EffortAlternateQuantity))%>%
    filter(trimws(GearType)!="LIMNO")

  #print warning
  if(nrow(effortsNA)>0){
    message(paste0("WARNING:",nrow(effortsNA)," effort(s) removed from summary due to missing data. Look at raw output to determine missing data."))
  }

  #summarize catch data
  catchByGear<-CatchEffortData%>%
    filter(!is.na(EffortTotalQuantity) | !is.na(EffortAlternateQuantity))%>%
    filter(!is.na(Species))%>%
    group_by(SurveyId,GearType,Species)%>%
    summarise(TotalNumberCaught=sum(TotalNumberCaught,na.rm = T),.groups = "drop")
  
  #summarize effort data
  effortByGear<-CatchEffortData%>%
    filter(!is.na(EffortTotalQuantity) | !is.na(EffortAlternateQuantity))%>%
    select(WaterBodyName,SurveyId,SurveyEffortId,SurveyEffortKey,GearType,EffortNumberofGearUsed,EffortTotalQuantity,EffortTotalMeasurement,EffortAlternateQuantity,EffortAlternateMeasurement)%>%
    unique()%>%
    group_by(WaterBodyName,SurveyId,GearType,EffortTotalMeasurement,EffortAlternateMeasurement)%>%
    summarize(EffortTotalQuantity=sum(as.numeric(EffortTotalQuantity)),
              EffortAlternateQuantity=sum(as.numeric(EffortAlternateQuantity)),
              .groups = "drop")%>%
    as.data.frame()
  
  
  #merge tables and calculate CPUE
  allDat<-effortByGear%>%
    inner_join(catchByGear,by=c("SurveyId","GearType"))%>% #this join causing issues with many to many
    mutate(
      EffortTotalQuantity = if_else(
        EffortTotalMeasurement == "Seconds",
        EffortTotalQuantity / 60,
        EffortTotalQuantity
      ),
      EffortAlternateQuantity = if_else(
        EffortAlternateMeasurement == "Seconds",
        EffortAlternateQuantity / 60,
        EffortAlternateQuantity
      ),
      EffortTotalMeasurement = if_else(
        EffortTotalMeasurement == "Seconds", "Minutes", EffortTotalMeasurement
      ),
      EffortAlternateMeasurement = if_else(
        EffortAlternateMeasurement == "Seconds", "Minutes", EffortAlternateMeasurement
      ),
      CPUE=TotalNumberCaught/EffortTotalQuantity,
      CPUE_alt=TotalNumberCaught/EffortAlternateQuantity)%>%
    select(WaterBodyName,SurveyId,GearType,Species,TotalNumberCaught,
           EffortTotalQuantity,EffortTotalMeasurement,CPUE,
           EffortAlternateQuantity,EffortAlternateMeasurement,CPUE_alt)
  
  if(OutputType=="Table"){
    return(allDat)
  }
  
  if(OutputType=="Figure"){
    plotOut<-ggplot(allDat)+
      geom_bar(aes(x=Species,y=CPUE,fill=as.factor(SurveyId)),stat="identity",position = "dodge")+
      facet_wrap(~GearType,scales = "free")+
      theme_classic()+
      scale_fill_viridis_d()+
      xlab("Species")+ylab("CPUE (net lift or seconds E-fishing)")+labs(fill="Survey")+
      theme(
        axis.text.x = element_text(angle = 90,vjust = 0.5, hjust = 1)
      )
    return(plotOut)
    
  }
}