#' CPUE_byGear
#'
#' Creates summary table of CPUE by gear and species

#'
#' @param CatchEffortData "Catch" data query from FISH_query function
#' @return Summary Table or Figure
#' @export
#'
#' @examples
#' #Single Survey
#' CatchEffortData <- FISH_query(con,QueryType = "Catch",SurveyId = 1162)
#' CPUE<-CPUE_byGear(CatchEffortData)
#' 
#' #Multiple Surveys
#' CatchEffortData <- FISH_query(con,QueryType = "Catch",SurveyPurpose = "Status & Trends",Year=2025)
#' CPUE<-CPUE_byGear(CatchEffortData)
#'


CPUE_byGear<-function(CatchEffortData,OutputType="Table"){
  
  #get efforts with NAs in effort quantity column for removal (e.g., limno or missing data)
  effortsNA<-CatchEffortData%>%
    filter(is.na(EffortTotalQuantity)&is.na(EffortAlternateQuantity))%>%
    filter(trimws(GearType)!="LIMNO")

  #print warning
  if(nrow(effortsNA)>0){
    message(paste0("WARNING:",nrow(effortsNA)," effort(s) removed from summary due to missing data. Look at raw output to determine missing data."))
  }

  #clean up data
  CatchEffortDataClean<-CatchEffortData%>%
    #remove missing efforts (a warning will spit out above)
    filter(!is.na(EffortTotalQuantity) | !is.na(EffortAlternateQuantity))%>%
    #clean up effort units and values
    mutate(
      #use the EffortTotalQuantity and EffortTotalMeasurement as primary, if exists; otherwise pull from alternate
      use_total = !is.na(EffortTotalQuantity) & EffortTotalQuantity != 0,
      Effort = if_else(
        use_total,
        EffortTotalQuantity,
        EffortAlternateQuantity
      ),
      
      Unit = case_when(
        use_total ~ EffortTotalMeasurement,
        TRUE      ~ EffortAlternateMeasurement
      ),
      
      #use alterante unit and value, if not used for primary
      Effort_alt = case_when(
        use_total ~ EffortAlternateQuantity,
        TRUE      ~ EffortTotalQuantity
      ),
      
      Unit_alt = case_when(
        use_total ~ EffortAlternateMeasurement,
        TRUE      ~ EffortTotalMeasurement
      )
    )%>%
    select(-c(EffortTotalQuantity,EffortTotalMeasurement,EffortAlternateQuantity,EffortAlternateMeasurement,use_total))
    
  #summarize catch data
  catchByGear<-CatchEffortDataClean%>%
    filter(!is.na(Species))%>%
    group_by(SurveyId,GearType,Species)%>%
    summarise(TotalNumberCaught=sum(TotalNumberCaught,na.rm = T),.groups = "drop")
  
  #summarize effort data
  effortByGear<-CatchEffortDataClean%>%
    select(WaterBodyName,SurveyId,SurveyEffortId,SurveyEffortKey,GearType,EffortNumberofGearUsed,Effort,Unit,Effort_alt,Unit_alt)%>%
    unique()%>%
    group_by(WaterBodyName,SurveyId,GearType,Unit,Unit_alt)%>%
    summarize(TotalEffort=sum(as.numeric(Effort)),
              TotalEffort_alt=sum(as.numeric(Effort_alt)),
              .groups = "drop")
  
  
  #merge tables and calculate CPUE
  allDat<-effortByGear%>%
    inner_join(catchByGear,by=c("SurveyId","GearType"))%>%
    mutate(
      TotalEffort = if_else(
        Unit == "Seconds",
        TotalEffort / 60,
        TotalEffort
      ),
      TotalEffort_alt = if_else(
        Unit_alt == "Seconds",
        TotalEffort_alt / 60,
        TotalEffort_alt
      ),
      Unit = if_else(
        Unit == "Seconds", "Minutes", Unit
      ),
      Unit_alt = if_else(
        Unit_alt == "Seconds", "Minutes", Unit_alt
      ),
      CPUE=TotalNumberCaught/TotalEffort,
      CPUE_alt=TotalNumberCaught/TotalEffort_alt)%>%
    select(WaterBodyName,SurveyId,GearType,Species,TotalNumberCaught,
           TotalEffort,Unit,CPUE,
           TotalEffort_alt,Unit_alt,CPUE_alt)
  
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