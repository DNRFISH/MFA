#' CPUE_byGear
#'
#' Creates summary table of CPUE by gear and species

#'
#' @param FCS_Data FCS data query from FCS_Query function
#' @return Summary Table
#' @export
#'
#'


CPUE_byGear<-function(FISH_Data,OutputType=NULL){
  
  #leaving this in for now... circle back to how to handle this (issue )
  # #get efforts with NAs in effort quantity column for removal (e.g., limno or missing data)
  # effortsNA<-FISH_Data%>%
  #   select(SurveyId,SurveyEffortId,EffortMeasurement,EffortQuantity,EffortMeasurement2,EffortQuantity2,GearType)%>%
  #   unique()%>%
  #   filter(is.na(EffortQuantity))
  # 
  # #print warning
  # if(nrow(effortsNA)>0){
  #   message(paste0("WARNING:",nrow(effortsNA)," effort(s) removed from summary due to missing data. Look at raw output to determine missing data."))
  # }
  # 
  
  
  #DECIDED TO PIVOT... create a new function that reads in catch by effort that will then be used in catch summary table and CPUE; it will avoid duplicating code across functions
  
  #summarize catch data
  catchByGear<-FISH_Data%>%
    filter(!is.na(Species_Name),!survey_key_ID%in%effortsNA$survey_key_ID)%>%
    group_by(Survey_Number,NGEAR,Species_Name)%>%
    summarise(TotalCatch=sum(Total_Number_Caught,na.rm = T))%>%
    ungroup()%>%
    group_by(Survey_Number)%>%
    tidyr::complete(Species_Name,NGEAR,fill=list(TotalCatch=0))
  
  #summarize effort data
  effortByGear<-FISH_Data%>%
    filter(!survey_key_ID%in%effortsNA$survey_key_ID)%>%
    select(Water_Body_Key,Water_Body_Name,Survey_Number,Survey_Effort_Key,survey_key_ID,NGEAR,EffortUnit,EffortValue)%>%
    unique%>%
    group_by(Water_Body_Key,Water_Body_Name,Survey_Number,NGEAR,EffortUnit)%>%
    summarize(TotalEffort=sum(as.numeric(EffortValue)))%>%
    as.data.frame()
  
  
  #merge tables and calculate CPUE
  allDat<-merge(catchByGear,effortByGear,by=c("Survey_Number","NGEAR"))
  allDat$CPUE<-round(allDat$TotalCatch/allDat$TotalEffort,2)
  
  #bring in FMU and water body name
  allDat$FMU<-FISH_Data$FMU[match(allDat$Survey_Number,FISH_Data$Survey_Number)]
  allDat$Water_Body_Name<-FISH_Data$Water_Body_Name[match(allDat$Survey_Number,FISH_Data$Survey_Number)]
  allDat$Species_Code<-FISH_Data$Species_Code[match(allDat$Species_Name,FISH_Data$Species_Name)]
  
  
  if(OutputType=="Figure"){
    summaryOut<-ggplot(allDat)+
      geom_bar(aes(x=Species_Name,y=CPUE,fill=NGEAR),stat="identity",position = "dodge")+
      facet_wrap(~Survey_Number)+
      theme_classic()+
      scale_fill_viridis_d()+
      xlab("Species Code")+ylab("CPUE (net lift or minute E-fishing)")+labs(fill="Gear")
  }
  
  if(OutputType=="Table"){
    #reorganize columns
    summaryOut<-allDat%>%
      select(FMU,Water_Body_Key,Water_Body_Name,Survey_Number,Species_Name,NGEAR,EffortUnit,TotalEffort,TotalCatch,CPUE)
  }
  
  return(summaryOut)
}