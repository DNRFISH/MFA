#### Test file for reading in FISH tables dynamically to reduce memory burden ####
library(dplyr)
library(odbc)
library(DBI)
library(stringr)
library(lubridate)

#Notes
#10/31/2025: started developing dynamic read function; everything is working; just need to add in additional data; 
#should also figure out where/when to filter out NAs associated with the catch data- there are a lot because of the moduledataId

FISH_query <- function(con,
                       QueryType="Survey", #or Efforts, Catch
                       MDNRID = NULL,
                       WaterBodyName = NULL,
                       SurveyId=NULL,
                       SurveyPurposeId=NULL,
                       Year=NULL,
                       GearType=NULL) {
  
  
  # --- Begin query ---
  #WaterBody
  WaterBody<-tbl(con, "WaterBody") %>%
    filter(IsActive==T)%>% #filter out inactive rows to avoid duplicates Issue #1
    select(MDNRID,WaterBodyName)%>%
    distinct(MDNRID, WaterBodyName, .keep_all = TRUE) #*should we carry over old waterbody ID for unlocated ones?
  if(!is.null(MDNRID)){
    WaterBody<-WaterBody%>%filter(.data$MDNRID %in% .env$MDNRID)
  }
  if(!is.null(WaterBodyName)){
    WaterBody<-WaterBody%>%filter(.data$WaterBodyName %in% .env$WaterBodyName)
  }
  
  #Survey
  Survey<-tbl(con, "Survey")%>%
    select(SurveyId,MDNRID,SurveyBeginTimestamp,SurveyPurposeId,SurveyStatusId,FixedOrRandom,SurveyPurposeDescription)
  if(!is.null(SurveyId)){
      Survey<-Survey%>%filter(.data$SurveyId %in% .env$SurveyId)
    }
  if(!is.null(SurveyPurposeId)){
    Survey<-Survey%>%filter(.data$SurveyPurposeId %in% .env$SurveyPurposeId)
  }
  if(!is.null(Year)){
    Survey<-Survey%>%filter(lubridate::year(.data$SurveyBeginTimestamp) %in% .env$Year)
  }
  
  #SurveyEffortGearSet
  SurveyEffortGearSet<-tbl(con, "SurveyEffortGearSet") %>%
      select(SurveyId,SurveyEffortId,GearType,GearCommonName)
    if(!is.null(GearType)){
      SurveyEffortGearSet<-SurveyEffortGearSet%>%filter(.data$GearType %in% .env$GearType)
    }
  
    
  #SurveyEffort
  SurveyEffort<-tbl(con, "SurveyEffort") %>%
    select(SurveyId,SurveyEffortId,SurveyEffortKey,GearTypeId,ModuleId,
           EffortNumberofGearUsed,EffortMeasurement,EffortQuantity,EffortMeasurement2,EffortQuantity2)
  
  #do joins based on type of query
  joinDat<-WaterBody%>%
    inner_join(Survey,by="MDNRID")
  if (QueryType%in%c("Efforts","Catch")) {
    joinDat<-joinDat%>%
      inner_join(SurveyEffortGearSet,by="SurveyId")%>%
      inner_join(SurveyEffort,by=c("SurveyId","SurveyEffortId"))
  }
  if (QueryType%in%c("Catch")) {
  print("still need to add catch data")
  }
  
  #collect and return in order
  outDat<-collect(joinDat)
  if(nrow(outDat)==0){
    print("No data found")
    if(QueryType!="Survey"){
      joinDat<-WaterBody%>%
        inner_join(Survey,by="MDNRID")
      outDat2<-collect(joinDat)
      if(nrow(outDat==0)){
        print("No Surveys Found")
      }
    }
  }
  return(outDat)
}

