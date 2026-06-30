#' FISH_query
#'
#'@description
#' Reads in data from FISHub dynamically to reduce memory burden. Can specify whether to query survey or efforts.
#' Filters allow you to filter by waterbody, surveyID, survey purpose, year, or gear type
#'
#' @param con Connection to FISHub
#' @param QueryType Specify "Survey" or "Efforts" (Default is Survey)
#' @param MDNRID MDNRID to be used in query
#' @param WaterBodyName WaterBodyName to be used in query
#' @param SurveyId SurveyId to be used in query
#' @param SurveyPurpose SurveyPurpose to be used in query. Options include: "Creel Census","Discretionary Survey","Fish Community",
#' "General Survey","Limnology","Management Evaluation","Population Estimate","Population Reduction","Recruitment Evaluation",
#' "Recruitment v Evaluation","Research Project","Special Study","Species Evaluation","Status & Trends","Stocking Evaluation".
#' @param Year Year to be used in query
#' @param GearType GearType to be used in query
#' @param Species Species name to be used in query
#' 
#' @details
#' Tables Used: WaterBody, Survey, SurveyPurpose, SurveyStatus, SurveyEffort, SurveyEffortDetails, and Gear
#' 
#' @return A merged data frame with data.
#' 
#' @export
#'

FISH_query <- function(con,
                       QueryType="Survey",
                       MDNRID = NULL,
                       WaterBodyName = NULL,
                       SurveyId=NULL,
                       SurveyPurpose=NULL,
                       Year=NULL,
                       GearType=NULL,
                       Species=NULL) {
  
  
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
  
  #Survey, SurveyPurpose, and SurveyStatus ID
  Survey<-tbl(con, "Survey")%>%
    left_join(tbl(con, "SurveyPurpose")%>%select(SurveyPurposeId,Descriptions),by = "SurveyPurposeId")%>%
    rename(SurveyPurpose=Descriptions)%>%
    left_join(tbl(con, "SurveyStatus")%>%select(SurveyStatusId,Descriptions),by = "SurveyStatusId")%>%
    rename(SurveyStatus=Descriptions)%>%
    select(SurveyId,MDNRID,SurveyPurpose,SurveyStatus,SurveyBeginTimestamp,FixedOrRandom,SurveyPurposeDescription)
    
  if(!is.null(SurveyId)){
      Survey<-Survey%>%filter(.data$SurveyId %in% .env$SurveyId)
    }
  if(!is.null(SurveyPurpose)){
    Survey<-Survey%>%filter(.data$SurveyPurpose %in% .env$SurveyPurpose)
  }
  if(!is.null(Year)){
    Survey<-Survey%>%filter(lubridate::year(.data$SurveyBeginTimestamp) %in% .env$Year)
  }

  

  #SurveyEffort, SurveyEffortDetail and, Gear (issue #9)
  Effort<-tbl(con, "SurveyEffort") %>%
    select(SurveyId,SurveyEffortId,SurveyEffortKey,ModuleId)%>%
    left_join(tbl(con, "SurveyEffortDetail"),by="SurveyEffortId")%>%
    left_join(tbl(con, "Gear") %>%select(GearId, GearType),by="GearId")%>%
    select(SurveyId,SurveyEffortId,SurveyEffortKey,ModuleId, BeginningEffortTimestamp, EndingEffortTimestamp, GearType,EffortNumberofGearUsed, EffortTotalQuantity, 
              EffortTotalMeasurement, EffortAlternateQuantity, EffortAlternateMeasurement)

  if(!is.null(GearType)){
    print("Note: query only retruns surveys/efforts that had the specified gears.")
    Effort<-Effort%>%filter(.data$GearType %in% .env$GearType)
  }
  
  #do joins
  surveyDat<-WaterBody%>%
    inner_join(Survey,by="MDNRID")
  
  surveyEffortDat<-surveyDat%>%
    left_join(Effort,by="SurveyId")

  #collect surveyEffort data for use in catch query -- could modify this to stay SQL; would require reworking catchByEffort function
  surveyEffortDat<-collect(surveyEffortDat)

  #read in catch data
  catchDat<-catchByEffort(surveyEffortDat)
  
  #join to surveyEffort data
  allDat<-surveyEffortDat%>%
    full_join(catchDat,by=c("SurveyId","ModuleId"))
  
  if(!is.null(Species)){
    allDat<-allDat%>%filter(.data$Species %in% .env$Species)
  }

  
  #specify output based on query
  if (QueryType=="Survey") {
    outDat<-allDat%>%select(colnames(surveyDat))%>%unique()
  }
  if (QueryType=="Efforts") {
    outDat<-allDat%>%select(colnames(surveyEffortDat))%>%unique()
  }
  if (QueryType=="Catch") {
    outDat<-allDat
    if(!is.null(Species)){
      print("Note: query only retruns surveys/efforts that caught the specified species. It is missing efforts with no capture. Be cautious when calculating CPUE or use CPUE function .")
    }
  }
  
  #print if no data found
  if(nrow(outDat)==0){
    print("No data found. Confirm spelling and data is in FISHub. Contact MFA team for assistance.")
  }
  return(outDat)
}

