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

  

  #SurveyEffort (issue #9)
  SurveyEffort<-tbl(con, "SurveyEffort") %>%
    select(SurveyId,SurveyEffortId,SurveyEffortKey,ModuleId)
  
  #SurveyEffortDetail and Gear (issue #9)
  SurveyEffortDetail <- tbl(con, "SurveyEffortDetail") %>%
    left_join(tbl(con, "Gear") %>%select(GearId, GearType),by="GearId")%>%
    select(SurveyEffortId, BeginningEffortTimestamp, EndingEffortTimestamp, GearType,EffortNumberofGearUsed, EffortTotalQuantity, 
              EffortTotalMeasurement, EffortAlternateQuantity, EffortAlternateMeasurement)

  #do joins based on type of query
  joinDat<-WaterBody%>%
    inner_join(Survey,by="MDNRID")
  if (QueryType%in%c("Efforts","Catch")) {
    joinDat<-joinDat%>%
      left_join(SurveyEffort,by="SurveyId")%>%
      left_join(SurveyEffortDetail,by="SurveyEffortId")
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

