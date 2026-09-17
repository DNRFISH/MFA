#' look up table for S&T surveys with extra efforts
#' Identifed by K. Wehrly and provided by K. King on 9/10/2026 via email
#' 
#' Used to flag warning if using survey and filters out when querying S&T data
#' 
#' See issue #19
#'
#'As of 9/11: "Also – I [KK] think the list is pretty comprehensive. Kevin did 
#'quite a bit of digging and Kevin’s list already included all but 1 of John’s list. 
#'From what I gather, it is just the extra shocking efforts that are problematic 
#'and I don’t think that all of the FMU’s are doing it. For example, while NLM 
#'has a bunch, John compiled a list for NLH and they only had 2. 
#'
#' @format A data frame with SurveyNumber,SurveyEffortKey,and Notes; joined SurveyId from Survey table
#'
#'
#'Added SurveyID
#'Status_and_Trends_extra_efforts <- readr::read_csv("data-raw/Status_and_Trends_extra_efforts.csv")
#'con<-FISHub_connect()
#'Survey<-tbl(con, "Survey")%>%select(SurveyNumber,SurveyId)%>%collect()
#'Status_and_Trends_extra_efforts<-Status_and_Trends_extra_efforts%>%
#'left_join(Survey,by="SurveyNumber")
#'
"Status_and_Trends_extra_efforts"