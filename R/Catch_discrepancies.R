#' look up table for surveys with Catch_discrepancies
#' Provided by K. King on 9/2/2026 via email
#' RIT staff are working through fixes
#' 
#' See issue #17
#'
#'
#' @format A data frame with SurveyNumber,SurveyEffortKey,StrainCd,SurveyId,WaterBodyName,
#' AgencyUnitName,SiteName,SurveyBeginTimestamp
#'
#'Example
#'library(MFA)
#'con<-FISHub_connect()
#'test<-FISH_query(con,QueryType = "Catch",SurveyId = 15221,Species = "Carps And Minnows")
#'
#'Some old SurveyNumbers were split into multiple new SurveyIds because of how they aligned with waterbodies
#'KK original spreadsheet had all SurveyIds listed
#'This code was used to assign to a single SurveyId
#'
#'Catch_discrepancies <- readr::read_csv("data-raw/Catch_discrepancies.csv")
#'con<-FISHub_connect()
#'SurveyEfforts<-tbl(con, "Survey")%>%
#'  left_join(tbl(con, "SurveyEffort"),by="SurveyId")%>%
#'  select(SurveyNumber,SurveyId,SurveyEffortKey)%>%
#'  collect()
#'
#'SurveyEffortsSub<-SurveyEfforts%>%
#'  dplyr::semi_join(
#'  Catch_discrepancies%>%
#'    filter(grepl(";",SurveyId)),by = c("SurveyNumber","SurveyEffortKey"))%>%
#'  rename(SingleSurveyId=SurveyId)
#'
#'Catch_discrepancies<-Catch_discrepancies%>%
#'  left_join(SurveyEffortsSub,by=c("SurveyNumber","SurveyEffortKey")) %>%
#'  mutate(
#'    SurveyId = coalesce(SingleSurveyId, as.integer(SurveyId)) #use SingleSurveyId if available, otherwise SurveyId
#'  )
#'usethis::use_data(Catch_discrepancies, overwrite = TRUE)

#'
"Catch_discrepancies"
