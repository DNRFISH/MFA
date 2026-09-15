#' look up table for surveys with potential issues in catch data
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
"flagged_surveys_species_2026.09.02"