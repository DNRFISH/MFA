#' FISH_query
#'
#'@description
#' Reads in data from FISHub dynamically to reduce memory burden. Can specify whether to query survey or efforts.
#' Filters allow you to filter by waterbody, surveyID, survey purpose, year, or gear type
#'
#' @param con A DBI connection to the FISHub database. The connection must
#'   be active when function is called.
#'
#' @param QueryType Character string specifying the type of data to return.
#'   Must be one of `"Survey"`, `"Efforts"`, or `"Catch"`. Defaults to
#'   `"Survey"`.
#'
#' @param MDNRID Character or numeric vector specifying one or more MDNRIDs
#'   to include in the query. If `NULL`, data for all waterbodies are
#'   included, subject to the other filters.
#'
#' @param WaterBodyName Character vector specifying one or more waterbody
#'   names to include in the query. Names must match the corresponding
#'   `WaterBodyName` values in FISHub. If `NULL`, all waterbodies are
#'   included, subject to the other filters.
#'
#' @param SurveyId Numeric vector specifying one or more Survey IDs to
#'   include in the query. If `NULL`, all surveys matching the other
#'   filters are included.
#'
#' @param SurveyPurpose Character vector specifying one or more survey
#'   purposes to include in the query. Values must match the survey purpose
#'   descriptions in FISHub. Must be one of
#'   `"Creel Census"`, `"Discretionary Survey"`, `"Fish Community"`,
#'   `"General Survey"`, `"Limnology"`, `"Management Evaluation"`,
#'   `"Population Estimate"`, `"Population Reduction"`,
#'   `"Recruitment Evaluation"`, `"Recruitment v Evaluation"`,
#'   `"Research Project"`, `"Special Study"`, `"Species Evaluation"`,
#'   `"Status & Trends"`, and `"Stocking Evaluation"`. If `NULL`, surveys
#'   of all purposes are included.
#'
#' @param Year Numeric vector specifying one or more years to include in
#'   the query. The year is determined from `SurveyBeginTimestamp`. If
#'   `NULL`, surveys from all years are included, subject to the other
#'   filters.
#'
#' @param GearType Character vector specifying one or more gear types to
#'   include in the query. If `NULL`, efforts using all gear types are
#'   included. When specified, only surveys and efforts associated with the
#'   selected gear types are returned. Must be one of
#'   `"EXPGNET"`, `"TRAPNET"`, `"BOOMSHK"`, `"LMFYKE"`, `"BACKSHK"`,
#'   `"OTHER"`, `"GLGNET"`, `"SMFYKE"`, `"SEINE"`, `"ANGLING"`,
#'   `"LIMNO"`, `"TRAWL"`, `"STRMSHK"`, `"Envelop"`, `"PhotoPt"`,
#'   `"PLNKNET"`, `"VGNET"`, `"TOXSAM"`, `"MNWTRAP"`, `"SRGNET"`,
#'   and `"SETHKS"`. `NA` is also permitted.
#'
#' @param Species Character vector specifying one or more species names to
#'   include in the query. Species names must match the corresponding
#'   species values in FISHub. If `NULL`, records for all species are
#'   included. When specified, the query returns only surveys and efforts
#'   associated with catch records for the selected species; efforts with
#'   no recorded catch of the selected species will therefore be excluded.
#' 
#' @details
#' `FISH_query()` dynamically queries FISHub using the supplied filters and
#' collects only the resulting records into R. This approach is intended to
#' reduce memory use compared with retrieving complete database tables.
#'
#' The query combines information from the `WaterBody`, `Survey`,
#' `SurveyPurpose`, `SurveyStatus`, `SurveyEffort`, `SurveyEffortDetail`,
#' and `Gear` tables. Catch information is subsequently added using
#' [catchByEffort()].
#'
#' All filter arguments are optional. When multiple filter values are
#' supplied, records matching any of the specified values are retained
#' within each filter, while different filters are applied simultaneously.
#' For example, specifying multiple `SurveyId` values returns data for any
#' of those surveys, while also applying any specified `Year` or `GearType`
#' filters.
#'
#' The `QueryType` argument determines the level of data returned. `"Survey"`
#' returns one row per unique survey, `"Efforts"` returns survey effort-level
#' records, and `"Catch"` returns the combined survey, effort, and catch data.
#'
#' When `GearType` is specified, only surveys and efforts associated with the
#' specified gear types are retained. When `Species` is specified, only
#' surveys and efforts with recorded catch of the specified species are
#' retained. Consequently, specifying `Species` excludes efforts in which
#' the selected species was not caught or was not recorded. This should be
#' considered when using the returned data to calculate catch rates or CPUE.
#'
#' If no records match the specified filters, an empty data frame is returned
#' and a message is printed indicating that no data were found.
#' 
#' @return
#' A data frame containing the requested FISHub data. The structure of the
#' returned data depends on `QueryType`:
#'
#' \itemize{
#'   \item `"Survey"`: One row per unique survey, including waterbody,
#'     survey purpose and status, survey dates, and survey-level attributes.
#'   \item `"Efforts"`: Survey- and effort-level records, including survey
#'     information, gear type, effort dates, number of gears used, and
#'     effort quantities and measurements.
#'   \item `"Catch"`: Combined survey-, effort-, and catch-level data,
#'     including species and catch information.
#' }
#'
#' The returned data frame contains only records matching the supplied
#' filter arguments. If no records match the filters, an empty data frame
#' is returned.
#' 
#' @examples
#' \dontrun{
#' #connect to database
#' con <- FISHub_connect()
#' 
#' #single survey; note different query types
#' FISH_Data <- FISH_query(con,QueryType = "Survey",SurveyId = 805)
#' FISH_Data <- FISH_query(con,QueryType = "Efforts",SurveyId = 805)
#' FISH_Data <- FISH_query(con,QueryType = "Catch",SurveyId = 805)
#' 
#' #all surveys from a waterbody
#' FISH_Data <- FISH_query(con,QueryType = "Survey",WaterBodyName = "Lake Orion")
#' 
#' #all SnT surveys from 2025
#' FISH_Data <- FISH_query(con,QueryType = "Survey",SurveyPurpose = "Status & Trends",Year=2025)
#' 
#' #all surveys that caught bowfin in 2025
#' FISH_Data <- FISH_query(con,QueryType = "Survey",Species="Bowfin",Year=2025)
#' 
#' #disconnect from database
#' DBI::dbDisconnect(con)
#' }
#' 
#' @importFrom dplyr tbl filter select distinct left_join rename inner_join full_join collect '%>%' semi_join
#' @importFrom lubridate year
#' @importFrom rlang .data .env
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
  
  
  #check to confirm query type is valid
  if (!QueryType %in% c("Survey", "Efforts", "Catch")) {
    stop("Invalid QueryType. Must be one of: 'Survey', 'Efforts', or 'Catch'.")
  }
  
  
  # --- Begin query ---
  #WaterBody
  WaterBody<-tbl(con, "WaterBody") %>%
    filter(IsActive==TRUE)%>% #filter out inactive rows to avoid duplicates Issue #1
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
    mutate(Year=lubridate::year(SurveyBeginTimestamp))%>%
    select(SurveyId,MDNRID,SurveyPurpose,SurveyStatus,Year,SurveyBeginTimestamp,FixedOrRandom,SurveyPurposeDescription)
    
  if(!is.null(SurveyId)){
      Survey<-Survey%>%filter(.data$SurveyId %in% .env$SurveyId)
    }
  if(!is.null(SurveyPurpose)){
    if(!SurveyPurpose%in%c("Creel Census", "Discretionary Survey", "Fish Community", "General Survey",
                           "Limnology", "Management Evaluation", "Population Estimate",
                           "Population Reduction", "Recruitment Evaluation", "Recruitment v Evaluation",
                           "Research Project", "Special Study", "Species Evaluation", "Status & Trends",
                           "Stocking Evaluation")){
      stop("Invalid SurveyPurpose. Must be one of: Creel Census, Discretionary Survey, Fish Community, General Survey,Limnology,
      Management Evaluation, Population Estimate,Population Reduction, Recruitment Evaluation, Recruitment v Evaluation,
      Research Project, Special Study, Species Evaluation, Status & Trends,Stocking Evaluation")
    }
    Survey<-Survey%>%filter(.data$SurveyPurpose %in% .env$SurveyPurpose)
  }
  if(!is.null(Year)){
    Survey<-Survey%>%filter(.data$Year %in% .env$Year)
  }

  

  #SurveyEffort, SurveyEffortDetail and, Gear (issue #9)
  Effort<-tbl(con, "SurveyEffort") %>%
    select(SurveyId,SurveyEffortId,SurveyEffortKey,ModuleId)%>%
    left_join(tbl(con, "SurveyEffortDetail"),by="SurveyEffortId")%>%
    left_join(tbl(con, "Gear") %>%select(GearId, GearType),by="GearId")%>%
    mutate(GearType<-trimws(GearType))%>%
    select(SurveyId,SurveyEffortId,SurveyEffortKey,ModuleId,BeginningEffortTimestamp, EndingEffortTimestamp, GearType,EffortNumberofGearUsed, EffortTotalQuantity, 
              EffortTotalMeasurement, EffortAlternateQuantity, EffortAlternateMeasurement)

  if(!is.null(GearType)){
    if(!GearType%in%c("ANGLING", "BACKSHK", "BOOMSHK", "Envelop", "EXPGNET", "GLGNET",
                      "LMFYKE", "LIMNO", "MNWTRAP", "OTHER", "PhotoPt", "PLNKNET",
                      "SEINE", "SETHKS", "SMFYKE", "SRGNET", "STRMSHK", "TOXSAM",
                      "TRAPNET", "TRAWL", "VGNET")){
      stop("GearType must be one of ANGLING,BACKSHK,BOOMSHK,Envelop,EXPGNET,GLGNET,LMFYKE,LIMNO,MNWTRAP,OTHER,PhotoPt,PLNKNET,SEINE,SETHKS,SMFYKE,SRGNET,STRMSHK,TOXSAM,TRAPNET,TRAWL,VGNET")
    }

    message("Note: query only returns surveys/efforts that had the specified gears.")
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
  catchDat<-catchByEffort(con,surveyEffortDat)
  
  #join to surveyEffort data
  allDat<-surveyEffortDat%>%
    full_join(catchDat,by=c("SurveyId","ModuleId"))
  
  if(!is.null(Species)){
    allDat<-allDat%>%filter(.data$Species %in% .env$Species)
  }

  
  #specify output based on query
  if (QueryType=="Survey") {
    outDat<-allDat%>%select(colnames(surveyDat))%>%unique()
    
    #look for potential issues (see issue #17)
    flagged <- outDat %>%
      mutate(SurveyId=as.character(SurveyId))%>%
      dplyr::semi_join(
      flagged_surveys_species_2026.09.02,
      by = c("SurveyId")
    )
    
    if (nrow(flagged) > 0) {
      warning(
        "DON'T USE THESE DATA UNTIL VERIFIED!!! \nThe following SurveyIDs were identified with potential errors: ",
        paste(
          paste(flagged$SurveyId),
          collapse = ", "
        ),
        "\nSee data(flagged_surveys_species_2026.09.02) for reference. Contact MFA team for more information."
      )
    }
  }
  if (QueryType=="Efforts") {
    outDat<-allDat%>%select(colnames(surveyEffortDat))%>%unique()
    
    #look for potential issues (see issue #17)
    flagged <- outDat %>%
      mutate(SurveyId=as.character(SurveyId))%>%
      dplyr::semi_join(
        flagged_surveys_species_2026.09.02,
        by = c("SurveyId","SurveyEffortKey")
      )
    
    if (nrow(flagged) > 0) {
      warning(
        "DON'T USE THESE DATA UNTIL VERIFIED!!! \nThe following SurveyID-SurveyEffortKey combinations were identified with potential errors: ",
        paste(
          paste(flagged$SurveyId, flagged$SurveyEffortKey, sep = "-"),
          collapse = ", "
        ),
        "\nSee data(flagged_surveys_species_2026.09.02) for reference. Contact MFA team for more information."
      )
    }
  }
  if (QueryType=="Catch") {
    outDat<-allDat
    if(!is.null(Species)){
      message("Note: query only returns surveys/efforts that caught the specified species. It is missing efforts with no capture. Be cautious when calculating CPUE or use CPUE function .")
    }
    
    #look for potential issues (see issue #17)
    flagged <- outDat %>%
      mutate(SurveyId=as.character(SurveyId))%>%
      dplyr::semi_join(
        flagged_surveys_species_2026.09.02,
        by = c("SurveyId","SurveyEffortKey") #didn't both with species since the codes are slightly different
      )%>%
      select(SurveyId,SurveyEffortKey)%>%
      unique()
    
    if (nrow(flagged) > 0) {
      warning(
        "DON'T USE THESE DATA UNTIL VERIFIED!!! \nThe following SurveyID-SurveyEffortKey combinations were identified with potential errors: ",
        paste(
          paste(flagged$SurveyId, flagged$SurveyEffortKey, sep = "-"),
          collapse = ", "
        ),
        "\nSee data(flagged_surveys_species_2026.09.02) for reference. Contact MFA team for more information."
      )
    }
  }
  
  #print if no data found
  if(nrow(outDat)==0){
    message("No data found. Confirm spelling and data are in FISHub. Contact MFA team for assistance if you believe there are missing data.")
  }
  return(outDat)
}

