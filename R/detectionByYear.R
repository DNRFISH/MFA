#' detectionByYear
#'
#' Summarize species detections by year
#'
#' Creates a summary of species detections by year from catch data returned by
#' \code{\link{FISH_query}}. Detection is summarized as a binary value for
#' each species and year, with a species considered detected when at least
#' one individual was caught during the year.
#'
#' The function excludes records without a \code{GearType}, which are assumed
#' to represent non-fishing or limnological sampling records. Survey year is
#' determined from \code{SurveyBeginTimestamp}. The resulting table is
#' reshaped so that each row represents a species and each column represents
#' a year.
#'
#' The function also appends the number of surveys and number of sampling
#' efforts conducted in each year. If data from more than one waterbody are
#' supplied, a warning is issued because the resulting detection history
#' combines detections across waterbodies.
#'
#' @param catchData A data frame containing catch data, typically returned by
#'   \code{\link{FISH_query}}. The data must contain \code{MDNRID},
#'   \code{GearType}, \code{SurveyBeginTimestamp}, \code{Species},
#'   \code{TotalNumberCaught}, \code{SurveyId}, and
#'   \code{SurveyEffortKey} columns.
#'
#' @return A data frame containing a species-by-year detection summary. Each
#'   row represents a species, and each year is represented by a separate
#'   column. The table also includes rows summarizing sampling effort:
#'   \describe{
#'     \item{Species}{Species name. The final rows contain the labels
#'       \code{"N_Surveys"} and \code{"N_Efforts"} rather than species names.}
#'     \item{Year columns}{Detection status for each species in the
#'       corresponding year. A value of \code{"x"} indicates that at least
#'       one individual of the species was caught during that year.
#'       \code{NA} indicates that no detection record was generated for that
#'       species-year combination.}
#'     \item{N_Surveys}{Number of unique surveys represented in the catch
#'       data for the corresponding year.}
#'     \item{N_Efforts}{Number of unique survey efforts represented in the
#'       catch data for the corresponding year. An effort is identified using
#'       the combination of \code{SurveyId} and \code{SurveyEffortKey}.}
#'   }
#'
#' @details
#' Detection is calculated separately for each species and year. A species is
#' considered detected when the summed \code{TotalNumberCaught} across all
#' included catch records for that species and year is greater than zero.
#' Detection is therefore based on catch records rather than on the presence
#' of a species record alone.
#'
#' Records with a missing \code{GearType} are excluded before calculating
#' detections and effort summaries. This is intended to exclude limnological
#' or other records that do not represent fishing effort.
#'
#' Survey year is extracted from \code{SurveyBeginTimestamp}. All surveys
#' occurring within the same calendar year are combined when determining
#' whether a species was detected.
#'
#' The function is primarily intended for data representing a single
#' waterbody. If more than one unique \code{MDNRID} is present, the function
#' issues a warning and combines detections from all waterbodies. In that
#' situation, an \code{"x"} indicates that the species was detected in at
#' least one of the included waterbodies during that year; it does not
#' indicate that the species was detected in every waterbody.
#'
#' Species-year combinations without a catch record are retained as
#' \code{NA} rather than being explicitly converted to zero or a blank.
#' Consequently, \code{NA} should be interpreted as no detection record in
#' the resulting summary, rather than necessarily as evidence that the
#' species was absent.
#'
#' The number of surveys is calculated as the number of unique
#' \code{SurveyId} values within each year. The number of efforts is
#' calculated as the number of unique combinations of \code{SurveyId} and
#' \code{SurveyEffortKey} within each year.
#'
#' @examples
#' \dontrun{
#' # Query catch data for a waterbody
#' catchData <- FISH_query(
#'   con,
#'   QueryType = "Catch",
#'   WaterBodyName = "Lake Orion"
#' )
#'
#' # Create a yearly detection summary
#' detection_summary <- detectionByYear(catchData)
#' }
#'
#'
#'@importFrom dplyr n_distinct filter mutate group_by summarize '%>%'
#'
#' @export


detectionByYear <- function(catchData) {
  
  #check for multiple waterbodies
  if(n_distinct(catchData$MDNRID)>1){
    message("WARNING: yearly detection summary includes multiple waterbodies")
  }
  
  #get detections by species and year
  detectSummary<-catchData%>%
    filter(!is.na(GearType))%>% #removes limno
    mutate(Year=lubridate::year(SurveyBeginTimestamp))%>%
    group_by(Year,Species)%>%
    summarize(Detect=as.integer(sum(TotalNumberCaught,na.rm = T)>0),.groups = "drop")%>%
    #mutate(Detect="x",Species=as.character(varhandle::unfactor(Species)))%>%
    mutate(Detect="x")%>%
    tidyr::spread(key=Year,value = Detect)%>%
    as.data.frame()%>%
    filter(!is.na(Species)) #surveys with no catch will show up as NA
  
  # for(i in 2:ncol(detectSummary)){
  #   detectSummary[which(is.na(detectSummary[,i])),i]<-""
  # }
  
  #get # surveys and # efforts by year
  effortSummary<-catchData%>%
    filter(!is.na(GearType))%>%
    mutate(Year=lubridate::year(SurveyBeginTimestamp))%>%
    group_by(Year)%>%
    summarize(N_Surveys=length(unique(SurveyId)),
              N_Efforts=length(unique(paste0(SurveyId,SurveyEffortKey))))%>%
    as.data.frame()%>%
    t()
  
  #add effort data to detection table
  detectSummary[(nrow(detectSummary)+1),]<-c(row.names(effortSummary)[2],effortSummary[2,])
  detectSummary[(nrow(detectSummary)+1),]<-c(row.names(effortSummary)[3],effortSummary[3,])
  
  
  
  return(detectSummary)
}


