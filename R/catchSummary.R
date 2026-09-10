#' catchSummary
#'
#' Summarizes catch data returned by [FISH_query()] to produce one row
#' for each survey and species combination. The summary includes total
#' catch, weighted average length, minimum and maximum length, and the
#' number and percentage of fish meeting the statewide or other specified
#' legal minimum size limit.
#'
#' @param catchData A data frame returned by [FISH_query()] with
#'   `QueryType = "Catch"`. The data must contain `SurveyId`, `Species`,
#'   `GearType`, `TotalNumberCaught`, `LengthAverage`, `LengthMinimum`,
#'   `LengthMaximum`, and `N_legal`.
#'   
#' @return A data frame with one row per survey and species combination.
#'   Columns include total catch, weighted average length, minimum and
#'   maximum length, number of legal fish, and percentage of fish that
#'   meet  statewide or other specified legal minimum size limit.
#'
#' @examples
#' catchData<-FISH_query(con,QueryType = "Catch",SurveyId = 805)
#' catchSum <- catchSummary(catchData)
#' 
#' @importFrom dplyr filter mutate group_by summarise
#' 
#' @export



#NOTE: avg length doesn't currently line up with SFRs because of data issues identifed during FISH development

catchSummary <- function(catchData) {
  
  if(!"TotalNumberCaught" %in% names(catchData)){
    stop("No catch data included. Need to provide catch data from FISH_query")
  }
  
  #do a check to see if multiple efforts are included in the table
  if (length(unique(catchData$GearType)) > 1) {
    message(paste0("Warning: ",
                   length(unique(catchData$GearType)),
                   " different gears included in catch summary data"))
  }
  
  outTab<-catchData%>%
    filter(!is.na(TotalNumberCaught))%>% #remove efforts with no catches
    mutate(w = TotalNumberCaught) %>%
    group_by(SurveyId, Species) %>%
    summarise(
      TotalNumberCaught = sum(w, na.rm = TRUE),
      LengthAverage = round(sum(LengthAverage * w, na.rm = TRUE) / sum(w, na.rm = TRUE),2),
      LengthMinimum = min(LengthMinimum, na.rm = TRUE),
      LengthMaximum = max(LengthMaximum, na.rm = TRUE),
      N_legal = sum(N_legal, na.rm = TRUE),
      PctLegal = round(100 * N_legal / TotalNumberCaught, 0),
      .groups = "drop"
    )
  
  return(outTab)
}


