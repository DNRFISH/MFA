#' catch_summary_table
#'
#' Creates summary table of catches by species per survey

#'
#' @param catchData Data query from catchByEffort function
#' @return Summary Table
#' @export
#'

#NOTE: avg length doesn't currently line up with SFRs because of data issues identifed during FISH development

catch_summary_table <- function(catchData) {
  
  #do a check to see if multiple efforts are included in the table
  if (length(unique(catchData$ModuleId)) > 1) {
    message(paste0("Warning: ",
                   length(unique(catchData$ModuleId)),
                   " ModuleIds included in catch summary data"))
  }
  
  outTab<-catchData%>%
    group_by(SurveyId,Species)%>%
    summarise(
      TotalNumberCaught=sum(TotalNumberCaught,na.rm = T),
      LengthMinimum = min(LengthMinimum, na.rm = TRUE),
      LengthMaximum = max(LengthMaximum, na.rm = TRUE),
      N_legal = sum(N_legal, na.rm = TRUE),
      PctLegal = round(100 * N_legal /TotalNumberCaught,0),
      .groups = "drop"
    )
  
  return(outTab)
}


