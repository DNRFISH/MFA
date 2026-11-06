#' Run the MFA Shiny application
#'
#' Launches the MFA Shiny application in a local web browser.
#' 
#' @param ... Additional arguments passed to [shiny::runApp()].
#' @import DT
#' @return Runs the Shiny application.
#' @export

run_MFA_app <- function(...) {
  
  # Check for Shiny dependency
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop(
      "The shiny package is required to run the MFA application. ",
      "Install it with install.packages('shiny').",
      call. = FALSE
    )
  }
  
  # Locate the installed application
  app_dir <- system.file("shiny", package = "MFA")
  
  if (app_dir == "") {
    stop(
      "Could not locate the MFA Shiny application.",
      call. = FALSE
    )
  }
  
  # Launch application
  shiny::runApp(app_dir, ...)
}