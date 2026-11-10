# tests/testthat/test-app-live.R
# Live Shiny smoke test using FISHub Survey ID 805.
# Run from the MFA package project: devtools::test(filter = "app-live")

library(shinytest2)

 testthat::test_that("Survey 805 query and all MFA analyses work", {
  testthat::skip_if_not_installed("shinytest2")

  # Locate the existing app without modifying it.
  paths <- c(file.path("..", "..", "inst", "shiny"),
             file.path("inst", "shiny"),
             system.file("shiny", package = "MFA"))
  paths <- paths[file.exists(file.path(paths, "app.R"))]
  testthat::expect_gt(length(paths), 0L)

  app <- shinytest2::AppDriver$new(
    app_dir = paths[[1]], name = "mfa-survey-805",
    load_timeout = 120000, timeout = 120000
  )
  on.exit(app$stop(), add = TRUE)

  # Acknowledge the development notice.
  app$wait_for_js("document.querySelector('#acknowledge_development') !== null")
  app$click("acknowledge_development")

  # Query Survey ID 805 and add it to the selected surveys.
  app$set_inputs(page = "browse", survey_id = "805")
  app$click("query")
  app$wait_for_js("document.querySelector('#use_all') !== null")
  app$click("use_all")
  app$wait_for_js("Array.from(document.querySelectorAll('.mfa-survey-chip')).some(x => x.textContent.includes('805'))")
  testthat::expect_equal(app$get_value(input = "survey_id"), "805")

  # Run all six analyses, using table output where available.
  app$set_inputs(page = "analyses")
  methods <- c("catchSummary", "CPUE_byGear", "detectionByYear",
               "lengthFreqByEffort", "RSD", "ageLengthSummary")

  for (method in methods) {
    app$set_inputs(analysis = method)

    # Clear earlier notifications so this check belongs to the current run.
    app$run_js("document.querySelectorAll('.shiny-notification').forEach(x => x.remove())")
    app$click("run_analysis")
    # Wait for the success notification from this analysis.
    # run_js() does not return a JavaScript string to R, so avoid
    # assigning its result to `status` and passing it to grepl().
    app$wait_for_js(
      "Array.from(document.querySelectorAll('.shiny-notification')).some(x => x.textContent.includes('Analysis complete'))"
    )
    testthat::succeed(info = paste(method, "completed"))

    # Dismiss data warnings, if present, before starting the next analysis.
    app$run_js("$('.modal').modal('hide')")
  }
})
 
 testthat::test_that("Survey 805 analyses return only Bluegill when filtered", {
   testthat::skip_if_not_installed("shinytest2")
   
   paths <- c(file.path("..", "..", "inst", "shiny"),
              file.path("inst", "shiny"),
              system.file("shiny", package = "MFA"))
   paths <- paths[file.exists(file.path(paths, "app.R"))]
   testthat::expect_gt(length(paths), 0L)
   
   app <- shinytest2::AppDriver$new(
     app_dir = paths[[1]], name = "mfa-survey-805-bluegill",
     load_timeout = 120000, timeout = 120000
   )
   on.exit(app$stop(), add = TRUE)
   
   app$wait_for_js("document.querySelector('#acknowledge_development') !== null")
   app$click("acknowledge_development")
   
   app$set_inputs(page = "browse", survey_id = "805")
   app$click("query")
   app$wait_for_js("document.querySelector('#use_all') !== null")
   app$click("use_all")
   app$wait_for_js("Array.from(document.querySelectorAll('.mfa-survey-chip')).some(x => x.textContent.includes('805'))")
   
   # Filter on Catch and Effort Data (the shared filter for all analyses).
   app$set_inputs(page = "view_data")
   app$wait_for_js("document.querySelector('#effort_species_filter') !== null")
   app$set_inputs(effort_species_filter = "Bluegill")
   testthat::expect_equal(app$get_value(input = "effort_species_filter"), "Bluegill")
   
   app$set_inputs(page = "analyses")
   methods <- c("catchSummary", "CPUE_byGear", "detectionByYear",
                "lengthFreqByEffort", "RSD", "ageLengthSummary")
   
   for (method in methods) {
     app$set_inputs(analysis = method)
     # Table is the app's default. Do not set output_type = "Table" here:
     # shinytest2 may time out if the value is already unchanged.
     app$run_js("document.querySelectorAll('.shiny-notification').forEach(x => x.remove())")
     app$click("run_analysis")
     app$wait_for_js(
       "Array.from(document.querySelectorAll('.shiny-notification')).some(x => x.textContent.includes('Analysis complete'))"
     )
     
     # Check the actual exported analysis data, not merely a success toast.
     app$wait_for_js("document.querySelector('#download_analysis') !== null")
     csv_path <- app$get_download("download_analysis")
     result <- utils::read.csv(csv_path, check.names = FALSE, stringsAsFactors = FALSE)
     testthat::expect_true(nrow(result) > 0L,info = paste(method, "has results"))
     testthat::expect_true("Species" %in% names(result),
                           info = paste(method, "has a Species column"))
     
     # Detection-by-year intentionally includes two non-species summary rows.
     allowed <- if (identical(method, "detectionByYear")) {
       c("Bluegill", "N_Surveys", "N_Efforts")
     } else {
       "Bluegill"
     }
     observed <- unique(trimws(as.character(result$Species)))
     testthat::expect_true(
       all(!is.na(observed) & observed %in% allowed),
       info = paste(method, "unexpected Species values:",
                    paste(setdiff(observed, allowed), collapse = ", "))
     )
     testthat::expect_true("Bluegill" %in% observed,
                           info = paste(method, "includes Bluegill"))
     
     # Close any data warning modal before the next analysis.
     app$run_js("$('.modal').modal('hide')")
   }
 })
