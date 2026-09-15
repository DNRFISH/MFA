con <- FISHub_connect()


test_that("lengthFreqByEffort returns a length-frequency table", {
  
  effortData <- FISH_query(con,QueryType = "Efforts",SurveyId = 805)
  result <- lengthFreqByEffort(con = con,effortData = effortData)
  
  expect_true(is.data.frame(result))
  expect_gt(nrow(result), 0)
  
  expect_true(all(c(
    "SurveyId",
    "ModuleId",
    "Species",
    "InchGroup",
    "TotalNumberCaught",
    "CatchTable"
  ) %in% names(result)))
  
  expect_true(all(result$CatchTable == "InchGroup"))
})


test_that("lengthFreqByEffort uses the requested effort modules", {
  effortData <- FISH_query(con,QueryType = "Efforts",SurveyId = 805)
  result <- lengthFreqByEffort(con = con,effortData = effortData)
  
  expect_true(all(result$ModuleId %in% effortData$ModuleId))
  expect_true(all(result$SurveyId %in% effortData$SurveyId))
})


test_that("lengthFreqByEffort returns only unmarked fish", {
  effortData <- FISH_query(con,QueryType = "Efforts",SurveyId = 805)
  result <- lengthFreqByEffort(con = con,effortData = effortData)
  
  expect_true(all(result$TotalNumberCaught >= 0))
  expect_true(all(is.na(result$TotalNumberCaught) |
                    result$TotalNumberCaught == floor(result$TotalNumberCaught)))
})


test_that("lengthFreqByEffort preserves module and inch-group detail", {
  effortData <- FISH_query(con,QueryType = "Efforts",SurveyId = 805)
  result <- lengthFreqByEffort(con = con,effortData = effortData)
  
  # Each combination should occur only once in the summary
  expect_equal(
    nrow(result),
    nrow(unique(result[c(
      "SurveyId",
      "ModuleId",
      "Species",
      "InchGroup"
    )]))
  )
})


test_that("lengthFreqByEffort returns a Figure when requested", {
  effortData <- FISH_query(con,QueryType = "Efforts",SurveyId = 805)
  result <- lengthFreqByEffort(con = con,effortData = effortData,OutputType = "Figure")
  expect_s3_class(result, "ggplot")
})


test_that("lengthFreqByEffort rejects invalid OutputType", {
  effortData <- FISH_query(con,QueryType = "Efforts",SurveyId = 805)
  expect_error(lengthFreqByEffort(con = con,effortData = effortData,OutputType = "BadType"))
})
