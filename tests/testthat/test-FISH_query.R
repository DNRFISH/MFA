
con<-FISHub_connect()



test_that("FISH_query is working", {
  surveyQuery<-FISH_query(con,QueryType = "Survey",SurveyPurpose = "Status & Trends",Year=2025)
  expect_true(all(c(
  "SurveyId",
  "WaterBodyName",
  "MDNRID"
) %in% names(surveyQuery)))
})

test_that("FISH_query filters by SurveyId", {
  result <- FISH_query(
    con,
    QueryType = "Catch",
    SurveyId = 1162
  )
  
  expect_true(all(result$SurveyId == 1162))
})

#need to work on this still... seems to be an issue with the multiple filters
# test_that("FISH_query applies multiple filters", {
#   result <- FISH_query(
#     con,
#     QueryType = "Catch",
#     GearType = "FykeNet",
#     Species = "Walleye"
#   )
#   
#   expect_true(all(result$Year == 2025))
#   expect_true(all(result$GearType == "FykeNet"))
# })
# 
# test_that("FISH_query rejects invalid QueryTypes", {
#   expect_error(
#     FISH_query(con, QueryType = "BadType"),
#     "must be one of"
#   )
# })
