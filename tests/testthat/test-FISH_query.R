
con<-FISHub_connect()


#not currenlty working issue #22
# test_that("FISH_query works with default QueryType", {
#   result <- FISH_query(con)
#   expect_true(is.data.frame(result))
#   expect_gt(nrow(result), 0)
# })

test_that("FISH_query is working, filters by SurveyID for each QueryType", {
  survey <- FISH_query(con, QueryType = "Survey",SurveyId = 1162)
  efforts <- FISH_query(con, QueryType = "Efforts",SurveyId = 1162)
  catch <- FISH_query(con, QueryType = "Catch",SurveyId = 1162)
  
  expect_true(all(c("SurveyId","WaterBodyName","MDNRID") %in% names(survey)))
  expect_true(is.data.frame(survey))
  expect_true(is.data.frame(efforts))
  expect_true(is.data.frame(catch))
})


test_that("FISH_query applies individual filters", {
  result <- FISH_query(con,QueryType = "Survey",Year = 2025)
  expect_gt(nrow(result), 0)
  expect_true(all(result$Year == 2025))
  
  result <- FISH_query(con,QueryType = "Survey",SurveyPurpose = "Status & Trends",Year=2025)
  expect_gt(nrow(result), 0)
  expect_true(all(result$SurveyPurpose == "Status & Trends"))
  
  result <- FISH_query(con,QueryType = "Catch",MDNRID = "L7844")
  expect_gt(nrow(result), 0)
  expect_true(all(result$MDNRID == "L7844"))
})


test_that("FISH_query applies multiple filters", {
  result <- FISH_query(con,QueryType = "Catch",GearType = "LMFYKE",Species = "Walleye")
  expect_true(all(result$Species == "Walleye"))
  expect_true(all(result$GearType == "LMFYKE"))
  
  result <- FISH_query(con,QueryType = "Catch",SurveyId = 1162,Species = "Largemouth Bass")
  expect_true(all(result$SurveyId == 1162))
  expect_true(all(result$Species == "Largemouth Bass"))
})

#**update this once I identify an effort** issue #21
# test_that("FISH_query retains efforts with zero catch", {
#   result <- FISH_query(
#     con,
#     QueryType = "Catch",
#     SurveyId = SOME_KNOWN_SURVEY
#   )
#   
#   expect_true(SOME_KNOWN_EFFORT_ID %in% result$SurveyEffortId)
# })

test_that("FISH_query rejects invalid queries", {
  expect_error(FISH_query(con, QueryType = "BadType"))
  expect_error(FISH_query(con, QueryType = "Survey",GearType="BadNet"))
  expect_error(FISH_query(con, QueryType = "Survey",SurveyPurpose="BadPurpose"))
})

test_that("FISH_query provides warning about data issues", {
  expect_warning(FISH_query(con,QueryType = "Survey",SurveyId = 15204),"DON'T USE")
  expect_warning(FISH_query(con,QueryType = "Efforts",SurveyId = 15204),"DON'T USE")
  expect_warning(FISH_query(con,QueryType = "Catch",SurveyId = 15204),"DON'T USE")
})

