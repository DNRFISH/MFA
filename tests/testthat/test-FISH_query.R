
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
  expect_warning(result <- FISH_query(con,QueryType = "Survey",Year = 2024),"DO NOT USE") #expected until Catch_discrepancies addressed
  expect_gt(nrow(result), 0)
  expect_true(all(result$Year == 2024))
  
  expect_warning(result <- FISH_query(con,QueryType = "Survey",SurveyPurpose = "Management Evaluation"),"DO NOT USE") #expected until Catch_discrepancies addressed
  expect_gt(nrow(result), 0)
  expect_true(all(result$SurveyPurpose == "Management Evaluation"))
  
  result <- FISH_query(con,QueryType = "Catch",MDNRID = "L7844")
  expect_gt(nrow(result), 0)
  expect_true(all(result$MDNRID == "L7844"))
})


test_that("FISH_query applies multiple filters", {
  expect_warning(result <- FISH_query(con,QueryType = "Catch",GearType = "LMFYKE",Species = "Walleye"),"DO NOT USE") #expected until Catch_discrepancies addressed
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

test_that("FISH_query provides warning about catch discrepancy issues", {
  expect_warning(FISH_query(con,QueryType = "Survey",SurveyId = 15204),"DO NOT USE")
  expect_warning(FISH_query(con,QueryType = "Efforts",SurveyId = 15204),"DO NOT USE")
  expect_warning(FISH_query(con,QueryType = "Catch",SurveyId = 15204),"DO NOT USE")
  expect_no_warning(FISH_query(con,QueryType = "Catch",SurveyId = 15222)) #was split out from a survey with a discrepancy, should be no warning
  expect_warning(FISH_query(con,QueryType = "Catch",SurveyId = 15221),"DO NOT USE")
  
})

test_that("FISH_query provides warning and prompts about extra status and trends efforts", {
  expect_warning(FISH_query(con,QueryType = "Survey",SurveyId = 2785),"DO NOT USE")
  expect_warning(result<-FISH_query(con,QueryType = "Catch",SurveyId = 2785),"DO NOT USE")
  expect_warning(result2<-FISH_query(con,QueryType = "Catch",SurveyId = 2785,SurveyPurpose = "Status & Trends"),"Extra efforts removed")
  expect_equal(nrow(result)-nrow(result2),3)
})

