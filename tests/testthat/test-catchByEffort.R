con<-FISHub_connect()

test_that("catchByEffort works with FISH_query Efforts output", {
  effortData <- FISH_query(con,QueryType = "Efforts",SurveyId = 805)
  result <- catchByEffort(con = con,effortData = effortData)
  
  expect_true(is.data.frame(result))
  expect_gt(nrow(result), 0)
  
  expect_true(all(c(
    "SurveyId",
    "ModuleId",
    "Species",
    "TotalNumberCaught",
    "LengthAverage",
    "LengthMinimum",
    "LengthMaximum",
    "LegalSize",
    "N_legal",
    "CatchTable"
  ) %in% names(result)))
})


test_that("catchByEffort rejects data without SurveyEffortId", {
  badData <- data.frame(
    ModuleId = 123,
    SurveyId = 805
  )
  
  expect_error(
    catchByEffort(con, badData)
  )
})

test_that("catchByEffort rejects data containing catch data", {
  badData <- data.frame(
    SurveyEffortId = 123,
    ModuleId = 456,
    SurveyId = 805,
    TotalNumberCaught = 10
  )
  
  expect_error(
    catchByEffort(con, badData)
  )
})

test_that("catchByEffort returns the requested SurveyId and data type", {
  effortData <- FISH_query(con,QueryType = "Efforts",SurveyId = 805)
  result <- catchByEffort(con, effortData)
  expect_true(all(result$SurveyId == 805))
  expect_true(all(result$CatchTable %in% c("InchGroup")))
})

#**add test for multiple catch data types**
# test_that("catchByEffort returns the requested data types", {
#   effortData <- FISH_query(con,QueryType = "Efforts",SurveyId = NEED SURVEY)
#   result <- catchByEffort(con, effortData)
#   expect_true(SPECIFY COUNTS)
# })


test_that("catchByEffort applies default legal size", {
  effortData <- FISH_query(con,QueryType = "Efforts",SurveyId = 805)
  result <- catchByEffort(con, effortData)
  
  LMB <- result[result$Species == "Largemouth Bass", ]
  
  if (nrow(LMB) > 0) {
    expect_true(all(LMB$LegalSize == 14))
  }
})

test_that("catchByEffort applies special legal-size overrides to the right species", {
  effortData <- FISH_query(con,QueryType = "Efforts",SurveyId = 805)
  result <- catchByEffort(con, effortData,Special_Legal_Sizes = c("Largemouth Bass" = 10))
  
  LMB <- result[result$Species == "Largemouth Bass", ]
  expect_true(all(LMB$LegalSize == 10))
  
  NOP <- result[result$Species == "Northern Pike", ]
  expect_true(all(NOP$LegalSize == 24))
})

#**need to get a verified avg length and # legal for a specific effort**
# test_that("catchByEffort calculates inch-group length correctly", {
#   effortData <- FISH_query(con,QueryType = "Efforts",SurveyId = 805)
#   result <- catchByEffort(con, effortData)
#   BLG <- result%>%filter(Species == "Bluegill",ModuleId==85864) 
#   expect_true(all(BLG$LengthAverage == 4.17)) 
# })
# 
# test_that("catchByEffort calculates N_legal correctly", {
#   effortData <- FISH_query(con,QueryType = "Efforts",SurveyId = 805)
#   result <- catchByEffort(con, effortData)
#   NOP <- result%>%filter(Species == "Northern Pike") 
#   expect_true(all() 
# })

##**add survey with only species-level catch data**
# test_that("catchByEffort returns NA N_legal for species-level catch", {
#   effortData <- FISH_query(con,QueryType = "Efforts",SurveyId = 1162)
#   
#   result <- catchByEffort(con, effortData)
#   
#   species <- result[result$CatchTable == "Species", ]
#   
#   if (nrow(species) > 0) {
#     expect_true(all(is.na(species$N_legal)))
#     expect_true(all(is.na(species$LegalSize)))
#   }
# })
