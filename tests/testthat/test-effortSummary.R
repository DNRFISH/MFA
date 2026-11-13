#single survey
#lake sixteen, survey ID 805

MFA_example_data <- readRDS(
  testthat::test_path("test-data/MFA_example_data_805.rds")
)

effortSum <- effortSummary(MFA_example_data)

BOOMSHK<-effortSum%>%filter(GearType=="BOOMSHK")

test_that("effortSummary returns the correct total count of efforts", {
    expect_equal(sum(effortSum$N_Efforts),24) #from dev/examples/LakeSixteen_805_FY2024_DataSum.xlsx; +2 for LIMNO
})

test_that("effortSummary returns the correct number of gears", {
  expect_equal(n_distinct(effortSum$GearType),6) #from dev/examples/LakeSixteen_805_FY2024_DataSum.xlsx; +1 for LIMNO
})

test_that("effortSummary returns the correct N efforts of boomshocking", {
  expect_equal(BOOMSHK$N_Efforts,3) #from dev/examples/LakeSixteen_805_FY2024_DataSum.xlsx, verified by staff
})

test_that("effortSummary returns the correct total seconds of boomshocking", {
  expect_equal(BOOMSHK$TotalEffort,1800) #uses midpoint of inch class
})

con<-FISHub_connect()

test_that("effortSummary works with live data, gets the same answer from catch and effort data", {
  result_efforts <- FISH_query(con,QueryType = "Efforts",SurveyId = 805)
  effortSum_efforts <- effortSummary(result_efforts)
  BOOMSHK_efforts<-effortSum%>%filter(GearType=="BOOMSHK")
  result_catch <- FISH_query(con,QueryType = "Catch",SurveyId = 805)
  effortSum_catch <- effortSummary(result_catch)
  BOOMSHK_catch<-effortSum%>%filter(GearType=="BOOMSHK")
  expect_equal(BOOMSHK_efforts$TotalEffort,BOOMSHK_catch$TotalEffort)
})

test_that("effortSummary can aggregate and get the same answer", {
  result <- FISH_query(con,QueryType = "Catch",SurveyId = c(805,1162))
  effortSum <- effortSummary(result)
  BOOMSHK<-effortSum%>%filter(GearType=="BOOMSHK")
  effortSumAgg <- effortSummary(result,Aggregate = T)
  BOOMSHKAgg<-effortSumAgg%>%filter(GearType=="BOOMSHK")
  expect_equal(BOOMSHKAgg$N_Surveys,2)
  expect_equal(sum(BOOMSHK$TotalEffort),BOOMSHKAgg$TotalEffort)
})