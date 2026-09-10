MFA_example_data <- readRDS(
  testthat::test_path("test-data/MFA_example_SurveyEffortData_805.rds")
)

con<-FISHub_connect()

ageDatRaw<-ageLengthSummary(con,MFA_example_data,OutputType = "RawData")

LMBageDat<-ageDatRaw%>%filter(Species=="Largemouth Bass")

test_that("Number of Largemouth Bass aged", {
  expect_equal(nrow(LMBageDat),39) #from dev/examples/LakeSixteen_805_FY2024_DataSum.xlsx
})

ageDatTab<-ageLengthSummary(con,MFA_example_data,OutputType = "Table")
YEPageDat<-ageDatTab%>%filter(Species=="Yellow Perch",Age==5)

test_that("Avg length of age 5 Yellow Perch", {
  expect_equal(YEPageDat$Mean_Length,8) #from dev/examples/LakeSixteen_805_FY2024_DataSum.xlsx
})
