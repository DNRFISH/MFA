con<-FISHub_connect()

effortData <- FISH_query(con,QueryType = "Efforts",SurveyId = 805)
ageDatRaw<-ageLengthSummary(con,effortData,OutputType = "RawData")

LMBageDat<-ageDatRaw%>%filter(Species=="Largemouth Bass")

test_that("Number of Largemouth Bass aged", {
  expect_equal(nrow(LMBageDat),39) #from dev/examples/LakeSixteen_805_FY2024_DataSum.xlsx
})

ageDatTab<-ageLengthSummary(con,effortData,OutputType = "Table")
YEPageDat<-ageDatTab%>%filter(Species=="Yellow Perch",Age==5)

test_that("Avg length of age 5 Yellow Perch", {
  expect_equal(YEPageDat$Mean_Length,8) #from dev/examples/LakeSixteen_805_FY2024_DataSum.xlsx
})
