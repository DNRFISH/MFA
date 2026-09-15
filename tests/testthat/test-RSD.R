con <- FISHub_connect()

effortData <- FISH_query(con,QueryType = "Efforts",SurveyId = 805)
lengthFreqData <- lengthFreqByEffort(con = con,effortData = effortData)


RSDsum<-RSD(lengthFreqData)
BCR<-RSDsum%>%filter(Species=="Black Crappie")

test_that("Number of preferred sized black crappie (>= 10 inches)", {
  expect_equal(BCR$N.Preferred,2) #from dev/examples/LakeSixteen_805_FY2024_DataSum.xlsx
})

test_that("RSD.p for black crappie (percent of fish greater than 5 inches that are >= 10 inches)", {
  expect_equal(BCR$RSD.p,18) #from dev/examples/LakeSixteen_805_FY2024_DataSum.xlsx
})

