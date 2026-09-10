#single survey
#lake sixteen, survey ID 805

MFA_example_data <- readRDS(
  testthat::test_path("test-data/MFA_example_data_805.rds")
)

catchSum <- catchSummary(MFA_example_data)

NOP<-catchSum%>%filter(Species=="Northern Pike")

LMB<-catchSum%>%filter(Species=="Largemouth Bass")

test_that("catchSummary returns the correct total count of fish captured", {
    expect_equal(sum(catchSum$TotalNumberCaught),1078) #from dev/examples/LakeSixteen_805_FY2024_DataSum.xlsx
})

test_that("catchSummary returns the correct number of species", {
  expect_equal(n_distinct(catchSum$Species),22) #from dev/examples/LakeSixteen_805_FY2024_DataSum.xlsx
})

test_that("catchSummary returns the correct number of Northern Pike captured", {
  expect_equal(NOP$TotalNumberCaught,5) #from dev/examples/LakeSixteen_805_FY2024_DataSum.xlsx, verified by staff
})

test_that("catchSummary returns the minimum length of Northern Pike captured", {
  expect_equal(NOP$LengthMinimum,18.5) #uses midpoint of inch class
})

test_that("catchSummary returns the maximim length of Northern Pike captured", {
  expect_equal(NOP$LengthMaximum,28.5) #uses midpoint of inch class
})

test_that("catchSummary returns the correct average length Largemouth Bass", {
  expect_equal(LMB$LengthAverage,7.02) #note differences from FCS because of calculation
})

test_that("catchSummary returns the correct percent legal  Largemouth Bass", {
  expect_equal(LMB$PctLegal,4)
})

#multiple surveys from one waterbody, subset back down to one survey
#lake orion
MFA_example_data <- readRDS(
  testthat::test_path("test-data/MFA_example_data_multiple_surveys.rds")
)

catchSum <- catchSummary(MFA_example_data%>%filter(SurveyId==1162))

test_that("catchSummary returns the correct total count of fish captured", {
  expect_equal(sum(catchSum$TotalNumberCaught),1005) #from dev/examples/LakeOrion_1162_draft
})

test_that("catchSummary returns the correct number of species", {
  expect_equal(n_distinct(catchSum$Species),1) #from dev/examples/LakeOrion_1162_draft
})

