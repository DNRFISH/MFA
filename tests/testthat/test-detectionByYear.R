#multiple surveys from one waterbody -- need to find an example SFR to test against
# #Lake Orion
# MFA_example_data <- readRDS(
#   testthat::test_path("test-data/MFA_example_data_multiple_surveys.rds")
# )
# 
# detectionByYearSum<-detectionByYear(MFA_example_data)
# 
# test_that("catchSummary returns the correct total count of fish captured", {
#   expect_equal(sum(catchSum$TotalNumberCaught),1005)
# })
# 
# test_that("catchSummary returns the correct number of species", {
#   expect_equal(n_distinct(catchSum$Species),1)
# })