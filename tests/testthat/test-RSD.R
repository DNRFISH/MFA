

MFA_example_data <- readRDS(
  testthat::test_path("test-data/MFA_example_LengthFreqdata_805.rds")
)

RSDsum<-RSD(MFA_example_data)
BCR<-RSDsum%>%filter(Species=="Black Crappie")

test_that("Number of preferred sized black crappie (>= 10 inches)", {
  expect_equal(BCR$N.Preferred,2) #from dev/examples/LakeSixteen_805_FY2024_DataSum.xlsx
})

test_that("RSD.p for black crappie (percent of fish greater than 5 inches that are >= 10 inches)", {
  expect_equal(BCR$RSD.p,18) #from dev/examples/LakeSixteen_805_FY2024_DataSum.xlsx
})

