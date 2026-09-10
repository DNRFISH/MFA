# CPUE tests

#first run it when only one survey was queried
MFA_example_data <- readRDS(
  testthat::test_path("test-data/MFA_example_data_8914.rds")
)

CPUE<-CPUE_byGear(MFA_example_data)
BLG_LMFYKE<-CPUE%>%filter(Species=="Bluegill",GearType=="LMFYKE")
  
test_that("Catch per unit effort for Bluegill in Largemesh Fyke Nets", {
  expect_equal(round(BLG_LMFYKE$CPUE,1),14.2) #from JB via email
})

#rerun it when two surveys were part of query
MFA_example_data2 <- readRDS(
  testthat::test_path("test-data/MFA_example_data_8913_8914.rds")
)

CPUE2<-CPUE_byGear(MFA_example_data2)
BLG_LMFYKE2<-CPUE%>%filter(Species=="Bluegill",GearType=="LMFYKE",SurveyId==8914)

test_that("Catch per unit effort for Bluegill in Largemesh Fyke Nets", {
  expect_equal(round(BLG_LMFYKE2$CPUE,1),14.2)
})
