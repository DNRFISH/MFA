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
BLG_LMFYKE2<-CPUE2%>%filter(Species=="Bluegill",GearType=="LMFYKE",SurveyId==8914)

test_that("Catch per unit effort for Bluegill in Largemesh Fyke Nets", {
  expect_equal(round(BLG_LMFYKE2$CPUE,1),14.2)
})


#Effort output has unique effort/species rows and explicit zeros
testthat::test_that("Effort output has unique effort/species rows and explicit zeros", {
  CPUE_effort<-CPUE_byGear(MFA_example_data,SummaryLevel = "Effort")
  testthat::expect_equal(n_distinct(CPUE_effort$SurveyEffortKey), 8)
  
  expGill<-CPUE_effort|>filter(GearType=="EXPGNET")
  testthat::expect_equal(nrow(expGill), 5)
  testthat::expect_all_equal(expGill$TotalEffort, 1)
  testthat::expect_all_equal(expGill$TotalNumberCaught, 0)
  testthat::expect_all_equal(expGill$CPUE, 0)
})

con<-FISHub_connect()
catchData <- FISH_query(
  con,
  QueryType = "Catch",
  SurveyId = 149
)
test_that("Zero-catch efforts contribute to survey CPUE", {
  
  totEffot<-catchData|>
    filter(GearType == "BOOMSHK")|>
    select(SurveyEffortId,GearType,EffortTotalMeasurement,EffortTotalQuantity)|>
    unique()|>
    summarise(tot=sum(EffortTotalQuantity))
  
  expected_effort=totEffot$tot/60
  
  result <- CPUE_byGear(
    catchData,
    SummaryLevel = "Survey"
  )
  
  mus <- result |>
    filter(
      SurveyId == 149,
      GearType == "BOOMSHK",
      Species == "Muskellunge"
    )
  
  expect_equal(nrow(mus), 1L)
  expect_equal(mus$TotalEffort, expected_effort)
  
  expect_equal(
    mus$CPUE,
    round(mus$TotalNumberCaught / expected_effort, 2)
  )
})


#invalid effort
testthat::test_that("Invalid effort is excluded with warning, not silently counted", {
  bad_data<-MFA_example_data
  bad_data$EffortAlternateQuantity[bad_data$SurveyEffortKey == 2] <- NA_real_
  testthat::expect_warning(
    out <- CPUE_byGear(bad_data, SummaryLevel = "Survey"),
    "removed"
  )
  bg <- dplyr::filter(out, Species=="Bluegill",GearType=="LMFYKE")
  testthat::expect_equal(bg$TotalNumberCaught, 36)
  testthat::expect_equal(bg$TotalEffort, 4)
})


testthat::test_that("Survey and effort figures are ggplot objects", {
  testthat::expect_s3_class(
    CPUE_byGear(MFA_example_data, OutputType = "Figure", SummaryLevel = "Survey"), "ggplot")
  testthat::expect_s3_class(
    CPUE_byGear(MFA_example_data, OutputType = "Figure", SummaryLevel = "Effort"), "ggplot")
})

test_that("Catch totals are preserved during CPUE calculation", {
  
  result <- CPUE_byGear(
    MFA_example_data,
    SummaryLevel = "Survey"
  )
  
  expected <- MFA_example_data |>
    dplyr::filter(!is.na(Species)) |>
    dplyr::group_by(SurveyId, GearType, Species) |>
    dplyr::summarise(
      ExpectedCatch = sum(TotalNumberCaught, na.rm = TRUE),
      .groups = "drop"
    )
  
  actual <- result |>
    dplyr::select(
      SurveyId, GearType, Species,
      TotalNumberCaught
    ) |>
    dplyr::rename(
      ActualCatch = TotalNumberCaught
    )
  
  comparison <- expected |>
    dplyr::left_join(
      actual,
      by = c("SurveyId", "GearType", "Species")
    )
  
  expect_false(anyNA(comparison$ActualCatch))
  
  expect_equal(
    comparison$ActualCatch,
    comparison$ExpectedCatch
  )
})
