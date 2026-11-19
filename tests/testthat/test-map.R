# map tests

con<-FISHub_connect()
catchData <- FISH_query(con, QueryType = "Catch",SurveyId = 805)

test_that("MFA_map returns a leaflet object", {
  map <- MFA_map(catchData)
  
  expect_s3_class(map, "leaflet")
  expect_s3_class(map, "htmlwidget")
})

test_that("MFA_map validates inputs", {

  expect_error(
    MFA_map(data.frame()),
    "nonempty"
  )
  
  expect_error(
    MFA_map(select(catchData, -WaterLat)),
    "Missing required"
  )
  
  expect_error(
    MFA_map(catchData, Symbology = "Invalid"),
    "arg"
  )
})


survey_cpue <- CPUE_byGear(
  catchData,
  SummaryLevel = "Survey"
)

effort_cpue <- CPUE_byGear(
  catchData,
  SummaryLevel = "Effort"
)

survey_cpue |>
  filter(
    GearType == "LMFYKE",
    Species == "Bluegill"
  )

effort_cpue |>
  filter(
    GearType == "LMFYKE",
    Species == "Bluegill"
  )
