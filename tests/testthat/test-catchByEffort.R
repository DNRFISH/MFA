
# ============================================================
# Tests for catchByEffort()
#
# Uses live FISHub data from SurveyId 8914.
# Requires an active database connection named 'con'.
#
# Tests:
# 1. Basic functionality
# 2. Catch source identification
# 3. Catch totals and length summaries
# 4. Default legal sizes
# 5. Special legal-size overrides
# 6. Legal fish counts
# 7. Duplicate output records
# 8. ModuleId-to-SurveyId relationships
# 9. Independent catch count validation
# 10. Missing effort identifiers
# 11. Existing catch data
# ============================================================

con<-FISHub_connect()

# ------------------------------------------------------------
# Helper: retrieve effort data directly from FISHub
# ------------------------------------------------------------

get_test_efforts <- function(con, survey_id = 8914) {
  
  dplyr::tbl(con, "SurveyEffort") %>%
    dplyr::filter(SurveyId == .env$survey_id) %>%
    dplyr::select(
      SurveyId,
      SurveyEffortId,
      SurveyEffortKey,
      ModuleId
    )
}


# ------------------------------------------------------------
# 1. Basic functionality
# ------------------------------------------------------------

test_that("catchByEffort returns expected catch data", {
  
  efforts <- get_test_efforts(con)
  
  result <- catchByEffort(con, efforts)
  
  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0)
  
  expect_named(
    result,
    c(
      "SurveyId",
      "ModuleId",
      "Species",
      "TotalNumberCaught",
      "LengthAverage",
      "LengthMinimum",
      "LengthMaximum",
      "LegalSize",
      "N_legal",
      "CatchTable"
    )
  )
  
  expect_true(all(result$SurveyId == 8914))
})


# ------------------------------------------------------------
# 2. Catch source identification
# ------------------------------------------------------------

test_that("catchByEffort identifies valid catch sources", {
  
  result <- catchByEffort(con, get_test_efforts(con))
  
  expect_true(all(
    result$CatchTable %in% c(
      "Species",
      "InchGroup",
      "ScaleEnvelope"
    )
  ))
  
  expect_false(anyNA(result$CatchTable))
})


# ------------------------------------------------------------
# 3. Catch totals and length summaries
# ------------------------------------------------------------

test_that("catchByEffort produces valid catch summaries", {
  
  result <- catchByEffort(con, get_test_efforts(con))
  
  expect_true(all(
    !is.na(result$TotalNumberCaught)
  ))
  
  expect_true(all(
    result$TotalNumberCaught >= 0
  ))
  
  lengths <- result %>%
    dplyr::filter(
      !is.na(LengthMinimum),
      !is.na(LengthMaximum)
    )
  
  expect_true(all(
    lengths$LengthMinimum <= lengths$LengthMaximum
  ))
  
  lengths <- lengths %>%
    dplyr::filter(!is.na(LengthAverage))
  
  expect_true(all(
    lengths$LengthAverage >= lengths$LengthMinimum - 0.01 &
      lengths$LengthAverage <= lengths$LengthMaximum + 0.01
  ))
})


# ------------------------------------------------------------
# 4. Default legal sizes
# ------------------------------------------------------------

test_that("catchByEffort applies default legal sizes", {
  
  result <- catchByEffort(con, get_test_efforts(con))
  
  defaults <- c(
    "Brook Trout" = 7,
    "Brown Trout" = 8,
    "Largemouth Bass" = 14,
    "Smallmouth Bass" = 14,
    "Walleye" = 15,
    "Northern Pike" = 24,
    "Muskellunge" = 42
  )
  
  for (sp in names(defaults)) {
    
    species_data <- result %>%
      dplyr::filter(
        Species == sp,
        CatchTable != "Species"
      )
    
    if (nrow(species_data) > 0) {
      expect_true(all(
        species_data$LegalSize == defaults[[sp]]
      ))
    }
  }
})


# ------------------------------------------------------------
# 5. Special legal-size overrides
# ------------------------------------------------------------

test_that("catchByEffort applies special legal sizes", {
  
  result <- catchByEffort(
    con,
    get_test_efforts(con),
    Special_Legal_Sizes = c(
      "Largemouth Bass" = 10,
      "Bluegill" = 6
    )
  )
  
  bass <- result %>%
    dplyr::filter(
      Species == "Largemouth Bass",
      CatchTable != "Species"
    )
  
  if (nrow(bass) > 0) {
    expect_true(all(bass$LegalSize == 10))
  }
  
  bluegill <- result %>%
    dplyr::filter(
      Species == "Bluegill",
      CatchTable != "Species"
    )
  
  if (nrow(bluegill) > 0) {
    expect_true(all(bluegill$LegalSize == 6))
  }
})


# ------------------------------------------------------------
# 6. Legal fish counts
# ------------------------------------------------------------

test_that("catchByEffort calculates valid legal counts", {
  
  result <- catchByEffort(con, get_test_efforts(con))
  
  legal <- result %>%
    dplyr::filter(!is.na(N_legal))
  
  expect_true(all(legal$N_legal >= 0))
  
  expect_true(all(
    legal$N_legal <= legal$TotalNumberCaught
  ))
})


# ------------------------------------------------------------
# 7. Duplicate output records
# ------------------------------------------------------------

test_that("catchByEffort does not return duplicate records", {
  
  result <- catchByEffort(con, get_test_efforts(con))
  
  duplicates <- result %>%
    dplyr::count(
      SurveyId,
      ModuleId,
      Species,
      CatchTable
    ) %>%
    dplyr::filter(n > 1)
  
  expect_equal(nrow(duplicates), 0)
})


# ------------------------------------------------------------
# 8. ModuleId-to-SurveyId relationships
# ------------------------------------------------------------

test_that("each ModuleId maps to one SurveyId", {
  
  efforts <- get_test_efforts(con)
  
  duplicates <- efforts %>%
    dplyr::select(ModuleId, SurveyId) %>%
    dplyr::distinct() %>%
    dplyr::count(ModuleId) %>%
    dplyr::filter(n > 1) %>%
    dplyr::collect()
  
  expect_equal(nrow(duplicates), 0)
})


# ------------------------------------------------------------
# 9. Independent catch count validation
# ------------------------------------------------------------

test_that("catchByEffort matches raw species catch totals", {
  
  efforts <- get_test_efforts(con)
  
  result <- catchByEffort(con, efforts)
  
  # Independently summarize the raw species catch table
  raw_counts <- dplyr::tbl(
    con, "ModuleDataCatchBySpecies"
  ) %>%
    dplyr::inner_join(
      dplyr::tbl(con, "ModuleData") %>%
        dplyr::select(ModuleDataId, ModuleId),
      by = "ModuleDataId"
    ) %>%
    dplyr::inner_join(
      efforts %>%
        dplyr::select(ModuleId) %>%
        dplyr::distinct(),
      by = "ModuleId"
    ) %>%
    dplyr::collect() %>%
    dplyr::group_by(ModuleId) %>%
    dplyr::summarise(
      RawTotal = sum(TotalNumberCaught, na.rm = TRUE),
      .groups = "drop"
    )
  
  calculated <- result %>%
    dplyr::filter(CatchTable == "Species") %>%
    dplyr::group_by(ModuleId) %>%
    dplyr::summarise(
      CalculatedTotal = sum(TotalNumberCaught),
      .groups = "drop"
    )
  
  comparison <- dplyr::full_join(
    raw_counts,
    calculated,
    by = "ModuleId"
  )
  
  expect_false(anyNA(comparison$RawTotal))
  expect_false(anyNA(comparison$CalculatedTotal))
  
  expect_equal(
    comparison$CalculatedTotal,
    comparison$RawTotal
  )
})


# ------------------------------------------------------------
# 10. Invalid input: missing SurveyEffortId
# ------------------------------------------------------------

test_that("catchByEffort rejects missing effort identifiers", {
  
  efforts <- get_test_efforts(con) %>%
    dplyr::select(-SurveyEffortId)
  
  expect_error(
    catchByEffort(con, efforts),
    "No survey data included"
  )
})


# ------------------------------------------------------------
# 11. Invalid input: catch data already included
# ------------------------------------------------------------

test_that("catchByEffort rejects existing catch data", {
  
  efforts <- get_test_efforts(con) %>%
    dplyr::mutate(TotalNumberCaught = 1)
  
  expect_error(
    catchByEffort(con, efforts),
    "Catch data included"
  )
})
