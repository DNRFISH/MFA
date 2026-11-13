
#' Summarize Survey Effort
#'
#' Summarizes sampling effort by survey and gear type,
#' optionally aggregating across multiple surveys.
#'
#' Effort counts include all unique SurveyEffortKeys,
#' regardless of whether effort measurements are available.
#' Effort quantities are summarized separately by unit.
#'
#' @param effortData Data frame returned by FISH_query()
#'   using QueryType = "Efforts" or "Catch".
#' @param Aggregate Logical. If TRUE, combines effort across
#'   surveys. Default is FALSE.
#'
#' @return A data frame containing effort counts and
#'   summed effort quantities by measurement unit.
#'
#'@importFrom rlang :=
#'
#' @export
#'
#' @examples
#' \dontrun{
#' con <- FISHub_connect()
#' dat <- FISH_query(con, QueryType = "Efforts",
#'                   Year = 2025)
#'
#' effortSummary(dat)
#' effortSummary(dat, Aggregate = TRUE)
#' }

effortSummary <- function(effortData,
                          Aggregate = FALSE) {
  
  # Validate argument
  if (!is.logical(Aggregate) ||
      length(Aggregate) != 1L ||
      is.na(Aggregate)) {
    stop("Aggregate must be TRUE or FALSE.")
  }
  
  required_cols <- c(
    "SurveyId",
    "SurveyEffortKey",
    "GearType",
    "EffortTotalQuantity",
    "EffortTotalMeasurement",
    "EffortAlternateQuantity",
    "EffortAlternateMeasurement"
  )
  
  missing_cols <- setdiff(required_cols, names(effortData))
  
  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  # Remove duplicate records caused by catch joins
  effort <- effortData %>%
    dplyr::select(dplyr::all_of(required_cols)) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      GearType = trimws(GearType),
      
      EffortTotalMeasurement = dplyr::na_if(
        trimws(EffortTotalMeasurement), ""
      ),
      
      EffortAlternateMeasurement = dplyr::na_if(
        trimws(EffortAlternateMeasurement), ""
      )
    )
  
  # Ensure each SurveyId-SurveyEffortKey has one record
  inconsistent <- effort %>%
    dplyr::count(SurveyId, SurveyEffortKey) %>%
    dplyr::filter(n > 1)
  
  if (nrow(inconsistent) > 0) {
    stop(
      "Some SurveyId-SurveyEffortKey combinations ",
      "have multiple distinct effort records."
    )
  }
  
  # Identify valid primary and alternate measurements
  effort <- effort %>%
    dplyr::mutate(
      IsMeasured =
        !is.na(EffortTotalQuantity) &
        !is.na(EffortTotalMeasurement),
      
      IsAlternateMeasured =
        !is.na(EffortAlternateQuantity) &
        !is.na(EffortAlternateMeasurement)
    )
  
  # Group by survey and gear, or gear only
  group_cols <- c(
    if (!Aggregate) "SurveyId",
    "GearType"
  )
  
  # Count efforts independently of measurement units
  effortCounts <- effort %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(group_cols))
    ) %>%
    dplyr::summarise(
      N_Surveys = dplyr::n_distinct(SurveyId),
      
      N_Efforts = dplyr::n_distinct(
        SurveyId, SurveyEffortKey
      ),
      
      N_Efforts_Measured = sum(IsMeasured),
      
      N_Efforts_Missing_Measurements =
        sum(!IsMeasured),
      
      .groups = "drop"
    )
  
  # Helper for summarizing quantities by measurement unit
  summarize_measurements <- function(data,
                                     quantity_col,
                                     unit_col,
                                     quantity_name,
                                     unit_name) {
    
    valid <- data %>%
      dplyr::filter(
        !is.na(.data[[quantity_col]]),
        !is.na(.data[[unit_col]])
      )
    
    if (nrow(valid) == 0) {
      return(NULL)
    }
    
    valid %>%
      dplyr::group_by(
        dplyr::across(dplyr::all_of(
          c(group_cols, unit_col)
        ))
      ) %>%
      dplyr::summarise(
        Total = sum(.data[[quantity_col]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::rename(
        !!quantity_name := Total,
        !!unit_name := dplyr::all_of(unit_col)
      )
  }
  
  # Summarize primary measurements
  primary <- summarize_measurements(
    effort,
    "EffortTotalQuantity",
    "EffortTotalMeasurement",
    "TotalEffort",
    "EffortUnit"
  )
  
  # Summarize alternate measurements
  alternate <- summarize_measurements(
    effort,
    "EffortAlternateQuantity",
    "EffortAlternateMeasurement",
    "TotalAlternateEffort",
    "AlternateEffortUnit"
  )
  
  # Join counts and measurement summaries
  out <- effortCounts
  
  if (!is.null(primary)) {
    out <- out %>%
      dplyr::left_join(primary, by = group_cols)
  } else {
    out$EffortUnit <- NA_character_
    out$TotalEffort <- NA_real_
  }
  
  if (!is.null(alternate)) {
    out <- out %>%
      dplyr::left_join(alternate, by = group_cols)
  }
  
  # Remove survey count when not aggregating
  if (!Aggregate) {
    out <- out %>%
      dplyr::select(-N_Surveys) %>%
      dplyr::arrange(SurveyId, GearType)
  } else {
    out <- out %>%
      dplyr::arrange(GearType)
  }
  
  return(out)
}
