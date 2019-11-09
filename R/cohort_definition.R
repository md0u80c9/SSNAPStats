#' Creates a new cohort_definition object
#'
#' This function is used to create a new \code{cohort_definition}.
#' 
#' Cohort definitions are used to describe how to take raw audit data
#' and select the data to represent a particular cohort.
#' 
#' Most cohorts involve selecting, and/or filtering data to get the
#' required cohort. \code{row_selection_criteria} is used to pass a
#' list of filters which should be applied. The filters are passed as
#' a list of expressions: where multiple filters are present they are
#' ANDed together (the equivalent to each individual filter being
#' applied in sequence).
#' 
#' All cohorts are time series objects: it is therefore important to
#' state which time series will form the index. The \code{tsibble}
#' package is used to support temporal-context outputs. This allows
#' the final results to be broken down and displayed by date range.
#' Sometimes rather than just selection we need to do more complex
#' operations to select our cohort - such as slicing the data to find
#' the first record meeting criteria. In these circumstances we
#' provide a function in attribute_to_team, which must take in a
#' single parameter (\code{source_data}).
#' 
#' Where we have multiple records pertaining to a patient (for
#' example where the patient has multiple admissions, or passes
#' between different teams), we may want to apply the results from
#' other records to our selected cohort. To do this we can use the
#' \code{apply_data_from} parameter. The cohort selection is then
#' right joined by \code{dplyr} to the apply_data_from results. Like
#' \code{attribute_to_team} this is a function with a single
#' parameter \code{source_data}.
#'
#' @param row_selection_criteria A list of expressions which describe
#' the filters to be applied to the raw data.
#' @param reporting_period_index_name The name of a date/time
#' field which will be used as the temporal index for results.
#' @param choose_patients_from_description A textual description of
#' how the function choose_patients_from should work.
#' @param choose_patients_from A function which describes which
#' patients should be used to produce the aggregated data. By default
#' this is NULL which will be all patients included.
#' @param attribute_to_team_description A textual description of
#' how the function attribute_to_team should work.
#' @param attribute_to_team A function which describes how
#' to find the records we want in our population. The function should
#' accept a single parameter, \code{source_data}. Typical uses of this
#' parameter are to apply slice operations for example, or complex
#' filter operations in combination with other actions. If not
#' required, this parameter should be set as NULL or omitted.
#' @param attribute_to_team_columns This is a list of the column
#' names we want to keep from the population we have selected the
#' records from. Columns not stated within this list will be
#' overwritten with the data in the apply_data_from column below.
#' The two columns which do not need specifying in this are the
#' unique patient identifier (PatientId) and the unique record
#' identifier (ProClinV1Id).
#' @param apply_data_from_description A textual description of
#' how the function apply_data_from should work.
#' @param apply_data_from A function which describes how to query for
#' the data we want to apply to our final cohort. This is used where
#' we might want to apply data from records different to the ones we
#' have selected (such as in SSNAP's Team72hr cohort where we choose
#' records from the admitting team, but we apply the results from
#' the team who locked the record at 72hrs).
#' @param csv_columns a character vector of all the columns from the
#' raw CSV file needed to make this audit measure.
#' @param description A textural description of what the cohort is
#' intended to represent. This will be passed to the metadata of
#' outputs.
#'
#' @return The created cohort_definition object.
#' @export

cohort_definition <- function(
  cohort_name,
  row_selection_criteria,
  reporting_period_index_name,
  choose_patients_from_description = NULL,
  choose_patients_from = NULL,
  attribute_to_team_description = NULL,
  attribute_to_team = NULL,
  attribute_to_team_columns = NULL,
  apply_data_from_description = NULL,
  apply_data_from = NULL,
  csv_columns = NULL,
  description = NULL) {

  if (!is.character(cohort_name)) {
    stop("cohort_name must be a string")
  }
  if (!is.list(row_selection_criteria)) {
    stop("row_selection_criteria must be a list of expressions")
  }
  if (!is.character(reporting_period_index_name) |
      length(reporting_period_index_name) != 1) {
    stop("reporting_period_index_name must be a string")
  }
  if (!is.null(choose_patients_from) &
      !is.function(choose_patients_from)) {
    stop("choose_patients_from must be a function")
  }
  if (!is.null(attribute_to_team_columns) &
      !is.character(attribute_to_team_columns)) {
    stop(glue::glue("attribute_to_team_columns must be either NULL 
                    or a character vector"))
  }
  if (!is.null(attribute_to_team) &
      !is.function(attribute_to_team)) {
    stop("attribute_to_team must be a function")
  }
  if (!is.null(apply_data_from) & !is.function(apply_data_from)) {
    stop("apply_data_from must be a function")
  }
  if (!is.null(csv_columns) & !is.character(csv_columns)) {
    stop("csv_columns must be either NULL or a character vector")
  }

  cohort_definition_output <- rlang::list2(
    "cohort_name" = cohort_name,
    "row_selection_criteria" = row_selection_criteria,
    "reporting_period_index_name" = reporting_period_index_name,
    "choose_patients_from_description" =
      choose_patients_from_description,
    "choose_patients_from" = choose_patients_from,
    "attribute_to_team_description" = attribute_to_team_description,
    "attribute_to_team" = attribute_to_team,
    "attribute_to_team_columns" = attribute_to_team_columns,
    "apply_data_from" = apply_data_from,
    "apply_data_from_description" = apply_data_from_description,
    "csv_columns" = csv_columns,
    "description" = description)
  class(cohort_definition_output) <- "cohort_definition"
  return(cohort_definition_output)
}

#' Test if the object is an cohort_definition
#'
#' This function returns `TRUE` for cohort_definitions
#'
#' @param x An object
#' @return `TRUE` if the object inherits from the `cohort_definition`
#' class.
#' @export

is_cohort_definition <- function(x) {
  "cohort_definition" %in% class(x)
}

#' @rdname is_cohort_definition
#' @usage NULL
#' @export
is.cohort_definition <- is_cohort_definition
