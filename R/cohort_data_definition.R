#' Creates a new cohort_data_definition object
#'
#' This function is used to create a new cohort_data_definition
#' object.
#' Results from our audit take two forms - either we want to collect
#' aggregated data from a temporally grouped cohort of patients; or
#' we want access to a subset of columns from that cohort.
#' Both the cohorts and the data that we want tend to have a
#' many-to-many relationship with each other - ie. we often want the
#' same aggregated results from multiple different cohort types (in
#' the case of SSNAP team and patient-centred results). In other
#' cases we want to apply different rules to the same cohorts in
#' different circumstances (in the case of SSNAP just Key Indicators
#' or just audit compliance in some cases, whereas on others the
#' whole portfolio is needed).
#' Using cohort_data_definitions we can combine all the permutations
#' we are likely to use commonly; or make our own custom permutations
#' eg. when creating user filters in a realtime dashboard.
#'
#' @param cohort_definition A cohort definition object, built using
#' the \code{cohort_definition} function.
#' @param audit_output_spec An output definition, built using the
#' \code{create_output} function.
#'
#' @return The created cohort_definition object.
#' @export

cohort_data_definition <- function(
  cohort_definition,
  audit_output_spec) {

  if (!is_cohort_definition(cohort_definition)) {
    stop("cohort_definition must be a cohort definition")
  }
  if (!is_audit_output_spec(audit_output_spec) |
      length(reporting_period_index_name) != 1) {
    stop("audit_output_spec must be a string")
  }

  cohort_definition_output <- rlang::list2(
    "cohort_definition" = cohort_definition,
    "audit_output_spec" = audit_output_spec)
  class(cohort_definition_output) <- "cohort_data_definition"
  return(cohort_definition_output)
}

#' Test if the object is a cohort_data_definition
#'
#' This function returns `TRUE` for cohort_data_definitions
#'
#' @param x An object
#' @return `TRUE` if the object inherits from the
#' `cohort_data_definition` class.
#' @export

is_cohort_data_definition <- function(x) {
  "cohort_data_definition" %in% class(x)
}

#' @rdname is_cohort_definition
#' @usage NULL
#' @export
is.cohort_definition <- is_cohort_definition
