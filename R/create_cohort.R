
create_cohort <- function(ssnap_data,
  csv_columns = NULL,
  from_cohort_definition,
  audit_outputs_table = NULL,
  filters = NULL,
  period_type = NULL,
  aggregate_by = NULL
  ) {

  if (is.character(ssnap_data) & (length(ssnap_data) == 1)) {
    ssnap_data <- ssnapinterface::read_audit_csv(ssnap_data,
      column_names = unique(c(
        "PatientId",
        "S1OnsetInHospital", # Need to check why this is a requirement
        "S7TeamClockStopDateTime", #Check this too
        "ProClinV1Id",
        from_cohort_definition$csv_columns,
        csv_columns))
    )
  }
  if (!tibble::is_tibble(ssnap_data)) {
    stop(glue::glue("
      ssnap_data must either be a valid filename consisting of a
      string vector of length 1, or a tibble containing SSNAP data.
    "))
  }

  if (!is.null(filters) & is.list(filters)) {
    stop("filters must be a list of expressions or left as NULL")
  }

  # Filter the data

  ssnap_data <- dplyr::filter(ssnap_data,
    !!! from_cohort_definition[["row_selection_criteria"]])

  # In some cases we are just selecting a cohort with
  # attribute_to_team and apply_data_from will therefore be NULL;
  # in these cases just run the selecting function

  if (is.null(from_cohort_definition[["apply_data_from"]])) {
    results <-
      from_cohort_definition[["attribute_to_team"]](ssnap_data)

  } else {

  # If apply_data_from is set then we are doing a join between the
  # two. We need to work out which columns we are keeping from which.

    choose_records_columns <- if (is.null(
      from_cohort_definition[["attribute_to_team_columns"]])) {
      "PatientId"
    } else {
      c("PatientId", "ProClinV1Id",
      from_cohort_definition[["attribute_to_team_columns"]])
    }

  # Select the patients using choose_patients_from
  # In some cases all patients apply; in others we may only want
  # patients who have hit a certain bit of their journey.

    if (!is.null(from_cohort_definition[["choose_patients_from"]])) {
      patient_tbl <- dplyr::select(
        from_cohort_definition[["choose_patients_from"]](ssnap_data),
        "PatientId")
      ssnap_data <- dplyr::filter(ssnap_data,
        .data[["PatientId"]] %in% patient_tbl[["PatientId"]])
    }
    
    left_cohort <- dplyr::select(
      from_cohort_definition[["attribute_to_team"]](ssnap_data),
      !!choose_records_columns)
    
    right_cohort <- if (is.null(
      from_cohort_definition[["attribute_to_team_columns"]])) {
        from_cohort_definition[["apply_data_from"]](ssnap_data)
    } else {
      apply_record_columns <- c("ProClinV1Id",
        from_cohort_definition[["attribute_to_team_columns"]])
      dplyr::select(
        from_cohort_definition[["apply_data_from"]](ssnap_data),
        -!!apply_record_columns)
    }
    results <- dplyr::right_join(
      left_cohort,
      right_cohort,
      by = "PatientId"
    )
  }

  # Apply filters only AFTER checking the cohort because otherwise
  # we might for example filter out cohort data which we need to
  # apply from a right join etc.
  if (!is.null(filters)) {
    ssnap_data <- dplyr::filter(ssnap_data, !!! filters)
  }
  
  # If period type is set then we are aggregating the data
  # otherwise we're adding fields / returning patient-level data
  if (!is.null(audit_outputs_table)) {
    if (!is.null(period_type)) {
      results <- aggregate_cohort_on_period_type(
        cohort = results,
        period_type = period_type,
        aggregate_by = aggregate_by,
        period_date_field =
          from_cohort_definition[["reporting_period_index_name"]])

      results <- dplyr::summarise(
        results,
        "n" = dplyr::n(),
        !!! audit_outputs_table$exprs)
    } else {
      results <- dplyr::mutate(results,
                            !!! audit_outputs_table$exprs)
    }
  }

  # Return a list rather than the tibble itself.
  # The list will contain 'results' and 'metadata' - we need to build
  # the latter later.
  metadata <- if (!is.null(audit_outputs_table)) {
    dplyr::select(audit_outputs_table,
                            "categories",
                            "descriptors",
                            "data_types")
  } else {
    list(description = from_cohort_definition[["description"]])
  }
  audit_results <- list("results" = results,
                        "metadata" = metadata)
  class(audit_results) <- "audit_results"
  return(audit_results)
}

#' Create a cohort from a cohort definition and audit output spec
#'
#' This function is used to take a cohort definition and an audit
#' output specification and build them together to produce results for
#' a cohort.
#' 
#' create_aggregated data is used to produce aggregated summary
#' statistics for a cohort; whilst create_filtered_cohort is used to
#' create a cohort of patient-level data with filtering applied and/or
#' new calculated fields added.
#'
#' @param ssnap_data Either a tibble containing raw SSNAP data, or
#' a string containing the name of a CSV file.
#' @param csv_columns Columns from the patient data table (processed
#' CSV, referred to as ssnap_data here) needed by the provided
#' audit_outputs_table. This will mostly need renaming to be clearer.
#' @param from_cohort_definition A cohort definition object which
#' describes how to select the patients and data from the cohort.
#' @param audit_outputs_table A table providing column names, fields
#' and labels for the audit outputs.
#' @param filters Filters to apply to ssnap_data before creating the
#' cohort. Common examples would be to select only a subset of teams
#' or a certain date range to reduce the need to summarise results
#' which are not going to be used subsequently.
#' @param period_type A string determining what the gap between
#' time periods should be. The factor should either be 'months',
#' 'periods' or financialyears'. Months and financialyears are
#' self-explanatory. Periods represent reporting periods - these were
#' initially quarterly, then were trimesterly, then quarterly again.
#' Periods are referred to by their numeric number, rather than a
#' defined time period to avoid confusion in the calcalations.
#' @param aggregate_by Set whether we are looking to aggregate by
#' 'team' (default) or 'national'.
#'  
#' @return The cohort is returned in a \code{tibble}.

#' @export

create_aggregated_cohort <- function(ssnap_data,
  csv_columns = NULL,
  from_cohort_definition,
  audit_outputs_table,
  period_type = c("months", "periods", "financialyears"),
  aggregate_by = c("national", "team"),
  filters = NULL) {

  if (is.null(audit_outputs_table)) {
    stop("audit_outputs_table must not be blank")
  }

  return(create_cohort(
    ssnap_data = ssnap_data,
    csv_columns = csv_columns,
    from_cohort_definition = from_cohort_definition,
    audit_outputs_table = audit_outputs_table,
    filters = filters,
    period_type = period_type,
    aggregate_by = aggregate_by))
}
  
#' @export
#' @rdname create_aggregated_cohort

create_filtered_cohort <- function(ssnap_data,
                                   csv_columns = NULL,
                                   from_cohort_definition,
                                   audit_outputs_table = NULL,
                                   filters = NULL) {

  create_cohort(ssnap_data = ssnap_data,
                csv_columns = csv_columns,
                from_cohort_definition = from_cohort_definition,
                audit_outputs_table = audit_outputs_table,
                filters = filters)
}


#' Group a cohort by a date period range
#'
#' This is an internal function used when creating cohorts to group
#' the data by the chosen period range (ie. months, report periods
#' or financial years) using the specified column.
#'
#' @param cohort The cohort being worked upon
#' @param period_type A string determining what the gap between
#' time periods should be. The factor should either be 'months',
#' 'periods' or financialyears'. Months and financialyears are
#' self-explanatory. Periods represent reporting periods - these were
#' initially quarterly, then were trimesterly, then quarterly again.
#' Periods are referred to by their numeric number, rather than a
#' defined time period to avoid confusion in the calcalations.
#' @param period_date_field The column to be used for aggregation.
#'  
#' @return The cohort is returned in a \code{tibble}.

aggregate_cohort_on_period_type <- function(cohort,
  period_type = c("months", "periods", "financialyears"),
  aggregate_by = c("national", "team"),
  period_date_field) {

  cohort <- dplyr::arrange(cohort,
                           .data[[period_date_field]])

  period <- list("months"         = "monthly",
                 "periods"        = ssnap_report_period_series,
                 "financialyears" = ssnap_report_year_series)

  cohort <- dplyr::mutate(
    cohort,
    ReportPeriod = tibbletime::collapse_index(
      as.Date(.data[[period_date_field]]),
      period = period[[period_type]],
      side = "start",
      clean = TRUE))

  if (aggregate_by != "national") {
    cohort <- dplyr::group_by(
      cohort,
      .data[["TeamCode"]])
  }

  cohort <- dplyr::group_by(
    cohort,
    .data[["ReportPeriod"]], .add = TRUE)
}
