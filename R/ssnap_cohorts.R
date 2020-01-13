#' Pre-built SSNAP cohorts
#' 
#' This is a vector of valid cohort names used by the SSNAP dataset.
#' SSNAP uses pre-defined cohorts which match cohort selection, to
#' the type of operation to perform and the list of outputs: this
#' makes using the outputs simpler than using the lower-level
#' generic audit functions.
#' 
#' \describe{
#'   \item{Team72HrCohortKIs}{This cohort provides the team centred
#'   Key Indicators only for records locked to 72h for patients who
#'   arrived at hospital (or had their stroke in hospital). The
#'   results are taken from the first record locked to 72hrs for the
#'   patient. The results are attributed to the first team which
#'   treated the patient, regardless of which team locked the record
#'   to 72h (i.e. the second team may have locked the record to 72h,
#'   but results are attributed to first team).}
#'   \item{Patient72HrCohortKIs}{This cohort provides the patient
#'   centred Key Indicators only for the first 72 hours of care and
#'   is based on records locked to 72h for patients who arrived at
#'   hospital (or had their stroke in hospital) divided into the
#'   aggregated time period.}
#'   \item{Team72HrCohortPortfolio}{This cohort provides the full
#'   team centred portfolio for records locked to 72h for patients who
#'   arrived at hospital (or had their stroke in hospital). The
#'   results are taken from the first record locked to 72hrs for the
#'   patient. The results are attributed to the first team which
#'   treated the patient, regardless of which team locked the record
#'   to 72h (i.e. the second team may have locked the record to 72h,
#'   but results are attributed to first team).}
#'   \item{Patient72HrCohortPortfolio}{This cohort provides the
#'   patient centred portfolio for the first 72 hours of care and
#'   is based on records locked to 72h for patients who arrived at
#'   hospital (or had their stroke in hospital) divided into the
#'   aggregated time period.}
#'   \item{TeamDischargeCohortKIs}{Results attributed to discharging
#'   team are attributed to the team which discharged the patient
#'   from inpatient care. The team centred post-72h results are based
#'   on records locked to discharge for patients who were discharged
#'   from inpatient care between the aggregated time periods.}
#'   \item{Team7DayCohortKIs}{NOT YET COMPLETE For the team centred
#'   post-72h results, measures are attributed to teams depending on
#'   the point at which they treated the patient along the inpatient
#'   pathway. Results attributed to the ‘7 day team’ are attributed
#'   to the team which had the patient in their care at 7 days
#'   following clock start (or, if the length of stay as an inpatient
#'   was less than 7 days, the team which discharged the patient from
#'   inpatient care). These results include measures which are
#'   considered to be most appropriately designated to the team which
#'   had the patient in their care at 7 days, but does not
#'   necessarily indicate that the care was received within 7 days.
#'   For instance, one measure in this section is whether the patient
#'   had a urinary continence plan by discharge. It is attributed to
#'   the team which had the patient at 7 days, but the measure is
#'   whether the patient had the plan by discharge.}
#'   \item{TeamPost72HrsAllTeamsCohortKIs}{Results attributed to all
#'   teams are for measures which are answered for every patient by
#'   every team along the pathway (therapy intensity, rehab goal
#'   setting, length of stay in hospital and on stroke unit and the
#'   discharge/transfer destination). These results are based only on
#'   what the team provided rather than what the patient received
#'   across the whole pathway, e.g. the team centred length of stay
#'   is the length of stay at each particular team compared to the
#'   patient centred length of stay which is the length of stay the
#'   patient had across all teams. The team centred post-72h all
#'   teams results are based on records locked to discharge or where
#'   a transfer to another team has been actioned between each
#'   aggregating period. This includes all records which have either
#'   been discharged out of inpatient care or transferred to another
#'   inpatient team.}
#'   \item{PatientPost72HrsCohortKIs}{NOT YET COMPLETE This section
#'   shows the patient centred results for the care between 72 hours
#'   and discharge from inpatient care. It is attributed to all teams
#'   which treated the patient at any point in their care. This means
#'   that a team which only treated the patient during the first 72h
#'   will still have the results for this patient's care between 72
#'   hours and discharge from inpatient care. We hope that this will
#'   encourage an open dialogue between teams treating patients along
#'   a care pathway and that teams treating the patient initially
#'   reflect on the continuing care they receive, as this will also
#'   impact upon the initial team's longer term outcome results. The
#'   patient centred post-72h results are based on records locked to
#'   discharge for patients who were discharged between each
#'   aggregating period.}
#'  }

#' @export
ssnap_cohort_names <- names(ssnap_output_specs)

#' Create a cohort of SSNAP data
#' 
#' This function is a higher level interface to the generic
#' create_cohorts functions.
#' 
#' Using the list of named cohort outputs in \code{ssnap_cohort_names}
#' it matches the correct cohort selection to the correct type of
#' operation and list of outputs, simplifying the eventual code in
#' reports and outputs.
#' 
#' You can still roll your own outputs by using the generic
#' \code{create_outputs} functions and supplying inputs from
#' \code{ssnap_cohort_definitions}, outputs from
#' \code{ssnap_audit_output_specs} or even building your own custom
#' output spec from \code{ssnap measures} depending upon how much
#' control and customisation you need.
#' @export

ssnap_cohorts <- function(ssnap_data,
  cohort,
  period_type = c("months", "periods", "financialyears"),
  aggregate_by = c("national", "team"),
  convert_team_codes = TRUE,
  filters = NULL) {

  if (!(cohort %in% names(ssnap_output_specs))) {
    stop("cohort name not known")
  }

  output_tbl <- create_output_tbl(
    ssnap_output_specs[[cohort]][["outputs_table"]])

  cohort_results <- create_aggregated_cohort(
    ssnap_data = ssnap_data,
    csv_columns = csv_columns_from_output_table(
      ssnap_output_specs[[cohort]][["outputs_table"]]),
    from_cohort_definition =
      ssnap_cohort_definitions[[
        ssnap_output_specs[[cohort]][["cohort"]]
      ]],
    audit_outputs_table = output_tbl,
    period_type = period_type,
    aggregate_by = aggregate_by,
    filters = filters)

  if (convert_team_codes) {
    cohort_results[["results"]] <-
      append_team_data_from_teamcode(cohort_results[["results"]])
  }

  return(cohort_results)
}

#' Create the SSNAP Scores summary from multiple cohorts
#' 
#' Unlike other cohorts, SSNAP Scores is built from the composite of
#' multiple cohorts, with a series of additional columns then created
#' on top. This can't be done easily within our generic audit
#' framework, so we create a special function for doing this.
#' 
#' @param ssnap_data Either a tibble containing raw SSNAP data, or
#' a string containing the name of a CSV file.
#' @param period_type A string determining what the gap between
#' time periods should be. The factor should either be 'months',
#' 'periods' or financialyears'. Months and financialyears are
#' self-explanatory. Periods represent reporting periods - these were
#' initially quarterly, then were trimesterly, then quarterly again.
#' Periods are referred to by their numeric number, rather than a
#' defined time period to avoid confusion in the calcalations.
#' @param aggregate_by Set whether we are looking to aggregate by
#' 'team' (default) or 'national'.
#' @param filters Filters to apply to ssnap_data before creating the
#' cohort. Common examples would be to select only a subset of teams
#' or a certain date range to reduce the need to summarise results
#' which are not going to be used subsequently.
#'  
#' @return The cohort is returned in a \code{tibble}.
#' 
#' @export

ssnap_scores <- function(ssnap_data,
  period_type = c("months", "periods", "financialyears"),
  aggregate_by = c("national", "team"),
  filters = NULL) {

  # We are going to repeatedly use the same data so pre-load the
  # data in one go if it's not in memory already.

  if (is.character(ssnap_data) & (length(ssnap_data) == 1)) {
    ssnap_data <- ssnapinterface::read_audit_csv(ssnap_data)
  }

  join_column <- c("TeamCode", "ReportPeriod")
  tcrename_columns <- c(TeamCode = "TCTeamCode",
                        ReportPeriod = "TCReportPeriod")
  pcrename_columns <- c(TeamCode = "PCTeamCode",
                        ReportPeriod = "PCReportPeriod")
  if (aggregate_by == "national") {
    join_column <- c("ReportPeriod")
    tcrename_columns <- c(ReportPeriod = "TCReportPeriod")
    pcrename_columns <- c(ReportPeriod = "PCReportPeriod")
  }

  # Fetch the team 72hr cohorts, precede all the columns with TC
  team72 <- ssnap_cohorts(ssnap_data,
                          "Team72HrCohortKIs",
                          period_type,
                          aggregate_by,
                          convert_team_codes = FALSE)
  team72[["results"]] <- dplyr::rename_all(team72[["results"]],
                                           list(~paste0("TC", .)))
  team72[["results"]] <- dplyr::rename(team72[["results"]],
                          !!! tcrename_columns,
                          TCn72hrs = "TCn")

  patient72 <- ssnap_cohorts(ssnap_data,
                          "Patient72HrCohortKIs",
                          period_type,
                          aggregate_by,
                          convert_team_codes = FALSE)
  patient72[["results"]] <- dplyr::rename_all(patient72[["results"]],
                                              list(~paste0("PC", .)))
  patient72[["results"]] <- dplyr::rename(patient72[["results"]],
                             !!! pcrename_columns,
                             PCn72hrs = "PCn")

  # Join team and patient 72hrs and drop the original tables
  cohort <- dplyr::inner_join(team72[["results"]],
                              patient72[["results"]],
                              by = join_column)
  rm(team72)
  rm(patient72)

  team_discharge <- ssnap_cohorts(ssnap_data,
                             "TeamDischargeCohortKIs",
                             period_type,
                             aggregate_by,
                             convert_team_codes = FALSE)
  team_discharge <- team_discharge[["results"]]
  
  team_discharge <- dplyr::rename_all(team_discharge,
                                      list(~paste0("TC", .)))
  team_discharge <- dplyr::rename(team_discharge,
                             !!! tcrename_columns,
                             TCnDischarge = "TCn")

  # Join team discharge and drop the original table
  cohort <- dplyr::inner_join(
    cohort, team_discharge, by = join_column)
  rm(team_discharge)

  team_post72hrs <- ssnap_cohorts(ssnap_data,
                                 "TeamPost72HrsAllTeamsCohortKIs",
                                 period_type,
                                 aggregate_by,
                                 convert_team_codes = FALSE)
  team_post72hrs <- team_post72hrs[["results"]]
  
  team_post72hrs <- dplyr::rename_all(team_post72hrs,
                                      list(~paste0("TC", .)))
  team_post72hrs <- dplyr::rename(team_post72hrs,
                                 !!! tcrename_columns,
                                 TCnPost72hrs = "TCn")

  # Join team post 72hrs and drop the original table
  cohort <- dplyr::inner_join(cohort,
                              team_post72hrs,
                              by = join_column)
  rm(team_post72hrs)

  patient_post72hrs <- ssnap_cohorts(ssnap_data,
                                  "PatientPost72HrsAllTeamsCohortKIs",
                                  period_type,
                                  aggregate_by,
                                  convert_team_codes = FALSE)
  patient_post72hrs <- patient_post72hrs[["results"]]
  
  patient_post72hrs <- dplyr::rename_all(patient_post72hrs,
                                         list(~paste0("PC", .)))
  patient_post72hrs <- dplyr::rename(patient_post72hrs,
                                  !!! pcrename_columns,
                                  PCnPost72hrs = "PCn")

  # Join team post 72hrs and drop the original table
  cohort <- dplyr::inner_join(cohort,
                              patient_post72hrs,
                              by = join_column)
  rm(patient_post72hrs)

  cohort <- append_team_data_from_teamcode(cohort)

  cohort <- dplyr::mutate(cohort,
    !!! summary_values_ssnap_scores("TC"),
    !!! summary_values_ssnap_scores("PC")
  )
  return(cohort)
}

#' @export
ssnap_transfer_tree <- function(ssnap_data,
  transfer_type = c("from", "to"),
  period_type = c("months", "periods", "financialyears"),
  aggregate_by = c("national", "team"),
  filters = NULL) {

  cohort_name <- "TeamPost72HrsAllTeamsCohort"
  report_field <- "reporting_period_index_name"

  transfer_type <- dplyr::if_else(transfer_type == "from",
                    "TransferFromTeamCode",
                    "TransferToTeamCode")

  post72hr <- create_filtered_cohort(
    ssnap_data = ssnap_data,
    from_cohort_definition = ssnap_cohort_definitions[[cohort_name]])
  post72hr <- post72hr[["results"]]
  
  post72hr <- dplyr::select(post72hr,
    ssnap_cohort_definitions[[cohort_name]][[report_field]],
    "TeamCode",
    transfer_type)

  #TODO We need to hive out the tibbletime indexing from
  # create_cohorts to its own function and use it here.

  if (!is.null(period_type)) {
    post72hr <- aggregate_cohort_on_period_type(
      cohort = post72hr,
      period_type = period_type,
      aggregate_by = aggregate_by,
      period_date_field = from_cohort_definition[[report_field]])
  }

  transfer_tree_from <- dplyr::group_by(post72hr,
                                      .data[["TeamCode"]],
                                      .data[[transfer_type]],
                                      add = TRUE)
  transfer_tree_from <- dplyr::count(transfer_tree_from,
                                   .data[["TeamCode"]],
                                   .data[[transfer_type]])
}
