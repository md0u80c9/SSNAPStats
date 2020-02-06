
#' Define available cohort definitions
#' 
#' These functions provide a list of all the standard cohort
#' definitions. It is possible to create your own cohorts on the fly.

#' @importFrom rlang ":="
#' @importFrom rlang "!!"
#' @export

ssnap_cohort_definitions <- list(

  # FirstTeamLockedTo72hrs ==========================================

  FirstTeamLockedTo72hrs = cohort_definition(
    cohort_name = "First team locked to 72hrs cohort",

    row_selection_criteria = rlang::exprs(
      !! ssnap_cohort_filters[["InpatientTeams"]],
      !! ssnap_cohort_filters[["Exclude6mReviews"]],
      !! ssnap_cohort_filters[["LockedTo72Hrs"]]
    ),

    reporting_period_index_name = "S1PatientClockStartDateTime",

    attribute_to_team = cohort_slice_first_team,

    csv_columns = c("TeamCode", "TransferFromDateTime",
                    "TransferFromTeamCode", "LockedS1", "LockedS2",
                    "LockedS3", "S1PatientClockStartDateTime")
  ),

  # FirstTeamAdmittingPatient =======================================

  FirstTeamAdmittingPatient = cohort_definition(
    cohort_name = "First team admitting patient cohort",

    row_selection_criteria = rlang::exprs(
      !! ssnap_cohort_filters[["InpatientTeams"]],
      !! ssnap_cohort_filters[["Exclude6mReviews"]]
    ),

    reporting_period_index_name = "S1PatientClockStartDateTime",

    attribute_to_team = cohort_slice_first_team,

    csv_columns = c("TeamCode", "TransferFromDateTime",
                    "TransferFromTeamCode",
                    "S1PatientClockStartDateTime")
  ),

  # Team72HrCohort ==================================================

  Team72HrCohort = cohort_definition(
    cohort_name = "Team-centred 72hrs cohort",

    row_selection_criteria = rlang::exprs(
      !! ssnap_cohort_filters[["InpatientTeams"]],
      !! ssnap_cohort_filters[["Exclude6mReviews"]],
      !! ssnap_cohort_filters[["72HrDataPresent"]]
    ),

    reporting_period_index_name = "S1PatientClockStartDateTime",

    attribute_to_team = cohort_slice_first_team,

    attribute_to_team_columns = c("TeamCode",
                                    "TransferFromDateTime",
                                    "TransferFromTeamCode"),

    apply_data_from = function(source_data) {
      # Exclude pa
      cohort_data <- dplyr::filter(source_data,
        !! ssnap_cohort_filters[["LockedTo72Hrs"]])
      cohort_slice_first_team(cohort_data)
    },

    csv_columns = c("TeamCode", "TransferFromDateTime",
                    "TransferFromTeamCode",
                    "LockedS1", "LockedS2", "LockedS3",
                    "S1PatientClockStartDateTime"),

    description = glue::glue("
      The team centred results for the first 72 hours of care are
      based on records locked to 72h for patients who arrived at
      hospital (or had their stroke in hospital) in the specified
      report period, and attributed to the first team which treated
      the patient, regardless of which team locked the record to 72h
      (i.e. the second team may have locked the record to 72h, but
      results are attributed to first team).")
  ),

  # Patient72HrCohort ===============================================

  Patient72HrCohort = cohort_definition(
    cohort_name = "Patient-centred 72hrs cohort",

    row_selection_criteria = rlang::exprs(
      !! ssnap_cohort_filters[["InpatientTeams"]],
      !! ssnap_cohort_filters[["Exclude6mReviews"]],
      !! ssnap_cohort_filters[["72HrDataPresent"]]
    ),

    reporting_period_index_name = "S1PatientClockStartDateTime",

    attribute_to_team = function(source_data) source_data,

    attribute_to_team_columns = c("TeamCode",
                                    "TransferFromDateTime",
                                    "TransferFromTeamCode"),
    
    apply_data_from = function(source_data) {
      cohort_data <- dplyr::filter(source_data,
        !! ssnap_cohort_filters[["LockedTo72Hrs"]])
      cohort_slice_first_team(cohort_data)
    },

    csv_columns = c("TeamCode", "TransferFromDateTime",
                    "TransferFromTeamCode",
                    "LockedS1", "LockedS2", "LockedS3",
                    "S1PatientClockStartDateTime"),

    description = glue::glue("
      The patient centred results for the first 72 hours of care are
      based on records locked to 72h for patients who arrived at
      hospital (or had their stroke in hospital) in the specified
      report period, and attributed to all teams treating the patient
      throughout their inpatient stay.")
  ),

  # TeamDischargeCohort =============================================

  TeamDischargeCohort = cohort_definition(
    cohort_name = "Team-centred discharge cohort",
    row_selection_criteria = rlang::exprs(
      !! ssnap_cohort_filters[["InpatientTeams"]],
      !! ssnap_cohort_filters[["Exclude6mReviews"]],
      !! ssnap_cohort_filters[["LockedToDischarge"]],
      !! ssnap_cohort_filters[["ExcludeReadmissions"]],
      .data[["S7DischargeType"]] != "T"
    ),

    reporting_period_index_name = "S7TeamClockStopDateTime",

    attribute_to_team = function(source_data) source_data,

    apply_data_from = NULL,

    csv_columns = c("TeamCode", "TransferFromDateTime",
                    "TransferFromTeamCode",
                    "LockedS4", "LockedS7",
                    "InpatientReadmission", "S7DischargeType",
                    "S7TeamClockStopDateTime")
    ),

  # TeamPost72HrsAllTeamsCohort =====================================

  TeamPost72HrsAllTeamsCohort = cohort_definition(
    cohort_name = "Team-centred post-72hrs cohort (all teams)",
    row_selection_criteria = rlang::exprs(
      !! ssnap_cohort_filters[["InpatientTeams"]],
      !! ssnap_cohort_filters[["Exclude6mReviews"]],
      # Select if we are a transfer which has been actioned
      (((.data[["S7DischargeType"]] == "T") |
          (.data[["S7DischargeType"]] == "TC")) &
          !is.na(.data[["TransferToTeamCode"]])) |
      # Or we are locked to discharge and any other transfer type
      ((.data[["S7DischargeType"]] != "T") &
        (.data[["S7DischargeType"]] != "TC") &
        !! ssnap_cohort_filters[["LockedToDischarge"]])
    ),

    reporting_period_index_name = "S7TeamClockStopDateTime",

    attribute_to_team = function(source_data) source_data,

    apply_data_from = NULL,

    csv_columns = c("TeamCode", "TransferFromDateTime",
                    "TransferFromTeamCode",
                    "S7DischargeType", "TransferToTeamCode",
                    "LockedS4", "LockedS7",
                    "S7TeamClockStopDateTime")
  ),

  # Team7DayCohort ==================================================

  Team7DayCohort = cohort_definition(
    cohort_name = "Team-centred 7-day cohort",
    row_selection_criteria = rlang::exprs(
      !! ssnap_cohort_filters[["InpatientTeams"]],
      !! ssnap_cohort_filters[["Exclude6mReviews"]],
      !! ssnap_cohort_filters[["ExcludeReadmissions"]],
      !! ssnap_cohort_filters[["HasTeamAndPatientClockStart"]],
      !! ssnap_cohort_filters[["HasDischargeFields"]]
    ),

    reporting_period_index_name = "S7TeamClockStopDateTime",

    choose_patients_from = function(source_data) {
      dplyr::filter(source_data,
        !! ssnap_cohort_filters[["LockedToDischarge"]],
        .data[["S7DischargeType"]] != "T")
    },

    attribute_to_team = function(source_data) {
      cohort_data <- dplyr::filter(source_data,
        !! ssnap_cohort_filters[["First7Days"]])
      cohort_slice_first_team(cohort_data)
    },

    attribute_to_team_columns = c("ProClinV1Id",
                                  "TeamCode",
                                  "TransferFromDateTime",
                                  "TransferFromTeamCode",
                                  "S1TeamClockStartDateTime"),


    apply_data_from = function(source_data) {
      # Apply data from the record at the time the patient was
      # discharged or died
      cohort_data <- dplyr::filter(source_data,
        !! ssnap_cohort_filters[["LockedToDischarge"]],
        .data[["S7DischargeType"]] != "T")
      cohort_slice_first_team(cohort_data)
    },

    csv_columns = c("TeamCode", "TransferFromDateTime",
                    "TransferFromTeamCode",
                    "InpatientReadmission",
                    "S1PatientClockStartDateTime",
                    "S1TeamClockStartDateTime",
                    "LockedS4",
                    "LockedS7",
                    "S7DischargeType",
                    "S7TeamClockStopDateTime"),
    
    description = glue::glue("
      Results attributed to the ‘7 day team’ are attributed to the
      team which had the patient in their care at 7 days following
      clock start (or, if the length of stay as an inpatient was less
      than 7 days, the team which discharged the patient from
      inpatient care). These results include measures which are
      considered to be most appropriately designated to the team
      which had the patient in their care at 7 days, but does not
      necessarily indicate that the care was received within 7 days.
      For instance, one measure in this section is whether the
      patient had a urinary continence plan by discharge. It is
      attributed to the team which had the patient at 7 days, but
      the measure is whether the patient had the plan by discharge.")
    ),

  # PatientPost72HrCohort ===========================================

  PatientPost72HrCohort = cohort_definition(
    cohort_name = "Patient-centred post-72hrs cohort",
    row_selection_criteria = rlang::exprs(
      !! ssnap_cohort_filters[["InpatientTeams"]],
      !! ssnap_cohort_filters[["Exclude6mReviews"]],
      !! ssnap_cohort_filters[["ExcludeReadmissions"]],
      !! ssnap_cohort_filters[["HasDischargeFields"]]
    ),

    reporting_period_index_name = "S7TeamClockStopDateTime",

    attribute_to_team = function(source_data) source_data,

    apply_data_from = function(source_data) {
      # TODO we also have to group by Patient ID, sum therapy times
      # and stroke unit admission times, and then apply to the last
      # record
      cohort_data <- dplyr::filter(source_data,
        !! ssnap_cohort_filters[["LockedToDischarge"]],
        .data[["S7DischargeType"]] != "T")
      cohort_slice_first_team(cohort_data)
    },

    csv_columns = c("TeamCode", "TransferFromDateTime",
                    "TransferFromTeamCode",
                    "InpatientReadmission",
                    "LockedS4",
                    "LockedS7",
                    "S7DischargeType",
                    "S7TeamClockStopDateTime")
  ),

  # SixMonthReviews ==========================================

  SixMonthReviews = cohort_definition(
    cohort_name = "Six month reviews",
    row_selection_criteria = rlang::exprs(
      .data[["LockedS8"]] == TRUE |
        (internal_time_difference(
          .data[["S1PatientClockStartDateTime"]],
          date(), "months") > 8)
    ),

    reporting_period_index_name = "S1PatientClockStartDateTime",
  
    attribute_to_team = cohort_slice_first_team,
    csv_columns = c("LockedS8",
                    "S1PatientClockStartDateTime")
  ),
  
  # CriticalHourCohort ==============================================
  
  CriticalHourCohort = cohort_definition(
    cohort_name = "Critical Hour Standards cohort",
    
    row_selection_criteria = rlang::exprs(
      !! ssnap_cohort_filters[["InpatientTeams"]],
      !! ssnap_cohort_filters[["Exclude6mReviews"]],
      !! ssnap_cohort_filters[["72HrDataPresent"]],
      !(.data[["S1OnsetInHospital"]])
    ),
    
    reporting_period_index_name = "S1PatientClockStartDateTime",
    
    attribute_to_team = cohort_slice_first_team,
    
    attribute_to_team_columns = c("TeamCode",
                                  "TransferFromDateTime",
                                  "TransferFromTeamCode"),
    
    apply_data_from = function(source_data) {
      # Exclude pa
      cohort_data <- dplyr::filter(source_data,
                                   !! ssnap_cohort_filters[["LockedTo72Hrs"]])
      cohort_slice_first_team(cohort_data)
    },
    
    csv_columns = c("TeamCode", "TransferFromDateTime",
                    "TransferFromTeamCode",
                    "LockedS1", "LockedS2", "LockedS3",
                    "S1PatientClockStartDateTime"),
    
    description = glue::glue("
      The Critical Hour Standards look at care provided in the first
      hour of admission. These are similar to the team-centred 72
      hour results: they are based upon records locked to 72h for
      patients who arrived at hospital in the specified
      report period, and attributed to the first team which treated
      the patient, regardless of which team locked the record to 72h
      (i.e. the second team may have locked the record to 72h, but
      results are attributed to first team). Unlike the Team 72 hour
      cohort, the Critical Hour Standards do not apply to inpatient
      strokes.")
  )
)
