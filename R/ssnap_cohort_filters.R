#' Filters used in the creation of cohorts for SSNAP
#'
#' These filters tend to be used frequently in cohort generation: it
#' also helps readability to design them as a list of expressions
#' which are inserted as the final query becomes much more readable
#' and filters can be tested individually with records.
#' 
#' @export

ssnap_cohort_filters <- rlang::exprs(
  "InpatientTeams" = .data[["TeamCode"]] > 0,

  "Exclude6mReviews" = !(is.na(.data[["TransferFromDateTime"]]) &
                          !is.na(.data[["TransferFromTeamCode"]])),

  "LockedTo72Hrs" = ( (.data[["LockedS1"]] == TRUE) &
                      (.data[["LockedS2"]] == TRUE) &
                      (.data[["LockedS3"]] == TRUE)),

  "72HrDataPresent" = (!is.na(.data[["LockedS1"]]) &
                       !is.na(.data[["LockedS2"]]) &
                       !is.na(.data[["LockedS3"]])),

  "HasDischargeFields" = (!is.na(.data[["LockedS4"]]) &
                          !is.na(.data[["LockedS7"]]) &
                          !is.na(.data[["S7DischargeType"]])),

  "LockedToDischarge" = ( (.data[["LockedS4"]] == TRUE) &
                          (.data[["LockedS7"]] == TRUE)),

  "ExcludeReadmissions" = (.data[["InpatientReadmission"]] == 0),
  
  "HasTeamAndPatientClockStart" = (!is.na(.data[["S1PatientClockStartDateTime"]]) &
                                     !is.na(.data[["S1TeamClockStartDateTime"]])), 

  "First7Days" = (internal_time_difference(
    .data[["S1PatientClockStartDateTime"]],
    .data[["S1TeamClockStartDateTime"]], "days") <= 7)
)
