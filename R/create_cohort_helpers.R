#' Return first or latest team in a SSNAP patient episode
#'
#' These functions group the data by \code{PatientId}, and then
#' slices by either the lowest (first) or highest (last)
#' \code{ProClinV1Id}, returning either the first or the latest
#' record of a patient spell.
#'
#' It is designed for the SSNAP dataset in the sense that it uses
#' \code{ProClinV1Id} and \code{PatientId} columns, and assumes a
#' similar record passing model - but could have more generic uses.
#' 
#' @param source_data A tibble of SSNAP data
#' @return the data will be grouped by \code{PatientId}, sliced to
#' return one record for each patient spell, and then ungrouped.
#' @export

cohort_slice_first_team <- function(source_data) {
  output_cohort <- dplyr::group_by(
    source_data,
    .data[["PatientId"]])
  output_cohort <- dplyr::slice(
    output_cohort,
    which.min(.data[["ProClinV1Id"]]))
  dplyr::ungroup(output_cohort)
}

#' @export
#' @rdname cohort_slice_first_team

cohort_slice_last_team <- function(source_data) {
  output_cohort <- dplyr::group_by(
    source_data,
    .data[["PatientId"]])
  output_cohort <- dplyr::slice(
    output_cohort,
    which.max(.data[["ProClinV1Id"]]))
  dplyr::ungroup(output_cohort)
}
