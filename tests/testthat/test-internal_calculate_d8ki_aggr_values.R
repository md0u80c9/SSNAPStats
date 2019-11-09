library(ssnapstats)
context("Check internal_calculate_d8ki_aggr_values")

# Define a minimalist dataset for the tibble
# Define each so that two pass and two fail (in different patterns)
# For the last value we are proving known failures / unknown values
# should fail

assessment_times <- tibble::tibble(
  S1PatientClockStartDateTime = as.POSIXct(
    c("2017-01-01 00:00:00",
      "2017-01-01 00:00:00",
      "2017-01-01 00:00:00",
      "2017-01-01 00:00:00"),
    origin = "1970-01-01",
    tz = "UTC"),
  S3OccTherapist72HrsDateTime = as.POSIXct(
    c("2017-01-01 00:06:00",
      "2017-01-01 00:12:00",
      "2017-01-02 00:00:00",
      "2017-01-05 00:00:00"),
    origin = "1970-01-01",
    tz = "UTC"),
  S3Physio72HrsDateTime = as.POSIXct(
    c("2017-01-01 00:06:00",
      "2017-01-02 00:12:00",
      "2017-01-05 00:00:00",
      "2017-01-05 00:00:00"),
    origin = "1970-01-01",
    tz = "UTC"),
  S3SpLangTherapistComm72HrsDateTime = as.POSIXct(
    c("2017-01-01 00:06:00",
      "2017-01-03 00:00:00",
      "2017-01-03 00:00:00",
      "2017-01-03 00:00:00"),
    origin = "1970-01-01",
    tz = "UTC"),
  S3OccTherapist72HrsNotAssessedReason = factor(
    c("", "", "", ""),
    levels = c("", "OR", "PU", "PR", "NK")),
  S3Physio72HrsNotAssessedReason = factor(
    c("", "", "", ""),
    levels = c("", "OR", "PU", "PR", "NK")),
  S3SpLangTherapistComm72HrsNotAssessedReason = factor(
    c("", "", "", ""),
    levels = c("", "OR", "PU", "PR", "NK")))
# S4ClockStartToRehabGoalsDays = c((74 * 60), (74 * 60), (74 * 60), (74 * 60),
#                                  (74 * 60), (74 * 60), (74 * 60)),
# S4RehabGoalsNoneReason = factor(
#   c("","OR", "PR", "MU", "NI", "NRP", "NK"),
#   levels=c("","OR", "PR", "MU", "NI", "NRP", "NK")))

assessment_times <- dplyr::mutate(
  assessment_times,
  !!! internal_d8ki_calculated_field_functions)

# Then create monthly tallies and group the data (NB we only have one group
# in our sample, but this imitates the domain calculations)
assessment_times <- dplyr::mutate(
  assessment_times,
  reportingMonth = format(.data[["S1PatientClockStartDateTime"]],
    "%m-%Y"))
monthly72hrs <- dplyr::group_by(assessment_times,
    .data[["reportingMonth"]])
kiresults <- dplyr::summarise(
  monthly72hrs,
  !!! internal_d8ki_aggr_value_functions("TC")
)

test_that("Check % seen by an OT in 72hrs calculated correctly", {
  expect_equal(kiresults[["TCKIPCOT72Hrs"]], c(75))
})

test_that("Check median OT time calculated correctly", {
  expect_equal(kiresults[["TCKIMedianOTTime"]], c(726))
})

test_that("Check % seen by a PT in 72hrs calculated correctly", {
  expect_equal(kiresults[["TCKIPCPT72Hrs"]], c(50))
})

test_that("Check median PT time calculated correctly", {
  expect_equal(kiresults[["TCKIMedianPTTime"]], c(3606))
})

test_that("Check % seen by SLT (Comms) in 72hrs", {
  expect_equal(kiresults[["TCKIPCSLTComm72Hrs"]], c(100))
})

test_that("Check median SLT (comms) time calculated correctly", {
  expect_equal(kiresults[["TCKIMedianSLTCommTime"]], c(2880))
})
