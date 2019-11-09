library(ssnapstats)
context("Check internal_calculate_d4ki_aggr_values")

# Define a minimalist dataset for the tibble
# Define each so that two pass and two fail (in different patterns)
# For the last value we are proving known failures / unknown values should fail

assessment_times <- tibble::tibble(
  S1PatientClockStartDateTime = as.POSIXct(
    c("2017-01-01 00:00:00",
      "2017-01-01 00:00:00",
      "2017-01-01 00:00:00",
      "2017-01-01 00:00:00",
      "2017-01-02 00:00:00"),
    origin = "1970-01-01",
    tz = "UTC"),
  S3StrokeConsultantAssessedDateTime = as.POSIXct(
    c("2017-01-02 01:00:00",
      "2017-01-01 01:00:00",
      "2017-01-01 01:00:00",
      "2017-01-02 01:00:00",
      "2017-01-01 00:00:00"),
    origin = "1970-01-01",
    tz = "UTC"),
  S3StrokeNurseAssessedDateTime = as.POSIXct(
    c("2017-01-01 01:00:00",
      "2017-01-02 01:00:00",
      "2017-01-02 01:00:00",
      "2017-01-01 01:00:00",
      "2017-01-01 00:00:00"),
    origin = "1970-01-01",
    tz = "UTC"),
  S2SwallowScreening4HrsDateTime = as.POSIXct(
    c("2017-01-01 00:55:00",
      "2017-01-01 06:00:00",
      "2017-01-01 00:25:00",
      "2017-01-01 08:00:00",
      "2017-01-01 00:00:00"),
    origin = "1970-01-01",
    tz = "UTC"),
  S3SpLangTherapistSwallow72HrsDateTime = as.POSIXct(
    c("2017-01-05 01:00:00",
      "2017-01-01 20:00:00",
      "2017-01-04 01:00:00",
      "2017-01-02 20:00:00",
      "2017-01-01 00:00:00"),
    origin = "1970-01-01",
    tz = "UTC"),
  S2SwallowScreening4HrsNotPerformedReason = factor(
    c("", "", "", "", NA),
    levels = c("", "OR", "PU", "PR", "NK")),
  S3SpLangTherapistSwallow72HrsNotAssessedReason = factor(
    c("", "", "", "", NA),
    levels = c("", "PS", "OR", "PU", "PR", "NK")))

assessment_times <- dplyr::mutate(
  assessment_times,
  !!! internal_d4ki_calculated_field_functions)

# Then create monthly tallies and group the data (NB we only have one group in
# our sample, but this imitates the domain calculations)
assessment_times <- dplyr::mutate(
  assessment_times,
  reportingMonth = format(.data[["S1PatientClockStartDateTime"]],
    "%m-%Y"))
  monthly72hrs <- dplyr::group_by(assessment_times,
    .data[["reportingMonth"]])
  kiresults <- dplyr::summarise(monthly72hrs,
  !!! internal_d4ki_aggr_value_functions("TC")
)

test_that("Check % seen by a consultant in 24hrs calculation", {
  expect_equal(kiresults[["TCKIPCConsultant24Hrs"]], c(60))
})

test_that("Check median consultant time calculated correctly", {
  expect_equal(kiresults[["TCKIMedianConsultantTime"]], c(60))
})

test_that("Check percent seen by a stroke nurse in 24hrs calculated
          correctly", {
  expect_equal(kiresults[["TCKIPCStrokeNurse24Hrs"]], c(60))
})

test_that("Check Median stroke nurse time calculated correctly", {
  expect_equal(kiresults[["TCKIMedianStrokeNurseTime"]], c(60))
})

test_that("Check % swallow screened in 4hrs calculated correctly", {
  expect_equal(kiresults[["TCKIPCSwallowScreen4Hrs"]], c(60))
})

test_that("Check % formal swallow screened in 72hrs calculated correctly", {
  expect_equal(kiresults[["TCKIPCSwallowAssessment72Hrs"]], c(60))
})
