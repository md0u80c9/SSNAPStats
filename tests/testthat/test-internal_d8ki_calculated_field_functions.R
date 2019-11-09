library(ssnapstats)
context("Check internal_d8ki_calculated_field_functions")

# Check that OT, PT, SLTComm and SLTSwallow give the expected results if >72hrs
# NB That Rehab goals is disabled because this is NA for team-centred results
# and will be needed for patient-centred results only

assessment_times <- tibble::tibble(
  S1PatientClockStartDateTime = as.POSIXct(
    c("2017-01-01 00:00:01",
      "2017-01-01 00:00:01",
      "2017-01-01 00:00:01",
      "2017-01-01 00:00:01"),
    origin = "1970-01-01",
    tz = "UTC"),
  S3OccTherapist72HrsDateTime = as.POSIXct(
    c("2017-01-01 00:06:00",
      "2017-01-01 00:12:00",
      "2017-01-02 00:00:00",
      "2017-01-04 00:01:01"),
    origin = "1970-01-01",
    tz = "UTC"),
  S3Physio72HrsDateTime = as.POSIXct(
    c("2017-01-01 00:06:00",
      "2017-01-01 00:12:00",
      "2017-01-04 02:00:00",
      "2017-01-04 00:01:01"),
    origin = "1970-01-01",
    tz = "UTC"),
  S3SpLangTherapistComm72HrsDateTime = as.POSIXct(
    c("2017-01-04 00:06:00",
      "2017-01-01 00:12:00",
      "2017-01-04 02:00:00",
      "2017-01-04 00:01:01"),
    origin = "1970-01-01",
    tz = "UTC"),
  S3SpLangTherapistSwallow72HrsDateTime = as.POSIXct(
    c("2017-01-01 00:06:00",
      "2017-01-01 00:12:00",
      "2017-01-02 00:00:00",
      "2017-01-04 00:01:01"),
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
    levels = c("", "OR", "PU", "PR", "NK")),
  S3SpLangTherapistSwallow72HrsNotAssessedReason = factor(
    c("", "", "", ""),
    levels = c("", "OR", "PU", "PR", "NK")))

assessment_times <- dplyr::mutate(
  assessment_times,
  !!! internal_d8ki_calculated_field_functions)

test_that("Check S3OTAssessmentWithin72hrs is created correctly", {
  expect_equal(assessment_times[["TCKIOTAssessmentWithin72hrs"]],
               c(TRUE, TRUE, TRUE, FALSE))
})

test_that("Check S3PTAssessmentWithin72hrs is created correctly", {
  expect_equal(assessment_times[["TCKIPTAssessmentWithin72hrs"]],
               c(TRUE, TRUE, FALSE, FALSE))
})

test_that("Check S3SLTCommAssessmentWithin72hrs is created correctly", {
  expect_equal(assessment_times[["TCKISLTCommAssessmentWithin72hrs"]],
               c(FALSE, TRUE, FALSE, FALSE))
})

#test_that("Check S4RehabGoalsWithin5Days is created correctly", {
#  expect_equal(assessment_times$S4RehabGoalsWithin5Days,
#  c(TRUE, TRUE, FALSE, FALSE, FALSE))
#})




# Now create a tibble to contain various exclusions to check exclusions

assessment_times <- tibble::tibble(
  S1PatientClockStartDateTime = as.POSIXct(
    c("2017-01-01 00:00:00",
      "2017-01-01 00:00:00",
      "2017-01-01 00:00:00",
      "2017-01-01 00:00:00",
      "2017-01-01 00:00:00",
      "2017-01-01 00:00:00",
      "2017-01-01 00:00:00"),
    origin = "1970-01-01",
    tz = "UTC"),
  S3OccTherapist72HrsDateTime = as.POSIXct(
    c("2017-01-05 00:00:01",
      "2017-01-05 00:00:01",
      "2017-01-05 00:00:01",
      "2017-01-05 00:00:01",
      "2017-01-05 00:00:01",
      "2017-01-05 00:00:01",
      "2017-01-05 00:00:01"),
    origin = "1970-01-01",
    tz = "UTC"),
  S3Physio72HrsDateTime = as.POSIXct(
    c("2017-01-05 00:00:01",
      "2017-01-05 00:00:01",
      "2017-01-05 00:00:01",
      "2017-01-05 00:00:01",
      "2017-01-05 00:00:01",
      "2017-01-05 00:00:01",
      "2017-01-05 00:00:01"),
    origin = "1970-01-01",
    tz = "UTC"),
  S3SpLangTherapistComm72HrsDateTime = as.POSIXct(
    c("2017-01-05 00:00:01",
      "2017-01-05 00:00:01",
      "2017-01-05 00:00:01",
      "2017-01-05 00:00:01",
      "2017-01-05 00:00:01",
      "2017-01-05 00:00:01",
      "2017-01-05 00:00:01"),
    origin = "1970-01-01",
    tz = "UTC"),
  S3SpLangTherapistSwallow72HrsDateTime = as.POSIXct(
    c("2017-01-05 00:00:01",
      "2017-01-05 00:00:01",
      "2017-01-05 00:00:01",
      "2017-01-05 00:00:01",
      "2017-01-05 00:00:01",
      "2017-01-05 00:00:01",
      "2017-01-05 00:00:01"),
    origin = "1970-01-01",
    tz = "UTC"),
  S3OccTherapist72HrsNotAssessedReason = factor(
    c("", "OR", "PU", "PR", "NK", "ND", ""),
    levels = c("", "OR", "PU", "PR", "NK", "ND")),
  S3Physio72HrsNotAssessedReason = factor(
    c("", "OR", "PU", "PR", "NK", "ND", ""),
    levels = c("", "OR", "PU", "PR", "NK", "ND")),
  S3SpLangTherapistComm72HrsNotAssessedReason = factor(
    c("", "OR", "PU", "PR", "NK", "ND", ""),
    levels = c("", "OR", "PU", "PR", "NK", "ND")),
  S3SpLangTherapistSwallow72HrsNotAssessedReason = factor(
    c("", "ND", "PU", "PR", "PS", "OR", ""),
    levels = c("", "ND", "PU", "PR", "PS", "OR")))

assessment_times <- dplyr::mutate(
  assessment_times,
  !!! internal_d8ki_calculated_field_functions)

test_that("Check S3OTAssessmentWithin72hrs exclusions", {
  expect_equal(assessment_times[["TCKIOTAssessmentWithin72hrs"]],
               c(FALSE, FALSE, NA, NA, FALSE, NA, FALSE))
})

test_that("Check S3PTAssessmentWithin72hrs exclusions", {
  expect_equal(assessment_times[["TCKIPTAssessmentWithin72hrs"]],
               c(FALSE, FALSE, NA, NA, FALSE, NA, FALSE))
})

test_that("Check S3SLTCommAssessmentWithin72hrs exclusions", {
  expect_equal(assessment_times[["TCKISLTCommAssessmentWithin72hrs"]],
               c(FALSE, FALSE, NA, NA, FALSE, NA, FALSE))
})
