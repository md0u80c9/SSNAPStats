library(ssnapstats)
context("Check internal_d4ki_calculated_field_functions")

# Define a minimalist dataset for the tibble
# Define each so that two pass and two fail (in different patterns)
# For the last value we are proving known failures / unknown values should fail

assessment_times <- tibble::tibble(
  S1PatientClockStartDateTime = as.POSIXct(
    c("2017-01-01 00:00:00",
      "2017-01-01 00:00:00",
      "2017-01-01 00:00:00",
      "2017-01-01 00:00:00"),
    origin = "1970-01-01",
    tz = "UTC"),
  S3StrokeConsultantAssessedDateTime = as.POSIXct(
    c("2017-01-02 01:00:00",
      "2017-01-01 01:00:00",
      "2017-01-01 01:00:00",
      "2017-01-02 01:00:00"),
    origin = "1970-01-01",
    tz = "UTC"),
  S3StrokeNurseAssessedDateTime = as.POSIXct(
    c("2017-01-01 01:00:00",
      "2017-01-02 01:00:00",
      "2017-01-02 01:00:00",
      "2017-01-01 01:00:00"),
    origin = "1970-01-01",
    tz = "UTC"),
  S2SwallowScreening4HrsDateTime = as.POSIXct(
    c("2017-01-01 00:55:00",
      "2017-01-01 06:00:00",
      "2017-01-01 00:25:00",
      "2017-01-01 08:00:00"),
    origin = "1970-01-01",
    tz = "UTC"),
  S3SpLangTherapistSwallow72HrsDateTime = as.POSIXct(
    c("2017-01-05 01:00:00",
      "2017-01-01 00:20:00",
      "2017-01-04 01:00:00",
      "2017-01-02 20:00:00"),
    origin = "1970-01-01",
    tz = "UTC"),
  S2SwallowScreening4HrsNotPerformedReason = factor(
    c("", "", "", ""),
    levels = c("", "OR", "PU", "PR", "NK")),
  S3SpLangTherapistSwallow72HrsNotAssessedReason = factor(
    c("", "", "", ""),
    levels = c("", "PS", "OR", "PU", "PR", "NK")))

assessment_times <- dplyr::mutate(assessment_times,
  !!! internal_d4ki_calculated_field_functions)

test_that("Check KIConsultantWithin24hrs is created correctly", {
  expect_equal(assessment_times[["TCKIConsultantWithin24hrs"]],
    c(FALSE, TRUE, TRUE, FALSE))
})

test_that("Check KIStrokeNurseWithin24hrs is created correctly", {
  expect_equal(assessment_times[["TCKIStrokeNurseWithin24hrs"]],
    c(TRUE, FALSE, FALSE, TRUE))
})

test_that("Check KISwallowScreenWithin4hrs is created correctly", {
  expect_equal(assessment_times[["TCKISwallowScreenWithin4hrs"]],
    c(TRUE, FALSE, TRUE, FALSE))
})


test_that("Check KISwallowAssessmentWithin72Hrs is created", {
  expect_equal(assessment_times[["TCKISwallowAssessmentWithin72Hrs"]],
    c(FALSE, TRUE, FALSE, TRUE))
})


# Now amend the tibble to contain various exclusions to check exclusions apply
# appropriately

exclusion_times <- tibble::tibble(
  S1PatientClockStartDateTime = as.POSIXct(
    c("2017-01-02 00:00:00",
      "2017-01-02 00:00:00",
      "2017-01-02 00:00:00",
      "2017-01-02 00:00:00"),
    origin = "1970-01-01",
    tz = "UTC"),
  S3StrokeConsultantAssessedDateTime = as.POSIXct(
    c("2017-01-03 01:00:00",
      "2017-01-02 01:00:00",
      "2017-01-02 01:00:00",
      "2017-01-03 01:00:00"),
    origin = "1970-01-01",
    tz = "UTC"),
  S3StrokeNurseAssessedDateTime = as.POSIXct(
    c("2017-01-02 01:00:00",
      "2017-01-03 01:00:00",
      "2017-01-03 01:00:00",
      "2017-01-02 01:00:00"),
    origin = "1970-01-01",
    tz = "UTC"),
  S2SwallowScreening4HrsDateTime = as.POSIXct(
    c("2017-01-03 01:00:00",
      "2017-01-03 01:00:00",
      "2017-01-03 01:00:00",
      "2017-01-03 01:00:00"),
    origin = "1970-01-01",
    tz = "UTC"),
  S3SpLangTherapistSwallow72HrsDateTime = as.POSIXct(
    c("2017-01-05 01:00:00",
      "2017-01-05 01:00:00",
      "2017-01-05 01:00:00",
      "2017-01-05 01:00:00"),
    origin = "1970-01-01",
    tz = "UTC"),
  S2SwallowScreening4HrsNotPerformedReason = factor(
    c("OR", "PU", "PR", "NK"),
    levels = c("", "OR", "PU", "PR", "NK")),
  S3SpLangTherapistSwallow72HrsNotAssessedReason = factor(
    c("PS", "OR", "PU", "PR"),
    levels = c("", "PS", "OR", "PU", "PR", "NK")))

exclusion_times <- dplyr::mutate(
  exclusion_times,
  !!! internal_d4ki_calculated_field_functions)

test_that("Check TCKISwallowScreenWithin4hrs exclusions work", {
  expect_equal(exclusion_times[["TCKISwallowScreenWithin4hrs"]],
    c(FALSE, NA, NA, FALSE))
})

test_that("Check TCKISwallowAssessmentWithin72Hrs exclusions work", {
  expect_equal(exclusion_times[["TCKISwallowAssessmentWithin72Hrs"]],
    c(NA, FALSE, NA, NA))
})
