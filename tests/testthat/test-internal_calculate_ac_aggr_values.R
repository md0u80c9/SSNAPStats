library(ssnapstats)
context("Check internal_calculate_ac_aggr_values")

# Define a minimal dataset of items to check audit compliance against
ac_times <- tibble::tibble(
  S1PatientClockStartDateTime =
    as.POSIXct(c("2017-01-01 00:00:00", "2017-01-01 00:00:00",
                 "2017-01-01 00:00:00", "2017-01-01 00:00:00",
                 "2017-01-01 00:00:00"),
                 origin = "1970-01-01", tz = "UTC"),
  CreatedDateTime = as.POSIXct(
    c("2017-01-01 02:00:00",
      "2017-01-01 04:00:00",
      "2017-01-01 04:00:00",
      "2017-01-01 08:00:00",
      "2016-01-01 08:00:00"),
    origin = "1970-01-01",
    tz = "UTC"),

  S1Ethnicity =
    c("A", "B", "E", NA, NA),
  S2SwallowScreening4HrsNotPerformedReason =
    c("NK", "PR", "PU", NA, NA),
  S3SwallowScreening72HrsNotPerformedReason =
    c("NK", "PR", "PU", NA, NA),
  S3OccTherapist72HrsNotAssessedReason =
    c("NK", "PR", "PU", NA, NA),
  S3Physio72HrsNotAssessedReason =
    c("NK", "PR", "PU", NA, NA),
  S3SpLangTherapistComm72HrsNotAssessedReason =
    c("NK", "PR", "PU", NA, NA),
  S3SpLangTherapistSwallow72HrsNotAssessedReason =
    c("NK", "PR", "PU", NA, NA),
  ACClockStartToRecordCreationHours  = c(2, 4, 4, 8, NA),
  S2NihssArrival = c(1, 2, 4, 8, NA),
  ACNihssComplete = c(TRUE, FALSE, TRUE, TRUE, TRUE),
  S2Nihss24HrsNK = c(TRUE, TRUE, TRUE, FALSE, TRUE),
  S2Nihss24Hrs = c(1, 2, 4, 8, NA)
)

ac_times <- dplyr::mutate(ac_times,
  reportingMonth = format(
    .data[["S1PatientClockStartDateTime"]], "%m-%Y"))
monthly72hrs <- dplyr::group_by(ac_times, .data[["reportingMonth"]])

kiresults <- dplyr::summarise(
  monthly72hrs,
  !!! internal_aggr_ac_functions_72hrs()
)

test_that("Check percent NIHSS recorded calculated correctly", {
  expect_equal(kiresults[["ACPCAdmissionNIHSSComplete"]], c(80))
})

test_that(
  "Check median clock start to record creation calculation", {
  expect_equal(kiresults[["ACMedianRecordCreationTimeHrs"]], c(4))
})

test_that("Check percent ethnicity recorded calculated correctly", {
  expect_equal(kiresults[["ACPCEthnicityKnown"]], c(60))
})

test_that(
  "Check percent swallow 4hrs recorded calculated correctly", {
  expect_equal(kiresults[["ACPC4HrSwallowComplete"]], c(80))
})

test_that(
  "Check percent swallow 72hrs recorded calculated correctly", {
  expect_equal(kiresults[["ACPC72HrSwallowComplete"]], c(80))
})

test_that(
  "Check percent ot assessment recorded calculated correctly", {
  expect_equal(kiresults[["ACPCOTAssessment72HrsComplete"]], c(80))
})

test_that(
  "Check percent pt assessment recorded calculated correctly", {
  expect_equal(kiresults[["ACPCPTAssessment72HrsComplete"]], c(80))
})

test_that(
  "Check % SLT communication recorded calculated correctly", {
  expect_equal(kiresults[["ACPCSLTCommAssessment72HrsComplete"]],
    c(80))
})

test_that("Check % SLT swallow recorded calculated correctly", {
  expect_equal(kiresults[["ACPCSLTSwallowAssessment72HrsComplete"]],
    c(80))
})
