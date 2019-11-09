library(ssnapstats)
context("Check internal_calculate_d1ki_aggr_values")

# Define a minimalist dataset for the tibble (we only need 
# S2ClockStartToBrainImagingMins)
# We set one less than 60 minutes, 1 under 60 minutes but less than
# 12 hours, and two over 12 hours.
# The last patient has a clock start after the imaging time so should
# be set as 1 minute (the actual dataset prevents this).

scan_times <- tibble::tibble(
  S2BrainImagingDateTime = as.POSIXct(
    c("2017-01-01 00:55:00",
      "2017-01-01 01:30:00",
      "2017-01-01 14:31:00",
      "2017-01-02 02:01:00",
      "2017-01-02 02:31:00"),
    origin = "1970-01-01",
    tz = "UTC"),
  S1PatientClockStartDateTime = as.POSIXct(
    c("2017-01-01 00:00:00",
      "2017-01-01 00:00:00",
      "2017-01-01 00:00:00",
      "2017-01-01 00:00:00",
      "2017-01-03 00:01:00"),
    origin = "1970-01-01",
    tz = "UTC"))

# Create the KI calculated fields

scan_times <- dplyr::mutate(
  scan_times,
  !!! internal_d1ki_calculated_field_functions)

# Then create monthly tallies and group the data (NB we only have one
# group in our sample, but this imitates the domain calculations)

scan_times <- dplyr::mutate(scan_times,
  reportingMonth = format(.data[["S1PatientClockStartDateTime"]],
    "%m-%Y"))
monthly72hrs <- dplyr::group_by(scan_times, .data[["reportingMonth"]])

kiresults <- dplyr::summarise(
  monthly72hrs,
  !!! internal_d1ki_aggr_value_functions("TC")
)

test_that("Check Median brain imaging time calculated correctly", {
  expect_equal(kiresults[["TCKIMedianBrainImagingTime"]], 90)
})

test_that("Check percent scanned in 1 hour calculated correctly", {
  expect_equal(kiresults[["TCKIPCScannedIn1Hr"]], c(40))
})

test_that("Check percent scanned in 12 hours calculated correctly", {
  expect_equal(kiresults[["TCKIPCScannedIn12Hrs"]], c(60))
})
