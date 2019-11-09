library(ssnapstats)
context("Check internal_d1ki_calculated_field_functions")


# Define a minimalist dataset for the tibble (we only need
# S2ClockStartToBrainImagingMins)
# We set one less than 60 minutes, 1 under 60 minutes but less than 12 hours,
# and two over 12 hours.
# The last patient has no time (presumably as not scanned) so has an NA.

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

scan_times <- dplyr::mutate(scan_times,
  !!! internal_d1ki_calculated_field_functions)

test_that("Check S2BrainImagingWithin1hr is created correctly", {
  expect_equal(scan_times[["TCKIBrainImagingWithin1hr"]],
               c(TRUE, FALSE, FALSE, FALSE, TRUE))
})

test_that("Check S2BrainImagingWithin12hrs is created correctly", {
  expect_equal(scan_times[["TCKIBrainImagingWithin12hrs"]],
    c(TRUE, TRUE, FALSE, FALSE, TRUE))
})
