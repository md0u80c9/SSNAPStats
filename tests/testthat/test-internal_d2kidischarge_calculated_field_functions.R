library(ssnapstats)
context("Check internal_d2kidischarge_calculated_field_functions")

# Define a minimalist dataset for the tibble (we only need
# S2ClockStartToBrainImagingMins)
# We set one less than 60 minutes, 1 under 60 minutes but less than
# 12 hours, and two over 12 hours.
# The last patient has no time (presumably as not scanned) so has
# an NA.

length_of_stay <- tibble::tibble(
  S7TeamClockStopDateTime = as.POSIXct(
    c("2017-01-01 00:55:00",
      "2017-01-01 01:30:00",
      "2017-01-01 14:31:00",
      "2017-01-02 02:01:00"),
    origin = "1970-01-01",
    tz = "UTC"),
  S1TeamClockStartDateTime = as.POSIXct(
    c("2017-01-01 00:00:00",
      "2017-01-01 00:00:00",
      "2017-01-01 00:00:00",
      "2017-01-01 00:00:00"),
    origin = "1970-01-01",
    tz = "UTC"),
  S4ArrivalDateTime = as.POSIXct(
    c("2017-01-01 00:00:00",
      "2017-01-01 00:00:00",
      "2017-01-01 00:00:00",
      "2017-01-01 00:00:00"),
    origin = "1970-01-01",
    tz = "UTC"),
  S4FirstWard = c("SU", "SU", "SU", "SU"),
  S4StrokeUnitArrivalDateTime = as.POSIXct(
    c("2017-01-01 00:00:00",
      "2017-01-01 00:00:00",
      "2017-01-01 00:00:00",
      "2017-01-01 00:00:00"),
    origin = "1970-01-01",
    tz = "UTC"),
  S7StrokeUnitDischargeDateTime = as.POSIXct(
    c("2017-01-01 00:55:00",
      "2017-01-01 01:30:00",
      "2017-01-01 14:31:00",
      "2017-01-02 02:01:00"),
    origin = "1970-01-01",
    tz = "UTC"),
  S7DeathDate = c(NA, NA, NA, NA))

length_of_stay <- dplyr::mutate(
  length_of_stay,
  !!! internal_d2discharge_calculated_field_functions)

test_that("Check S7length_of_stayAtTeamMins is created correctly", {
  expect_equal(length_of_stay[["S7length_of_stayAtTeamMins"]],
    c(55, 90, 871, 1561))
})
