library(ssnapstats)
context("Check internal_d2ki72hrs_calculated_field_functions")

test_that("Check routine SU admission within 4hrs works", {
  sutimes <- tibble::tibble(
    S1FirstStrokeUnitArrivalDateTime = as.POSIXct(
      c("2017-01-01 01:00:00",
        "2017-01-01 02:00:00",
        "2017-01-01 03:00:00",
        "2017-01-01 04:00:00",
        "2017-01-01 05:00:00"),
      origin = "1970-01-01",
      tz = "UTC"),
    S1PatientClockStartDateTime = as.POSIXct(
      c("2017-01-01 00:00:00",
        "2017-01-01 00:00:00",
        "2017-01-01 00:00:00",
        "2017-01-01 00:00:00",
        "2017-01-01 00:00:00"),
      origin = "1970-01-01",
      tz = "UTC"),
    S1FirstWard = factor(c( "SU", "SU", "SU", "SU", "SU"),
                         levels = c("ICH", "SU", "MAC", "O")),
    S2IAI = c(FALSE, FALSE, FALSE, FALSE, FALSE))

  sutimes <- dplyr::mutate(sutimes,
    !!! internal_d2ki72hrs_calculated_field_functions)

  expect_equal(sutimes[["TCKIFirstStrokeUnitWithin4hrs"]],
               c(TRUE, TRUE, TRUE, TRUE, FALSE))
  expect_equal(sutimes[["TCKIClockStartToFirstStrokeUnitMins"]],
               c(60L, 120L, 180L, 240L, 300L))
})

test_that("Check 4hr stroke unit exclusions/fails due to ward
          work", {
  sutimes <- tibble::tibble(
    S1FirstStrokeUnitArrivalDateTime = as.POSIXct(
      c("2017-01-01 01:00:00",
        "2017-01-01 01:00:00",
        "2017-01-01 01:00:00",
        "2017-01-01 01:00:00"),
      origin = "1970-01-01", tz = "UTC"),
    S1PatientClockStartDateTime = as.POSIXct(
      c("2017-01-01 00:00:00",
        "2017-01-01 00:00:00",
        "2017-01-01 00:00:00",
        "2017-01-01 00:00:00"),
      origin = "1970-01-01", tz = "UTC"),
    S1FirstWard = factor(c( "ICH",  "O", "SU", "MAC"),
      levels = c("ICH", "SU", "MAC", "O")),
    S2IAI = c(FALSE, FALSE, FALSE, FALSE))

  sutimes <- dplyr::mutate(sutimes,
    !!! internal_d2ki72hrs_calculated_field_functions)

  expect_equal(sutimes[["TCKIFirstStrokeUnitWithin4hrs"]],
               c(NA, FALSE, TRUE, FALSE))
  # Medians aren't affected by the exclusions
  expect_equal(sutimes[["TCKIClockStartToFirstStrokeUnitMins"]],
    c(60L, 60L, 60L, 60L))
})


test_that("Check 4hr stroke unit exclusions/fails due to IAI work", {
  sutimes <- tibble::tibble(
    S1FirstStrokeUnitArrivalDateTime = as.POSIXct(
      c("2017-01-01 01:00:00",
        "2017-01-01 01:00:00",
        "2017-01-01 06:00:00",
        "2017-01-01 06:00:00"),
      origin = "1970-01-01",
      tz = "UTC"),
    S1PatientClockStartDateTime = as.POSIXct(
      c("2017-01-01 00:00:00",
        "2017-01-01 00:00:00",
        "2017-01-01 00:00:00",
        "2017-01-01 00:00:00"),
      origin = "1970-01-01",
      tz = "UTC"),
    S1FirstWard = factor(c( "SU", "SU", "SU", "SU"),
      levels = c("ICH", "SU", "MAC", "O")),
    S2IAI = c(FALSE, TRUE, FALSE, TRUE))

  sutimes <- dplyr::mutate(sutimes,
    !!! internal_d2ki72hrs_calculated_field_functions)

  expect_equal(sutimes[["TCKIFirstStrokeUnitWithin4hrs"]],
               c(TRUE, NA, FALSE, NA))
  # Medians aren't affected by the exclusions
  expect_equal(sutimes[["TCKIClockStartToFirstStrokeUnitMins"]],
               c(60L, 60L, 360L, 360L))
})
