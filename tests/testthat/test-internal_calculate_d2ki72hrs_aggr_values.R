library(ssnapstats)
context("Check internal_calculate_d2ki72hrs_aggr_values")

# Create the KI computed variables
su_times <- tibble::tibble(
  S1FirstStrokeUnitArrivalDateTime = c(as.POSIXct(
    c("2017-01-01 01:00:00",
      "2017-01-01 02:00:00",
      "2017-01-01 03:00:00",
      "2017-01-01 04:00:00",
      "2017-01-01 05:00:00"),
    origin = "1970-01-01",
    tz = "UTC"), NA),
  S1PatientClockStartDateTime = as.POSIXct(
    c("2017-01-01 00:00:00",
      "2017-01-01 00:00:00",
      "2017-01-01 00:00:00",
      "2017-01-01 00:00:00",
      "2017-01-01 00:00:00",
      "2017-01-01 00:00:00"),
    origin = "1970-01-01",
    tz = "UTC"),
  S1FirstWard = factor(c( "SU", "SU", "SU", "SU", "SU", "O"),
    levels = c("ICH", "SU", "MAC", "O")),
  S2IAI = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))

su_times <- dplyr::mutate(
  su_times,
  !!! internal_d2ki72hrs_calculated_field_functions)

# Then create monthly tallies and group the data (NB we only have
# one group in our sample, but this imitates the domain calculations)

su_times <- dplyr::mutate(
  su_times,
  reportingMonth = format(.data[["S1PatientClockStartDateTime"]],
    "%m-%Y"))
monthly72hrs <- dplyr::group_by(su_times,
                                .data[["reportingMonth"]])
kiresults <- dplyr::summarise(
  monthly72hrs,
  !!! internal_d2ki72hrs_aggr_value_functions("TC")
)

test_that("Check Median first stroke unit time calculated
          correctly", {
  expect_equal(kiresults[["TCKIMedianFirstSUTime"]], c(180))
})

test_that("Check percent admitted to a stroke unit within 4 hrs
          calculated correctly", {
  expect_equal(kiresults[["TCKIPCFirstSUIn4Hrs"]], c(66.7))
})
