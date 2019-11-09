library(ssnapinterface)
library(ssnapstats)
context("Check internal_calculate_d3ki_aggr_values")

# Define a minimalist dataset for the tibble (we only need
# S2ClockStartToThrombolysisMins and S2Thrombolysis)
# We set one less than 60 minutes, 1 under 60 minutes but less than
# 12 hours, and two over 12 hours.
# The last patient has no time (presumably as not scanned) so has an
# NA.

tpa_times <- tibble::tibble(
  S1AgeOnArrival = c(75, 75, 85, 75),
  S1OnsetInHospital = c(FALSE, FALSE, FALSE, FALSE),
  S1OnsetDateTime = as.POSIXct(c("2017-01-01 22:00:00",
                                 "2017-01-01 21:30:00",
                                 "2017-01-01 21:00:00",
                                 "2017-01-01 22:00:00"),
                               origin = "1970-01-01",
                               tz = "UTC"),
  S1PatientClockStartDateTime = as.POSIXct(c("2017-01-02 00:00:00",
                                             "2017-01-02 00:00:00",
                                             "2017-01-02 00:00:00",
                                             "2017-01-02 00:00:00"),
                                           origin = "1970-01-01",
                                           tz = "UTC"),
  S2ThrombolysisDateTime = as.POSIXct(
    c("2017-01-02 00:30:00", "2017-01-02 01:30:00", NA, NA),
    origin = "1970-01-01", tz = "UTC"),
  S1OnsetDateIsPrecise = c(TRUE, TRUE, TRUE, TRUE),
  S1OnsetTimeIsPrecise = c(TRUE, TRUE, TRUE, TRUE),
  S2NihssArrival = c(10, 12, 14, 12),
  S2Thrombolysis = factor(c("Y", "Y", "NB", "N"),
                          levels = c("Y", "N", "NB")),
  S2ThrombolysisNoBut = c(
    0, 0, ssnapinterface::tpa_no_but["TimeWindow"], 0),
  S1FirstWard = c("SU", "SU", "SU", "SU"),
  S2IAI = c(FALSE, FALSE, FALSE, FALSE),
  TCKIFirstStrokeUnitWithin4hrs = c(FALSE, TRUE, FALSE, TRUE)
)

tpa_times <- dplyr::mutate(
  tpa_times,
  !!! internal_d3ki_calculated_field_functions)

# Then create monthly tallies and group the data (NB we only have one
# group in our sample, but this imitates the domain calculations)
tpa_times <- dplyr::mutate(tpa_times,
  reportingMonth = format(.data[["S1PatientClockStartDateTime"]],
  "%m-%Y"))
monthly72hrs <- dplyr::group_by(tpa_times, .data[["reportingMonth"]])

kiresults <- dplyr::summarise(monthly72hrs,
  !!! internal_d3ki_aggr_value_functions("TC")
)

test_that("Check percent thrombolysed calculated correctly (2/4)", {
  expect_equal(kiresults[["TCKIPCtPA"]], c(50))
})

test_that("Check percent meeting RCP criteria calculated correctly", {
  expect_equal(kiresults[["TCKIPCtPAMeetsRCPCriteria"]], c(66.7))
})

test_that("Check median door-to-needle time calculated correctly", {
  expect_equal(kiresults[["TCKIMediantPATime"]], c(60))
})

test_that("Check percent meeting RCP criteria and admitted within
          4hrs calculated correctly", {
  expect_equal(kiresults[["TCKItPAAndSUIn4hrs"]], c(25))
})

test_that("Check percent thrombolysed within 1hr calculated
          correctly (1/2)", {
  expect_equal(kiresults[["TCKIPCtPAIn1Hr"]], c(50))
})
