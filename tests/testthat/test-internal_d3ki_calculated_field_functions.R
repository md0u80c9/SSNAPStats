library(ssnapstats)
context("Check internal_d3ki_calculated_field_functions")

# FIXME test_that 2.0.0 can't cope with named vectors in the answer
# so we have to hard-code the thrombolysis bitfield. I have left the
# named values commented out - if/when this is fixed we can switch
# to named values but watch for issues if this bitfield changes in
# SSNAPInterface as the unit tests in here will fail!

# Define a minimalist dataset for the tibble (we only need
# S2ClockStartToThrombolysisMins, the rest are required elements
# for the whole domain)
# We set one less than 60 minutes, and one over 60 minutes
# The last patient has no time (presumably as not given tPA) so has
# an NA.
test_that("Check KItPAWithin1hr is created correctly", {
  tpa_times <- tibble::tibble(
    S1AgeOnArrival = c(75, 82, 75),
    S1OnsetInHospital = c(FALSE, FALSE, FALSE),
    S1OnsetDateIsPrecise = c(TRUE, TRUE, TRUE),
    S1OnsetTimeIsPrecise = c(TRUE, TRUE, TRUE),
    S2NihssArrival = c(10, 12, 14),
    S2Thrombolysis = factor(c("Y", "Y", "N"),
                            levels = c("Y", "N", "NB")),
    S1OnsetDateTime = as.POSIXct(
      c("2017-01-01 22:00:00",
        "2017-01-01 21:30:00",
        "2017-01-01 21:00:00"),
      origin = "1970-01-01",
      tz = "UTC"),
    S1PatientClockStartDateTime = as.POSIXct(
      c("2017-01-02 00:00:00",
        "2017-01-02 00:00:00",
        "2017-01-02 00:00:00"),
      origin = "1970-01-01",
      tz = "UTC"),
    S2ThrombolysisDateTime = as.POSIXct(
      c("2017-01-02 00:55:00",
        "2017-01-02 01:30:00",
        "2017-01-01 05:00:00"),
      origin = "1970-01-01",
      tz = "UTC"),
    S2ThrombolysisNoBut = c(0, 0, 0),
    S1FirstWard = c("SU", "SU", "SU"),
    S2IAI = c(FALSE, FALSE, FALSE),
    TCKIFirstStrokeUnitWithin4hrs = c(120, 180, 300)
  )
  tpa_times <- dplyr::mutate(
    tpa_times,
    !!! internal_d3ki_calculated_field_functions)

  expect_equal(tpa_times[["TCKItPAWithin1hr"]], c(TRUE, FALSE, NA))
})

# Now test the inconsistency rules function correctly

# Patient 1 has a precise onset time the no and false for no tPA as
# 'onset time unknown' (ie. consistent)
# Patient 2 has no precise onset and is 'onset time unknown' (ie.
# consistent) Patient 3 has a precise onset but the no but time is
# 'onset unknown' (ie. inconsistent)
test_that(glue::glue(
  "Check inconsistency rules - precise onset time with the tPA no ",
  "but being onset time unknown"), {
  tpa_times <- tibble::tibble(
    S1AgeOnArrival = c(75, 82, 75),
    S1OnsetInHospital = c(FALSE, FALSE, FALSE),
    S1OnsetDateIsPrecise = c(TRUE, NA, TRUE),
    S1OnsetTimeIsPrecise = c(TRUE, FALSE, TRUE),
    S2NihssArrival = c(10, 12, 14),
    S2Thrombolysis = factor(c("NB", "NB", "NB"),
      levels = c("Y", "N", "NB")),
    S1OnsetDateTime = as.POSIXct(
      c("2017-01-01 22:00:00",
        "2017-01-01 21:30:00",
        "2017-01-01 21:00:00"),
      origin = "1970-01-01",
      tz = "UTC"),
    S1PatientClockStartDateTime = as.POSIXct(
      c("2017-01-02 00:00:00",
        "2017-01-02 00:00:00",
        "2017-01-02 00:00:00"),
      origin = "1970-01-01",
      tz = "UTC"),
    S2ThrombolysisDateTime = as.POSIXct(
      c("2017-01-02 00:55:00",
        "2017-01-02 01:30:00",
        "2017-01-01 05:00:00"),
      origin = "1970-01-01",
      tz = "UTC"),
    S2ThrombolysisNoBut = c(0,
      tpa_no_but["TimeUnknownWakeUp"],
      tpa_no_but["TimeUnknownWakeUp"]),
    S1FirstWard = c("SU", "SU", "SU"),
    S2IAI = c(FALSE, FALSE, FALSE),
    TCKIFirstStrokeUnitWithin4hrs = c(120, 180, 300)
  )
  tpa_times <- dplyr::mutate(tpa_times,
    !!! internal_d3ki_calculated_field_functions)

  expect_equal(tpa_times[["S2ThrombolysisNoButConsistent"]],
    c(0,
      256, # tpa_no_but is TimeUnknownWakeUp
      0))
})

# Patient 1 has age under 80 and 'No But age' is not selected.
# Patient 2 has age over 80 and 'No But age' is selected.
# Patient 3 has age under 80 and 'No But age' is selected
# (inconsistent).
test_that("Check inconsistency rules - age over 80 but the NB is
          because of age.", {
  tpa_times <- tibble::tibble(
    S1AgeOnArrival = c(75, 82, 75),
    S1OnsetInHospital = c(FALSE, FALSE, FALSE),
    S1OnsetDateIsPrecise = c(TRUE, TRUE, TRUE),
    S1OnsetTimeIsPrecise = c(TRUE, FALSE, TRUE),
    S2NihssArrival = c(10, 12, 14),
    S1OnsetDateTime = as.POSIXct(
      c("2017-01-01 22:00:00",
        "2017-01-01 21:30:00",
        "2017-01-01 21:00:00"),
      origin = "1970-01-01",
      tz = "UTC"),
    S1PatientClockStartDateTime = as.POSIXct(
      c("2017-01-02 00:00:00",
        "2017-01-02 00:00:00",
        "2017-01-02 00:00:00"),
      origin = "1970-01-01",
      tz = "UTC"),
    S2ThrombolysisDateTime = as.POSIXct(
      c("2017-01-02 00:55:00",
        "2017-01-02 01:30:00",
        "2017-01-01 05:00:00"),
      origin = "1970-01-01",
      tz = "UTC"),
    S2Thrombolysis = factor(c("NB", "NB", "NB"),
      levels = c("Y", "N", "NB")),
    S2ClockStartToThrombolysisMins = c(55, 90, NA),
    S2ThrombolysisNoBut = c(0,
                            tpa_no_but["Age"],
                            tpa_no_but["Age"]),
    S1FirstWard = c("SU", "SU", "SU"),
    S2IAI = c(FALSE, FALSE, FALSE),
    TCKIFirstStrokeUnitWithin4hrs = c(120, 180, 300)
  )
  tpa_times <- dplyr::mutate(tpa_times,
    !!! internal_d3ki_calculated_field_functions)

  expect_equal(tpa_times[["S2ThrombolysisNoButConsistent"]],
    c(0,
      64, # tpa_no_but is Age
      0))
})

# Patient 1 has Age over 80, onset of 3 hrs and time window given as
# reason.
# Patient 2 has Age under 80, onset over 3.5 hrs and time window
# given as reason
# Patient 3 has Age under 80, onset under 3.5hrs but imprecise and
# time window given as reason.
# Patient 4 has Age under 80, onset under 3.5hrs and time window
# given as
# reason (inconsistent).
test_that("Check inconsistency rules - onset under 3.5hrs, age under
          80, time window given as reason.", {
  tpa_times <- tibble::tibble(
    S1AgeOnArrival = c(85, 75, 75, 75),
    S1OnsetInHospital = c(FALSE, FALSE, FALSE, FALSE),
    S1OnsetToClockStartMins = c(180, 240, 180, 180),
    S1OnsetDateIsPrecise = c(TRUE, TRUE, TRUE, TRUE),
    S1OnsetTimeIsPrecise = c(TRUE, TRUE, FALSE, TRUE),
    S2NihssArrival = c(10, 3, 10, 14),
    S1OnsetDateTime = as.POSIXct(
      c("2017-01-01 21:00:00",
        "2017-01-01 20:00:00",
        "2017-01-01 21:00:00",
        "2017-01-01 21:00:00"),
      origin = "1970-01-01",
      tz = "UTC"),
    S1PatientClockStartDateTime = as.POSIXct(
      c("2017-01-02 00:00:00",
        "2017-01-02 00:00:00",
        "2017-01-02 00:00:00",
        "2017-01-02 00:00:00"),
      origin = "1970-01-01",
      tz = "UTC"),
    S2ThrombolysisDateTime = as.POSIXct(
      c("2017-01-02 00:55:00",
        "2017-01-02 01:30:00",
        "2017-01-02 01:30:00",
        "2017-01-02 05:00:00"),
      origin = "1970-01-01",
      tz = "UTC"),
    S2Thrombolysis = factor(c("NB", "NB", "NB", "NB"),
      levels = c("Y", "N", "NB")),
    S2ThrombolysisNoBut = c(0,
                            tpa_no_but["TimeWindow"],
                            tpa_no_but["TimeWindow"],
                            tpa_no_but["TimeWindow"]),
    S1FirstWard = c("SU", "SU", "SU", "SU"),
    S2IAI = c(FALSE, FALSE, FALSE, FALSE),
    TCKIFirstStrokeUnitWithin4hrs = c(120, 180, 300, 360)
  )
  tpa_times <- dplyr::mutate(tpa_times,
    !!! internal_d3ki_calculated_field_functions)

  expect_equal(tpa_times[["S2ThrombolysisNoButConsistent"]],
    c(0,
      512, # tpa_no_but is TimeWindow
      512, # tpa_no_but is TimeWindow
      0))
})

# Patient 1 has NIHSS 4 or more and 'No But Severity' is not selected.
# Patient 2 has NIHSS under 4 and 'No But Severity' is selected.
# Patient 3 has NIHSS 4 ore more and 'No But Severity' is selected
# (inconsistent).
test_that("Check inconsistency rules - NIHSS 4 or more but No But is
          too mild.", {
  tpa_times <- tibble::tibble(
    S1AgeOnArrival = c(75, 82, 75),
    S1OnsetInHospital = c(FALSE, FALSE, FALSE),
    S1OnsetDateIsPrecise = c(TRUE, TRUE, TRUE),
    S1OnsetTimeIsPrecise = c(TRUE, FALSE, TRUE),
    S2NihssArrival = c(10, 3, 14),
    S1OnsetDateTime = as.POSIXct(
      c("2017-01-01 22:00:00",
        "2017-01-01 21:30:00",
        "2017-01-01 21:00:00"),
      origin = "1970-01-01",
      tz = "UTC"),
    S1PatientClockStartDateTime = as.POSIXct(
      c("2017-01-02 00:00:00",
        "2017-01-02 00:00:00",
        "2017-01-02 00:00:00"),
      origin = "1970-01-01",
      tz = "UTC"),
    S2ThrombolysisDateTime = as.POSIXct(
      c("2017-01-02 00:55:00",
        "2017-01-02 01:30:00",
        "2017-01-01 05:00:00"),
      origin = "1970-01-01",
      tz = "UTC"),
    S2Thrombolysis = factor(c("NB", "NB", "NB"),
      levels = c("Y", "N", "NB")),
    S2ThrombolysisNoBut = c(0,
      tpa_no_but["TooMildSevere"],
      tpa_no_but["TooMildSevere"]),
    S1FirstWard = c("SU", "SU", "SU"),
    S2IAI = c(FALSE, FALSE, FALSE),
    TCKIFirstStrokeUnitWithin4hrs = c(120, 180, 300)
  )
  tpa_times <- dplyr::mutate(tpa_times,
    !!! internal_d3ki_calculated_field_functions)

  expect_equal(tpa_times[["S2ThrombolysisNoButConsistent"]],
    c(0,
      128, # tpa_no_but is TooMildSevere
      0))
})

# Patient 1 is over 80 with an onset time of 3 hours, and age is
# selected.
# Patient 2 is over 80 with an onset time of 3 hours, and time window
# is selected.
# Patient 3 is over 80 with an imprecise onset time of 3 hours, and
# time window is selected.
# Patient 4 is over 80 with a precise onset time of 1 hour, and age
# is selected (inconsistent).
# Patient 5 is over 80 with a precise onset time of 1 hour, and time
# window is selected (inconsistent).
test_that("Check inconsistency rules - age over 80 but the NB is
          because of age.", {
  tpa_times <- tibble::tibble(
    S1AgeOnArrival = c(85, 85, 85, 85, 85),
    S1OnsetInHospital = c(FALSE, FALSE, FALSE, FALSE, FALSE),
    S1OnsetDateIsPrecise = c(TRUE, TRUE, TRUE, TRUE, TRUE),
    S1OnsetTimeIsPrecise = c(TRUE, FALSE, FALSE, TRUE, TRUE),
    S2NihssArrival = c(10, 3, 14, 12, 10),
    S2Thrombolysis = factor(c("NB", "NB", "NB", "NB", "NB"),
      levels = c("Y", "N", "NB")),
    S1OnsetDateTime = as.POSIXct(
      c("2017-01-01 21:00:00",
        "2017-01-01 21:00:00",
        "2017-01-01 21:00:00",
        "2017-01-01 23:00:00",
        "2017-01-01 23:00:00"),
      origin = "1970-01-01",
      tz = "UTC"),
    S1PatientClockStartDateTime = as.POSIXct(
      c("2017-01-02 00:00:00",
        "2017-01-02 00:00:00",
        "2017-01-02 00:00:00",
        "2017-01-02 00:00:00",
        "2017-01-02 00:00:00"),
      origin = "1970-01-01",
      tz = "UTC"),
    S2ThrombolysisDateTime = as.POSIXct(
      c("2017-01-02 00:55:00",
        "2017-01-02 01:30:00",
        "2017-01-02 01:30:00",
        "2017-01-02 01:30:00",
        "2017-01-02 05:00:00"),
      origin = "1970-01-01",
      tz = "UTC"),
    S2ThrombolysisNoBut = c(tpa_no_but["Age"],
                            tpa_no_but["TimeWindow"],
                            tpa_no_but["TimeWindow"],
                            tpa_no_but["Age"],
                            tpa_no_but["TimeWindow"]),
    S1FirstWard = c("SU", "SU", "SU", "SU", "SU"),
    S2IAI = c(FALSE, FALSE, FALSE, FALSE, FALSE),
    TCKIFirstStrokeUnitWithin4hrs = c(120, 180, 300, 360, 400)
  )
  tpa_times <- dplyr::mutate(tpa_times,
    !!! internal_d3ki_calculated_field_functions)

  expect_equal(tpa_times[["S2ThrombolysisNoButConsistent"]],
    c(64,  # tpa_no_but is Age
      512, # tpa_no_but is TimeWindow
      512, # tpa_no_but is TimeWindow
      0,
      0))
})

# Patient 1 has an inpatient stroke, with stroke in sleep, and time
# window is the no but.
# Patient 2 has an inpatient stroke, without stroke in sleep, and
# time window is not selected.
# Patient 3 has an inpatient stroke, without stroke in sleep, and
# time window is the no but (inconsistent), but too mild is
# consistent so therefore the patient still has a valid 'no but'.
test_that("Check inconsistency rules - Inpatient stroke.", {
  tpa_times <- tibble::tibble(
    S1AgeOnArrival = c(75, 82, 75),
    S1OnsetInHospital = c(TRUE, TRUE, TRUE),
    S1OnsetDateIsPrecise = c(NA, TRUE, TRUE),
    S1OnsetTimeIsPrecise = c(TRUE, TRUE, TRUE),
    S2NihssArrival = c(10, 14, 3),
    S2Thrombolysis = factor(c("NB", "Y", "NB"),
      levels = c("Y", "N", "NB")),
    S1OnsetDateTime = as.POSIXct(
      c("2017-01-01 22:00:00",
        "2017-01-01 21:30:00",
        "2017-01-01 21:00:00"),
      origin = "1970-01-01",
      tz = "UTC"),
    S1PatientClockStartDateTime = as.POSIXct(
      c("2017-01-02 00:00:00",
        "2017-01-02 00:00:00",
        "2017-01-02 00:00:00"),
      origin = "1970-01-01",
      tz = "UTC"),
    S2ThrombolysisDateTime = as.POSIXct(
      c("2017-01-02 00:55:00",
        "2017-01-02 01:30:00",
        "2017-01-01 05:00:00"),
      origin = "1970-01-01",
      tz = "UTC"),
    S2ThrombolysisNoBut = c(tpa_no_but["TimeWindow"],
      0,
      tpa_no_but["TooMildSevere"] + tpa_no_but["TimeWindow"]),
    S1FirstWard = c("SU", "SU", "SU"),
    S2IAI = c(FALSE, FALSE, FALSE),
    TCKIFirstStrokeUnitWithin4hrs = c(120, 180, 300)
  )
  tpa_times <- dplyr::mutate(
    tpa_times,
    !!! internal_d3ki_calculated_field_functions)

  expect_equal(tpa_times[["S2ThrombolysisNoButConsistent"]],
    c(512, # tpa_no_but is TimeWindow
      0,
      128)) # tpa_no_but is TooMildSevere
})
