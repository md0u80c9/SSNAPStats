library(ssnapstats)
context("Check internal_d567ki_calculated_field_functions")

# The data consists of four months
# Month 1 tests out 4 % admitted in 4 hours (should be 80%)
# Month 2 tests out the median (should be 90)

therapy_times <- tibble::tibble(
  S1PatientClockStartDateTime = as.POSIXct(c("2017-01-01 00:00:00",
    "2017-01-01 00:00:00",
    "2017-01-01 00:00:00",
    "2017-01-01 00:00:00",
    "2017-01-01 00:00:00"),
    origin = "1970-01-01", tz = "UTC"),
  S1TeamClockStartDateTime = .data[["S1PatientClockStartDateTime"]],
  S4PhysioDays = c(0, 1, 0, 1, 1),
  S4PhysioEndDate = as.POSIXct(c("2017-01-01 00:00:00",
    "2017-01-03 00:00:00",
    "2017-01-01 00:00:00",
    "2017-01-02 00:00:00",
    "2017-01-02 00:00:00"),
    origin = "1970-01-01", tz = "UTC"),
  S4OccTherEndDate = .data[["S4PhysioEndDate"]],
  S4SpeechLangEndDate = .data[["S4PhysioEndDate"]],
  S4OccTherDays = c(0, 2, 0, 1, 1),
  S4SpeechLangDays = c(0, 2, 0, 1, 1),
  S4Physio = c(TRUE, FALSE, TRUE, FALSE, TRUE),
  S4OccTher = c(TRUE, FALSE, TRUE, FALSE, TRUE),
  S4SpeechLang = c(TRUE, FALSE, TRUE, FALSE, TRUE),
  S4PhysioMinutes = c(30, 30, 30, 30, 30),
  S4OccTherMinutes = c(10, 15, 20, 25, 30),
  S4SpeechLangMinutes = c(30, 30, 30, 30, 30)
)

therapy_times <- dplyr::mutate(
  therapy_times,
  !!! internal_d567ki_calculated_field_functions)

test_that("Check minutes of physio need", {
  expect_equal(therapy_times[["S4TeamPhysioNeedMins"]],
               c(0, 48 * 60, 0, 24 * 60, 24 * 60))
})

# Cases 2 and 4 had no physio need, so they are excluded.

  test_that("Check % therapy days delivered", {
    expect_equal(therapy_times[["TCKIPCPhysioDays"]],
                 c(0, NA, 0, NA, 100))
  })

  test_that("monthlyTherapyData minsPerDay", {
    expect_equal(therapy_times[["TCKISpeechLangMinutesPerDay"]],
                 c(0, 15, 0, 30, 30))
  })
