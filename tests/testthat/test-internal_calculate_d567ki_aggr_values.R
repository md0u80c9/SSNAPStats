library(ssnapstats)
context("Check internal_calculate_d567ki_aggr_values")

# The data consists of four months
# Month 1 tests out 4 % admitted in 4 hours (should be 80%)
# Month 2 tests out the median (should be 90)
clock_start <- as.POSIXct(c("2017-01-01 00:00:01",
                           "2017-01-01 00:00:01",
                           "2017-01-01 00:00:01",
                           "2017-01-01 00:00:01",
                           "2017-01-01 00:00:01",
                           "2017-02-01 00:00:01",
                           "2017-02-01 00:00:01",
                           "2017-02-01 00:00:01",
                           "2017-02-01 00:00:01",
                           "2017-02-01 00:00:01",
                           "2017-03-01 00:00:01",
                           "2017-03-01 00:00:01",
                           "2017-03-01 00:00:01",
                           "2017-03-01 00:00:01",
                           "2017-03-01 00:00:01",
                           "2017-04-01 00:00:01",
                           "2017-04-01 00:00:01",
                           "2017-04-01 00:00:01",
                           "2017-04-01 00:00:01",
                           "2017-04-01 00:00:01"),
                         origin = "1970-01-01", tz = "UTC")
valid_therapyneed <- c(TRUE, FALSE, TRUE, FALSE, TRUE,
                       TRUE,  TRUE, TRUE,  TRUE, TRUE,
                       TRUE,  TRUE, TRUE,  TRUE, TRUE,
                       FALSE, TRUE, TRUE,  TRUE, TRUE)
end_date <- as.POSIXct(c("2017-01-02 00:00:01",
                        "2017-01-02 00:00:01",
                        "2017-01-02 00:00:01",
                        "2017-01-02 00:00:01",
                        "2017-01-02 00:00:01",
                        "2017-02-02 00:00:01",
                        "2017-02-02 00:00:01",
                        "2017-02-02 00:00:01",
                        "2017-02-02 00:00:01",
                        "2017-02-02 00:00:01",
                        "2017-03-02 00:00:01",
                        "2017-03-03 00:00:01",
                        "2017-03-04 00:00:01",
                        "2017-03-02 00:00:01",
                        "2017-03-03 00:00:01",
                        "2017-04-06 00:00:01",
                        "2017-04-06 00:00:01",
                        "2017-04-06 00:00:01",
                        "2017-04-06 00:00:01",
                        "2017-04-06 00:00:01"),
                      origin = "1970-01-01", tz = "UTC")
valid_therapydays <- c(1, 1, 1, 1, 1,
                       1, 1, 1, 1, 1,
                       0, 2, 0, 1, 1,
                       5, 5, 5, 5, 5)
valid_therapymins <- c(30, 30, 30, 30, 30,
                       10, 15, 20, 25, 30,
                       22, 22, 22, 22, 22,
                       45, 45, 45, 45, 45)

therapy_times <- tibble::tibble(
  S1PatientClockStartDateTime = clock_start,
  S1TeamClockStartDateTime = clock_start,
  S4PhysioEndDate = end_date,
  S4PhysioDays = valid_therapydays,
  S4OccTherEndDate = end_date,
  S4OccTherDays = valid_therapydays,
  S4SpeechLangEndDate = end_date,
  S4SpeechLangDays = valid_therapydays,
  S4Physio = valid_therapyneed,
  S4OccTher = valid_therapyneed,
  S4SpeechLang = valid_therapyneed,
  S4PhysioMinutes = valid_therapymins,
  S4OccTherMinutes = valid_therapymins,
  S4SpeechLangMinutes = valid_therapymins
)
therapy_times <- dplyr::mutate(
  therapy_times,
  !!! internal_d567ki_calculated_field_functions)

# Then create monthly tallies and group the data (NB we only have one group
# in our sample, but this imitates the domain calculations)
therapy_times <- dplyr::mutate(
  therapy_times,
  reportingMonth = format(.data[["S1PatientClockStartDateTime"]],
    "%m-%Y"))
  monthly72hrs <- dplyr::group_by(therapy_times,
    .data[["reportingMonth"]])
  kiresults <- dplyr::summarise(monthly72hrs,
  !!! internal_d567ki_aggr_value_functions("TC")
)

# Test sample data row 1 to check % with a therapy need correctly
  test_that("monthlyTherapyData % therapy need", {
    expect_equal(kiresults[["TCKIPCOccTher"]][1], 60)
  })

# Test sample data row 2 to check the median minutes of therapy are correct
  test_that("monthlyTherapyData median minutes of therapy", {
    expect_equal(kiresults[["TCKIMedianPhysioMinutes"]][2], 20)
  })
  
# Test sample data row 3 to check the median % days of therapy are correct
  test_that("monthlyTherapyData median minutes of therapy", {
    expect_equal(kiresults[["TCKIMedianPCSpeechLangDays"]][3], 50)
  })

# Test sample data row 4 to check the compliance calculation works
# We only check OT using this; PT and SLT are assumed as there is a trivial difference
  test_that("monthlyTherapyData check OT compliance", {
    expect_equal(kiresults[["TCKIOccTherCompliance"]][4], 28)
  })
