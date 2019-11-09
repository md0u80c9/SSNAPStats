library(ssnapstats)
context("SSNAP time difference test")

test_that("Check ssnap_report_period with sample values", {
  test_times <- tibble::tibble(
    Time2 = as.POSIXct(
      c("2017-01-01 00:55:00",
        "1970-01-01 00:00:00",
        "2017-01-01 00:55:00",
        "2017-01-01 01:30:00",
        NA,
        "2017-01-01 01:30:00",
        "2017-01-02 02:31:00"),
      origin = "1970-01-01",
      tz = "UTC"),
    Time1 = as.POSIXct(
      c("1970-01-01 00:00:00",
        "2017-01-01 00:00:00",
        "2017-01-01 00:00:00",
        NA,
        "2017-01-01 00:00:00",
        "2017-01-01 00:00:00",
        "2017-01-03 00:01:00"),
      origin = "1970-01-01",
      tz = "UTC"))

  test_times <- dplyr::mutate(test_times,
    TimeTest = internal_time_difference(
      .data[["Time1"]], .data[["Time2"]], "mins"))

  expect_equal(test_times[["TimeTest"]],
               c(NA, NA, 55, NA, NA, 90, -1290))
})
