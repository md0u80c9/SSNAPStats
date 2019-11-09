library(ssnapstats)
context("SSNAP report period tests")

test_that("Check ssnap_report_period with sample values", {
  expect_equal(ssnap_report_period(as.Date("2014-02-01")), 5)
  expect_equal(ssnap_report_period(as.Date("2018-02-01")), 19)
})

test_that(glue::glue(
  "SSNAP Report series are currently only valid until 2030,
   so fail the unit test if we are at 2029 as a reminder to
  check the SSNAP reports code"), {
  expect_true(Sys.Date() < as.Date("2029-01-01"))
})
