library(ssnapstats)
context("Test audit output spec production")

# These tests will use two real SSNAP measures, Age On Arrival, and
# AgeOver80 (a discrete version of age on arrival).
# NB We don't test the actual formulae yet - but we will do later.

test_measures <- list(
  AgeOnArrival = audit_measure(
    stem_name = "AgeOnArrival",
    description = "Age on arrival",
    exclusions = NULL,
    numerators = rlang::expr(.data[["S1AgeOnArrival"]]),
    measure_type = "continuous"),

  # AgeOver80 =======================================================

  # F4.4-6 Proportion aged > 80
  AgeOver80 = audit_measure(
    stem_name = "AgeOver80",
    description = "Age over 80",
    exclusions = NULL,
    numerators = rlang::expr(.data[["S1AgeOnArrival"]] > 80),
    measure_type = "discrete")
)
  
test_that("Check audit_measure outputs medians", {
  test1 <- create_output(
    test_measures[["AgeOnArrival"]],
    output_type = "median")
  expect_true(rlang::is_list(test1))
  expect_equal(length(test1), 1)
})

test_that("Check audit_measure outputs quartiles", {
  test2 <- create_output(
    test_measures[["AgeOnArrival"]],
    output_type = "quartiles")
  expect_true(rlang::is_list(test2))
  expect_equal(length(test2), 3)
})

test_that("Check audit_measure outputs percentages", {
  test3 <- create_output(
    test_measures[["AgeOver80"]],
    output_type = "pct")
  expect_true(rlang::is_list(test3))
  expect_equal(length(test3), 1)
})

test_that("Check audit_measure outputs D, N and percentages", {
  test4 <- create_output(
    test_measures[["AgeOver80"]],
    output_type = "d_n_pct")
  expect_true(rlang::is_list(test4))
  expect_equal(length(test4), 3)
})
