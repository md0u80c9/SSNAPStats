library(ssnapstats)
context("Audit measure class tests")

test_that("is_audit_measure rejects apppropriately", {
  expect_equal(is_audit_measure("Not an audit measure"), FALSE)
})

test_that("Build a working audit measure", {
  expect_equal(is_audit_measure(
    audit_measure(
      stem_name = "Gender",
      description = "Gender",
      exclusions = NULL,
      numerators = list(
        "Male" = rlang::quo(.data[["S1IsMale"]]),
        "Female" = rlang::quo(!.data[["S1IsMale"]])
      ),
      measure_type = "discrete")),
    TRUE)
})
