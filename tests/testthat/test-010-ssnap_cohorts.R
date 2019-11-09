library(ssnapstats)
context("SSNAP cohort tests")

for (cohort in ssnap_cohort_names) {
  test_that(glue::glue("Create report for cohort {cohort}"), {
    expect_true(
      !is.null(ssnap_cohorts(
        ssnap_fake_data,
        cohort,
        "months",
        "team")
      )
    )
    expect_true(
      !is.null(ssnap_cohorts(
        ssnap_fake_data,
        cohort,
        "months",
        "national")
      )
    )
  })
}

# We need to build a unit test to check filters work as expected.
# For example if you want to filter a report period - you need to
# filter after building the reports together just in case you
# miss out records which should be included.
# I suspect we can do this more efficiently but we need to build a
# set of tests to check the filtering works as expected.
