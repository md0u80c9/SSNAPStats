library(ssnapstats)
context("Match SSNAP cohorts statements to imported CSV")

for (cohort in ssnap_cohort_definitions) {
    test_that(glue::glue("CSV columns for {cohort$cohort_name}"), {
      validated_columns <- unique(c(
        source_data_columns(cohort$row_selection_criteria),
        cohort$reporting_period_index_name))
      csv_columns <- NULL
      if (!is.null(validated_columns)) {
        csv_columns <- vector(mode = "character",
                            length = length(validated_columns))
        if (!is.null(cohort$csv_columns)) {
          csv_columns <- cohort$csv_columns
        }
      }
      expect_equal(csv_columns, validated_columns)
  })

  # TODO We can't check attribute_to_team or apply_data_from yet.
  # Instead row_selection_criteria is needed to specify !is.null
  # for columns which are needed from those functions. This is why
  # ssnap_cohort_filters for things like '72hr data present' exists.
  # This is a little inefficient and we should optimise this later.
}
