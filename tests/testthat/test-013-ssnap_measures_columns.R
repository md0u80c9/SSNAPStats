library(ssnapstats)
context("Match SSNAP measures to imported CSV")

for (measure in ssnap_measures) {
    test_that(glue::glue("CSV columns for {measure$stem_name}"), {
      validated_columns <- c(source_data_columns(measure$numerators),
          source_data_columns(list(measure$exclusions)))
      csv_columns <- NULL
      if (!is.null(validated_columns)) {
        csv_columns <- vector(mode = "character",
                            length = length(validated_columns))
        if (!is.null(measure$csv_columns)) {
          csv_columns <- measure$csv_columns
        }
      }
      expect_equal(csv_columns, validated_columns)
  })
}
