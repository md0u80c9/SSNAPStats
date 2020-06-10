library(ssnapstats)
context("Tests for Team72HrCohort")

test_that("A team admits and locks their patient to 72hrs.", {
  sample_data <- tibble::tibble(
    PatientId = 1,
    ProClinV1Id = 1,
    TeamCode = 900,
    TransferFromDateTime = as.POSIXct(NA),
    TransferFromTeamCode = NA,
    LockedS1 = TRUE,
    LockedS2 = TRUE,
    LockedS3 = TRUE,
    S1PatientClockStartDateTime = as.POSIXct("2019-01-01 00:00:30")
  )
  
  test_cohort <- ssnapstats::create_filtered_cohort(
    ssnap_data = sample_data,
    from_cohort_definition =
      ssnap_cohort_definitions[["Team72HrCohort"]])
  
  # Our single record should be included in the cohort unchanged.
  expect_equal(test_cohort$results, sample_data)
})

test_that("A community team is excluded.", {
  sample_data <- tibble::tibble(
    PatientId = 1,
    ProClinV1Id = 1,
    TeamCode = -900,
    TransferFromDateTime = as.POSIXct(NA),
    TransferFromTeamCode = NA,
    LockedS1 = TRUE,
    LockedS2 = TRUE,
    LockedS3 = TRUE,
    S1PatientClockStartDateTime = as.POSIXct("2019-01-01 00:00:30")
  )
  
  test_cohort <- ssnapstats::create_filtered_cohort(
    ssnap_data = sample_data,
    from_cohort_definition =
      ssnap_cohort_definitions[["Team72HrCohort"]])
  
  # Our single record will be excluded so we have a length 0 table
  expect_equal(length(test_cohort$results$PatientId), 0)
})


test_that(glue::glue(
  "A patient passed within 72hrs from one team to a second team,
  the second team locks the record"), {
  sample_data <- tibble::tibble(
    PatientId = c(1, 1),
    ProClinV1Id = c(1, 2),
    TeamCode = c(900, 901),
    TransferFromDateTime = c(as.POSIXct(NA),
                           as.POSIXct("2019-01-02 00:00:30")),
    TransferFromTeamCode = c(NA, 900),
    LockedS1 = c(TRUE, TRUE),
    LockedS2 = c(FALSE, TRUE),
    LockedS3 = c(FALSE, TRUE),
    S1PatientClockStartDateTime = c(as.POSIXct("2019-01-01 00:00:30"),
      as.POSIXct("2019-01-01 00:00:30")),
    S1AgeOnArrival = c(NA, 42)
  )
  
  test_cohort <- ssnapstats::create_filtered_cohort(
    ssnap_data = sample_data,
    from_cohort_definition =
      ssnap_cohort_definitions[["Team72HrCohort"]])
  
  # Our single record should be allocated to the admitting team
  expected_result <- tibble::tibble(
    PatientId = 1,
    ProClinV1Id = 1,
    TeamCode = 900,
    TransferFromDateTime = as.POSIXct(NA),
    TransferFromTeamCode = NA_real_,
    LockedS1 = TRUE,
    LockedS2 = TRUE,
    LockedS3 = TRUE,
    S1PatientClockStartDateTime = as.POSIXct("2019-01-01 00:00:30"),
    S1AgeOnArrival = 42)
  
  expect_equal(test_cohort$results, expected_result)
})



test_that(glue::glue(
  "A patient passed within 72hrs from one team to a second team,
  the first team locks the record"), {
    sample_data <- tibble::tibble(
      PatientId = c(1, 1),
      ProClinV1Id = c(1, 2),
      TeamCode = c(900, 901),
      TransferFromDateTime = c(as.POSIXct(NA),
                               as.POSIXct("2019-01-02 00:00:30")),
      TransferFromTeamCode = c(NA, 900),
      LockedS1 = c(TRUE, TRUE),
      LockedS2 = c(TRUE, TRUE),
      LockedS3 = c(TRUE, TRUE),
      S1PatientClockStartDateTime = c(as.POSIXct("2019-01-01 00:00:30"),
                                      as.POSIXct("2019-01-01 00:00:30")),
      S1AgeOnArrival = c(56, 99)
    )
    
    test_cohort <- ssnapstats::create_filtered_cohort(
      ssnap_data = sample_data,
      from_cohort_definition =
        ssnap_cohort_definitions[["Team72HrCohort"]])
    
    # Our single record should use only data from the first team and
    # ignore any (erroneous) edits to the second record.
    expected_result <- tibble::tibble(
      PatientId = 1,
      ProClinV1Id = 1,
      TeamCode = 900,
      TransferFromDateTime = as.POSIXct(NA),
      TransferFromTeamCode = NA_real_,
      LockedS1 = TRUE,
      LockedS2 = TRUE,
      LockedS3 = TRUE,
      S1PatientClockStartDateTime = as.POSIXct("2019-01-01 00:00:30"),
      S1AgeOnArrival = 56)
    
    expect_equal(test_cohort$results, expected_result)
  })

test_that(glue::glue(
  "A patient whose admitting team is before the review window
 (should be excluded) but the record was locked to 72hrs inside
 the review window by a second team."), {
    
test_audit_outputs_table <- create_output_tbl(
  tibble::tribble(~x, ~numerator, ~category, ~output_type,
  ssnap_measures[["AgeOnArrival"]], NULL, "Casemix", "median"))

  sample_data <- tibble::tibble(
    PatientId = c(1, 1),
    ProClinV1Id = c(1, 2),
    TeamCode = c(900, 901),
    TransferFromDateTime = c(as.POSIXct(NA),
                             as.POSIXct("2019-01-02 00:00:30")),
    TransferFromTeamCode = c(NA, 900),
    LockedS1 = c(TRUE, TRUE),
    LockedS2 = c(FALSE, TRUE),
    LockedS3 = c(FALSE, TRUE),
    S1PatientClockStartDateTime =
      c(as.POSIXct("2018-12-31 00:00:30"),
        as.POSIXct("2018-12-31 00:00:30")),
    S1AgeOnArrival = c(NA, 42)
  )

  test_cohort <- ssnapstats::create_aggregated_cohort(
    ssnap_data = sample_data,
    period_type = "months",
    aggregate_by = "team",
    audit_outputs_table = test_audit_outputs_table,
    from_cohort_definition =
      ssnap_cohort_definitions[["Team72HrCohort"]])

  # The patient will be placed in December's data, even though the
  # second team which locked the record did so in January.
  expected_result <- tibble::tibble(
    TeamCode = 900,
    ReportPeriod = as.Date("2018-12-01"),
    n = as.integer(1),
    AgeOnArrivalQ2 = 42)
  
  actual_result <- dplyr::ungroup(test_cohort$results)

  expect_equal(actual_result, expected_result)
})

# A patient for six month review (excluded)
