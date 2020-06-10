library(ssnapstats)
context("Tests for Team7DayCohort")


test_that("A team admits and locks their patient to discharge.", {
  sample_data <- tibble::tibble(
    PatientId = 1,
    ProClinV1Id = 1,
    TeamCode = 900,
    TransferFromDateTime = as.POSIXct(NA),
    TransferFromTeamCode = NA,
    S1TeamClockStartDateTime = as.POSIXct("2019-01-01 00:00:30"),
    LockedS1 = TRUE,
    LockedS2 = TRUE,
    LockedS3 = TRUE,
    LockedS4 = TRUE,
    LockedS5 = TRUE,
    LockedS6 = TRUE,
    LockedS7 = TRUE,
    InpatientReadmission = 0,
    S1PatientClockStartDateTime = as.POSIXct("2019-01-01 00:00:30"),
    S7DischargeType = "H"
  )
  
  test_cohort <- ssnapstats::create_filtered_cohort(
    ssnap_data = sample_data,
    from_cohort_definition =
      ssnap_cohort_definitions[["Team7DayCohort"]])

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
    LockedS4 = TRUE,
    LockedS5 = TRUE,
    LockedS6 = TRUE,
    LockedS7 = TRUE,
    InpatientReadmission = 0,
    S1TeamClockStartDateTime = as.POSIXct("2019-01-01 00:00:30"),
    S1PatientClockStartDateTime = as.POSIXct("2019-01-01 00:00:30"),
    S7DischargeType = "H")
  
  test_cohort <- ssnapstats::create_filtered_cohort(
    ssnap_data = sample_data,
    from_cohort_definition =
      ssnap_cohort_definitions[["Team7DayCohort"]])
  
  # Our single record will be excluded so we have a length 0 table
  expect_equal(length(test_cohort$results$PatientId), 0)
})

# Need to test the whole locked to discharge / transfer thing too
# So if the last record is still unlocked the patient should be
# excluded.

# This test proves that the cohort selection is failing.
# Ends up with no records - I think because we're filtering wrongly.

test_that(glue::glue(
  "A patient passed after 10 days from one team to a second team,
  the first team should be the '7 day' cohort"), {
  sample_data <- tibble::tibble(
    PatientId = c(1, 1),
    ProClinV1Id = c(1, 2),
    TeamCode = c(900, 901),
    TransferFromDateTime = c(as.POSIXct(NA),
                             as.POSIXct("2019-01-10 00:00:30")),
    TransferFromTeamCode = c(NA, 900),
    S1TeamClockStartDateTime = c(as.POSIXct("2019-01-01 00:00:30"),
                                 as.POSIXct("2019-01-10 00:00:30")),
    LockedS1 = c(TRUE, TRUE),
    LockedS2 = c(TRUE, TRUE),
    LockedS3 = c(TRUE, TRUE),
    LockedS4 = c(TRUE, TRUE),
    LockedS5 = c(TRUE, TRUE),
    LockedS6 = c(TRUE, TRUE),
    LockedS7 = c(TRUE, TRUE),
    InpatientReadmission = c(0, 0),
    S1PatientClockStartDateTime = c(as.POSIXct("2019-01-01 00:00:30"),
                                    as.POSIXct("2019-01-01 00:00:30")),
    S7DischargeType = c("T", "H"),
    S1AgeOnArrival = c(0, 42))
  
  test_cohort <- ssnapstats::create_filtered_cohort(
    ssnap_data = sample_data,
    from_cohort_definition =
      ssnap_cohort_definitions[["Team7DayCohort"]])

  # Our single record should be allocated to the admitting team
  expected_result <- tibble::tibble(
    PatientId = 1,
    ProClinV1Id = 1,
    TeamCode = 900,
    TransferFromDateTime = as.POSIXct(NA),
    TransferFromTeamCode = NA_real_,
    S1TeamClockStartDateTime = as.POSIXct("2019-01-01 00:00:30"),
    LockedS1 = TRUE,
    LockedS2 = TRUE,
    LockedS3 = TRUE,
    LockedS4 = TRUE,
    LockedS5 = TRUE,
    LockedS6 = TRUE,
    LockedS7 = TRUE,
    InpatientReadmission = 0,
    S1PatientClockStartDateTime = as.POSIXct("2019-01-01 00:00:30"),
    S7DischargeType = "H",
    S1AgeOnArrival = 42)
  
  expect_equal(test_cohort$results, expected_result)
})

# Repeat the above test but the team locking the record will be the
# second team, not the first team.

# A patient for six month review (excluded)
