
#' Define available auditing measures
#' 
#' These functions provide a list of all the available auditing
#' measures. The master list is retrieved using \code{ssnap_measures}
#' which is an amalgamation of all lists; lists of measures intended
#' for a particular cohort group are stored using the other functions
#' for code legibility and/or ease of access if you know you only
#' need a subset of the measures.

#' @importFrom rlang ":="
#' @importFrom rlang "!!"
#' @export

ssnap_measures <- list(

# * Casemix ---------------------------------------------------------
# * -----------------------------------------------------------------

  # Gender ==========================================================

  # F3.1-5 Proportions of men and women.
  Gender = audit_measure(
    stem_name = "Gender",
    description = "Gender",
    exclusions = NULL,
    numerators = list(
      "Male" = rlang::expr(.data[["S1IsMale"]]),
      "Female" = rlang::expr(!.data[["S1IsMale"]])),
    numerator_descriptors = list(
      "Male" = "Male",
      "Female" = "Female"),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "Male",
        rlang::expr(.data[["S1IsMale"]]),
        "Male",
      "Female",
        rlang::expr(!.data[["S1IsMale"]]),
        "Female"
    ),
    csv_columns = "S1IsMale",
    measure_type = "discrete"),

  # AgeOnArrival ====================================================

  # F4.1-3 Age distribution
  # G1.3, H1.3 Median age.
  AgeOnArrival = audit_measure(
    stem_name = "AgeOnArrival",
    description = "Age on arrival",
    exclusions = NULL,
    numerators = rlang::expr(.data[["S1AgeOnArrival"]]),
    new_numerators = rlang::expr(.data[["S1AgeOnArrival"]]),
    csv_columns = "S1AgeOnArrival",
    measure_type = "continuous"),

  # AgeOver80 =======================================================

  # F4.4-6 Proportion aged > 80
  AgeOver80 = audit_measure(
    stem_name = "AgeOver80",
    description = "Age over 80",
    exclusions = NULL,
    numerators = rlang::expr(.data[["S1AgeOnArrival"]] > 80),
    new_numerators = rlang::expr(.data[["S1AgeOnArrival"]] > 80),
    csv_columns = "S1AgeOnArrival",
    measure_type = "discrete"),

  #TODO Conditional medians for age distribution of men and women
  # F4.7-4.9 Female age distribution
  # F4.10-4.12 Male age distribution

  # FemaleAgeOver80 =================================================

  # F4.13-15 Females aged over 80
  FemaleAgeOver80 = audit_measure(
    stem_name = "FemaleAgeOver80",
    description = "Women aged over 80",
    exclusions = rlang::expr(!.data[["S1IsMale"]]),
    numerators = rlang::expr(.data[["S1AgeOnArrival"]] > 80),
    new_numerators = rlang::expr(.data[["S1AgeOnArrival"]] > 80),
    csv_columns = c("S1AgeOnArrival", "S1IsMale"),
    measure_type = "continuous"),

  # MaleAgeOver80 ===================================================

  # F4.16-18 Males aged over 80
  MaleAgeOver80 = audit_measure(
    stem_name = "MaleAgeOver80",
    description = "Men aged over 80",
    exclusions = rlang::expr(!.data[["S1IsMale"]]),
    numerators = rlang::expr(.data[["S1AgeOnArrival"]] > 80),
    new_numerators = rlang::expr(.data[["S1AgeOnArrival"]] > 80),
    csv_columns = c("S1AgeOnArrival", "S1IsMale"),
    measure_type = "continuous"),

  # AgeOnArrivalGrouped =============================================

  # F4.19-29 Age breakdown
  AgeOnArrivalGrouped = audit_measure(
    stem_name = "AgeOnArrivalGrouped",
    description = "Age on arrival (grouped)",
    exclusions = NULL,
    numerators = rlang::exprs(
      "LessThan60" = .data[["S1AgeOnArrival"]] < 60,
      "60To69" = dplyr::between(.data[["S1AgeOnArrival"]], 60, 69),
      "70To79" = dplyr::between(.data[["S1AgeOnArrival"]], 70, 79),
      "80To89" = dplyr::between(.data[["S1AgeOnArrival"]], 80, 89),
      "90OrMore" = .data[["S1AgeOnArrival"]] >= 90),
    numerator_descriptors = list(
      "LessThan60" = "Age less than 60",
      "60To69" = "Age 60-69",
      "70To79" = "Age 70-79",
      "80To89" = "Age 80-89",
      "90OrMore" = "Age 90+"),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "LessThan60", 
        rlang::expr(.data[["S1AgeOnArrival"]] < 60),
        "Age less than 60",
      "60To69",
        rlang::expr(dplyr::between(.data[["S1AgeOnArrival"]], 60, 69)),
        "Age 60-69",
      "70To79",
        rlang::expr(dplyr::between(.data[["S1AgeOnArrival"]], 70, 79)),
        "Age 70-79",
      "80To89",
        rlang::expr(dplyr::between(.data[["S1AgeOnArrival"]], 80, 89)),
        "Age 80-89",
      "90OrMore",
        rlang::expr(.data[["S1AgeOnArrival"]] >= 90),
        "Age 90+"),
    csv_columns = "S1AgeOnArrival",
    measure_type = "discrete"),

  # EthnicityKnown ==================================================

  # C6.1-6.3 Ethnicity known
  EthnicityKnown = audit_measure(
    stem_name = "EthnicityKnown",
    description = "Ethnicity known",
    exclusions = NULL,
    numerators = rlang::expr(.data[["S1Ethnicity"]] != "Unknown"),
    new_numerators = rlang::expr(.data[["S1Ethnicity"]] != "Unknown"),
    csv_columns = "S1Ethnicity",
    measure_type = "discrete"),


# * -----------------------------------------------------------------
# * Record creation -------------------------------------------------

  ClockStartToRecordCreationHours = audit_measure(
    stem_name = "ClockStartToRecordCreationHours",
    description = "Clock start to record creation (hours)",
    exclusions = NULL,
    numerators = ssnap_field[["ClockStartToRecordCreationHours"]],
    new_numerators = rlang::expr(ssnap_field[["ClockStartToRecordCreationHours"]]),
    csv_columns = c("S1PatientClockStartDateTime", "CreatedDateTime"),
    measure_type = "continuous"),


# * -----------------------------------------------------------------
# * Comorbidities ---------------------------------------------------
# * -----------------------------------------------------------------

  # ComorbidityCount ================================================

  # F5.1-5.13 Comorbidities
  ComorbidityCount = audit_measure(
    stem_name = "ComorbidityCount",
    description = "Comorbidity count",
    exclusions = NULL,
    numerators = rlang::exprs(
      "0" = !! ssnap_field[["Comorbidities"]] == 0,
      "1" = !! ssnap_field[["Comorbidities"]] == 1,
      "2" = !! ssnap_field[["Comorbidities"]] == 2,
      "3" = !! ssnap_field[["Comorbidities"]] == 3,
      "4" = !! ssnap_field[["Comorbidities"]] == 4,
      "5" = !! ssnap_field[["Comorbidities"]] == 5),
    numerator_descriptors = list(
      "0" = "0 comorbidities",
      "1" = "1 comorbidity",
      "2" = "2 comorbidities",
      "3" = "3 comorbidities",
      "4" = "4 comorbidities",
      "5" = "5 comorbidities"),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "0",
        rlang::expr(!! ssnap_field[["Comorbidities"]] == 0),
        "0 comorbidities",
      "1",
        rlang::expr(!! ssnap_field[["Comorbidities"]] == 1),
        "1 comorbidity",
      "2",
        rlang::expr(!! ssnap_field[["Comorbidities"]] == 2),
        "2 comorbidities",
      "3",
        rlang::expr(!! ssnap_field[["Comorbidities"]] == 3),
        "3 comorbidities",
      "4",
        rlang::expr(!! ssnap_field[["Comorbidities"]] == 4),
        "4 comorbidities",
      "5",
        rlang::expr(!! ssnap_field[["Comorbidities"]] == 5),
        "5 comorbidities"),
    csv_columns = c("S2CoMCongestiveHeartFailure",
                    "S2CoMHypertension",
                    "S2CoMAtrialFibrillation",
                    "S2CoMDiabetes",
                    "S2CoMStrokeTIA"),
    measure_type = "discrete"),

  # CongestiveHeartFailure ==========================================

  # F5.14-16 CHF before stroke
  CongestiveHeartFailure = audit_measure(
    stem_name = "CongestiveHeartFailure",
    description = "Congestive heart failure",
    exclusions = NULL,
    numerators = rlang::expr(.data[["S2CoMCongestiveHeartFailure"]]),
    new_numerators = rlang::expr(.data[["S2CoMCongestiveHeartFailure"]]),
    csv_columns = "S2CoMCongestiveHeartFailure",
    measure_type = "discrete"),

  # Hypertension ====================================================

  # F5.17-19 Hypertension before stroke
  Hypertension = audit_measure(
    stem_name = "Hypertension",
    description = "Hypertension",
    exclusions = NULL,
    numerators = rlang::expr(.data[["S2CoMHypertension"]]),
    new_numerators = rlang::expr(.data[["S2CoMHypertension"]]),
    csv_columns = "S2CoMHypertension",
    measure_type = "discrete"),

  # Diabetes ========================================================

  # F5.20-22 Diabetes before stroke
  Diabetes = audit_measure(
    stem_name = "Diabetes",
    description = "Diabetes",
    exclusions = NULL,
    numerators = rlang::expr(.data[["S2CoMDiabetes"]]),
    new_numerators = rlang::expr(.data[["S2CoMDiabetes"]]),
    csv_columns = "S2CoMDiabetes",
    measure_type = "discrete"),

  # PreviousStrokeTIA ===============================================

  # F5.23-25 Stroke or TIA before stroke
  PreviousStrokeTIA = audit_measure(
    stem_name = "PreviousStrokeTIA",
    description = "Previous stroke or TIA",
    exclusions = NULL,
    numerators = rlang::expr(.data[["S2CoMStrokeTIA"]]),
    new_numerators = rlang::expr(.data[["S2CoMStrokeTIA"]]),
    csv_columns = "S2CoMStrokeTIA",
    measure_type = "discrete"),

# * -----------------------------------------------------------------
# * Atrial fibrillation ---------------------------------------------
# * -----------------------------------------------------------------

  # AtrialFibrillation ==============================================

  # F6.1-3 Atrial fibrillation (AF) before stroke
  AtrialFibrillation = audit_measure(
    stem_name = "AtrialFibrillation",
    description = "Previously known atrial fibrillation",
    exclusions = NULL,
    numerators = rlang::expr(.data[["S2CoMAtrialFibrillation"]]),
    new_numerators = rlang::expr(.data[["S2CoMAtrialFibrillation"]]),
    csv_columns = "S2CoMAtrialFibrillation",
    measure_type = "discrete"),

  # AFOnAntiplatelet ================================================

  # F6.4-10 Atrial fibrillation (AF) before stroke
  AFOnAntiplatelet = audit_measure(
    stem_name = "AFOnAntiplatelet",
    description = "Previous atrial fibrillation on antiplatelets",
    exclusions = NULL,
    numerators = rlang::exprs(
      "Yes" = .data[["S2CoMAFAntiplatelet"]] == "Y",
      "No" = .data[["S2CoMAFAntiplatelet"]] == "N",
      "NoBut" = .data[["S2CoMAFAntiplatelet"]] == "NB"),
    numerator_descriptors = list("Yes", "No", "No but"),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "Yes",
        rlang::expr(.data[["S2CoMAFAntiplatelet"]] == "Y"),
        "Yes",
      "No",
        rlang::expr(.data[["S2CoMAFAntiplatelet"]] == "N"),
        "No",
      "NoBut",
        rlang::expr(.data[["S2CoMAFAntiplatelet"]] == "NB"),
        "No but"),
    csv_columns = "S2CoMAFAntiplatelet",
    measure_type = "discrete"),

  # AFOnAnticoagulant ===============================================

  # F6.11-17 Atrial fibrillation (AF) before stroke
  AFOnAnticoagulant = audit_measure(
    stem_name = "AFOnAnticoagulant",
    description = "Previous atrial fibrillation on anticoagulants",
    exclusions = NULL,
    numerators = rlang::exprs(
      "Yes" = .data[["S2CoMAFAnticoagulent"]] == "Y",
      "No" = .data[["S2CoMAFAnticoagulent"]] == "N",
      "NoBut" = .data[["S2CoMAFAnticoagulent"]] == "NB"),
    numerator_descriptors = list("Yes", "No", "No but"),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "Yes",
        rlang::expr(.data[["S2CoMAFAnticoagulent"]] == "Y"),
        "Yes",
      "No",
        rlang::expr(.data[["S2CoMAFAnticoagulent"]] == "N"),
        "No",
      "NoBut",
        rlang::expr(.data[["S2CoMAFAnticoagulent"]] == "NB"),
        "No but"),
    csv_columns = "S2CoMAFAnticoagulent",
    measure_type = "discrete"),

  # AFMeds ==========================================================

  # F6.18-26 Atrial fibrillation (AF) before stroke
  AFMeds = audit_measure(
    stem_name = "AFMeds",
    description = "Atrial fibrillation medication",
    exclusions = NULL,
    numerators = rlang::exprs(
      "Both" = (.data[["S2CoMAFAnticoagulent"]] == "Y")
        & (.data[["S2CoMAFAntiplatelet"]] == "Y"),
      "Anticoagulant" = (.data[["S2CoMAFAnticoagulent"]] == "Y")
        & (.data[["S2CoMAFAntiplatelet"]] != "Y"),
      "Antiplatelet" = (.data[["S2CoMAFAnticoagulent"]] != "Y")
        & (.data[["S2CoMAFAntiplatelet"]] == "Y"),
      "None" = (.data[["S2CoMAFAnticoagulent"]] != "Y")
        & (.data[["S2CoMAFAntiplatelet"]] != "Y")),
    numerator_descriptors = list(
      "Both" = "Both anticoagulant and antiplatelet medication",
      "Anticoagulant" = "Anticoagulant medication only",
      "Antiplatelet" = "Antiplatelet medication only",
      "None" = "Neither medication"),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "Both",
        rlang::expr((.data[["S2CoMAFAnticoagulent"]] == "Y")
          & (.data[["S2CoMAFAntiplatelet"]] == "Y")),
        "Both anticoagulant and antiplatelet medication",
      "Anticoagulant",
        rlang::expr((.data[["S2CoMAFAnticoagulent"]] == "Y")
          & (.data[["S2CoMAFAntiplatelet"]] != "Y")),
        "Anticoagulant medication only",
      "Antiplatelet",
        rlang::expr((.data[["S2CoMAFAnticoagulent"]] != "Y")
          & (.data[["S2CoMAFAntiplatelet"]] == "Y")),
        "Antiplatelet medication only",
      "None",
        rlang::expr((.data[["S2CoMAFAnticoagulent"]] != "Y")
          & (.data[["S2CoMAFAntiplatelet"]] != "Y")),
        "Neither medication"),
    csv_columns = c("S2CoMAFAnticoagulent", "S2CoMAFAntiplatelet"),
    measure_type = "discrete"),

# * -----------------------------------------------------------------
# * StrokeType ------------------------------------------------------
# * -----------------------------------------------------------------

  # F7.1-7 Stroke type
  # G1.9-10 Infarction / PIH
  # H1.9-10 Infarction / PIH
  StrokeType = audit_measure(
    stem_name = "StrokeType",
    description = "Stroke type",
    exclusions = NULL,
    numerators = rlang::exprs(
      "Infarct" = .data[["S2StrokeTypeIsInfarct"]],
      "PIH" = !.data[["S2StrokeTypeIsInfarct"]]),
    numerator_descriptors = list(
      "Infarct" = "Infarction",
      "PIH" = "Primary intracerebral haemorrhage"),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "Infarct",
        rlang::expr(.data[["S2StrokeTypeIsInfarct"]]),
        "Infarction",
      "PIH",
        rlang::expr(!.data[["S2StrokeTypeIsInfarct"]]),
        "Primary intracerebral haemorrhage"
  ),
    csv_columns = "S2StrokeTypeIsInfarct",
    measure_type = "discrete"),

# * Modified Rankin Score -------------------------------------------
# * -----------------------------------------------------------------

  # F8.1-13 mRS before stroke
  RankinBeforeStroke = audit_measure(
    stem_name = "RankinBeforeStroke",
    description = "Pre-stroke Rankin score",
    exclusions = NULL,
    numerators = rlang::exprs(
      "0" = .data[["S2RankinBeforeStroke"]] == 0,
      "1" = .data[["S2RankinBeforeStroke"]] == 1,
      "2" = .data[["S2RankinBeforeStroke"]] == 2,
      "3" = .data[["S2RankinBeforeStroke"]] == 3,
      "4" = .data[["S2RankinBeforeStroke"]] == 4,
      "5" = .data[["S2RankinBeforeStroke"]] == 5),
    numerator_descriptors = list(
      "0" = "0", "1" = "1", "2" = "2", "3" = "3",
      "4" = "4", "5" = "5"),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "0",
        rlang::expr(.data[["S2RankinBeforeStroke"]] == 0),
      "0",
      "1",
        rlang::expr(.data[["S2RankinBeforeStroke"]] == 1),
      "1",
      "2",
        rlang::expr(.data[["S2RankinBeforeStroke"]] == 2),
      "2",
      "3",
        rlang::expr(.data[["S2RankinBeforeStroke"]] == 3),
      "3",
      "4",
        rlang::expr(.data[["S2RankinBeforeStroke"]] == 4),
      "4",
      "5",
        rlang::expr(.data[["S2RankinBeforeStroke"]] == 5),
      "5"),
    csv_columns = "S2RankinBeforeStroke",
    measure_type = "discrete"),

# * Modified Rankin Score Grouped -----------------------------------
# * -----------------------------------------------------------------

  # G1.11-13 mRS grouping before stroke
  # H1.11-13 mRS grouping before stroke
  RankinBeforeStrokeGrouped = audit_measure(
    stem_name = "RankinBeforeStrokeGrouped",
    description = "Pre-stroke Rankin score",
    exclusions = NULL,
    numerators = rlang::exprs(
      "0" = .data[["S2RankinBeforeStroke"]] == 0,
      "1Or2" = (.data[["S2RankinBeforeStroke"]] == 1) |
        (.data[["S2RankinBeforeStroke"]] == 2),
      "3OrMore" = .data[["S2RankinBeforeStroke"]] > 2),
    numerator_descriptors = list(
      "0" = "Modified Rankin Scale (mRS) score before stroke: 0",
      "1Or2" =
        "Modified Rankin Scale (mRS) score before stroke: 1 or 2",
      "3OrMore" =
        "Modified Rankin Scale (mRS) score before stroke: 3, 4 or 5"),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "0",
        rlang::expr(.data[["S2RankinBeforeStroke"]] == 0),
        "Modified Rankin Scale (mRS) score before stroke: 0",
      "1Or2",
        rlang::expr((.data[["S2RankinBeforeStroke"]] == 1) |
          (.data[["S2RankinBeforeStroke"]] == 2)),
        "Modified Rankin Scale (mRS) score before stroke: 1 or 2",
      "3OrMore",
        rlang::expr(.data[["S2RankinBeforeStroke"]] > 2),
        "Modified Rankin Scale (mRS) score before stroke: 3, 4 or 5"),
    csv_columns = "S2RankinBeforeStroke",
    measure_type = "discrete"),

# * NIHSS -----------------------------------------------------------
# * -----------------------------------------------------------------

  # LOCArrival ======================================================

  # F9.1-9 NIHSS Level of consciousness at arrival
  # Expressed as discrete values (ie. broken down by value)
  LOCArrival = audit_measure(
    stem_name = "LOCArrival",
    description = "Level of consciousness on arrival",
    exclusions = NULL,
    numerators = rlang::exprs(
      "0" = .data[["S2NihssArrivalLoc"]] == 0,
      "1" = .data[["S2NihssArrivalLoc"]] == 1,
      "2" = .data[["S2NihssArrivalLoc"]] == 2,
      "3" = .data[["S2NihssArrivalLoc"]] == 3),
    numerator_descriptors = list(
      "0" = "0", "1" = "1", "2" = "2", "3" = "3"),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "0", rlang::expr(.data[["S2NihssArrivalLoc"]] == 0), "0",
      "1", rlang::expr(.data[["S2NihssArrivalLoc"]] == 1), "1",
      "2", rlang::expr(.data[["S2NihssArrivalLoc"]] == 2), "2",
      "3", rlang::expr(.data[["S2NihssArrivalLoc"]] == 3), "3"),
    csv_columns = "S2NihssArrivalLoc",
    measure_type = "discrete"),

  # LOCArrivalGrouped ===============================================

  # G1.14-16 LOC at arrival
  # H1.14-16 LOC at arrival
  LOCArrivalGrouped = audit_measure(
    stem_name = "LOCArrivalGrouped",
    description = "Level of consciousness on arrival",
    exclusions = NULL,
    numerators = rlang::exprs(
      "0" = .data[["S2NihssArrivalLoc"]] == 0,
      "1Or2" = (.data[["S2NihssArrivalLoc"]] == 1) |
        (.data[["S2NihssArrivalLoc"]] == 2),
      "3" = .data[["S2NihssArrivalLoc"]] == 3),
    numerator_descriptors = list(
      "0" = "Alert upon arrival",
      "1Or2" = "Responds to voice or pain on arrival",
      "3" = "Unresponsive on arrival"),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "0",
        rlang::expr(.data[["S2NihssArrivalLoc"]] == 0),
        "Alert upon arrival",
      "1Or2",
        rlang::expr((.data[["S2NihssArrivalLoc"]] == 1) |
          (.data[["S2NihssArrivalLoc"]] == 2)),
        "Responds to voice or pain on arrival",
      "3",
        rlang::expr(.data[["S2NihssArrivalLoc"]] == 3),
        "Unresponsive on arrival"),
    csv_columns = "S2NihssArrivalLoc",
    measure_type = "discrete"),

  # WorstLOCFirst7Days ==============================================

  # K4.1-9 Worst LOC in first 7 days
  WorstLOCFirst7Days = audit_measure(
    stem_name = "WorstLOCFirst7Days",
    description = "Worst level of consciousness in the first 7 days from clock start",
    exclusions = NULL,
    numerators = rlang::exprs(
      "0" = .data[["S5LocWorst7Days"]] == 0,
      "1" = .data[["S5LocWorst7Days"]] == 1,
      "2" = .data[["S5LocWorst7Days"]] == 2,
      "3" = .data[["S5LocWorst7Days"]] == 3),
    numerator_descriptors = list(
      "0" = "Alert upon arrival",
      "1" = "Responds to voice",
      "2" = "Responds to pain",
      "3" = "Unresponsive on arrival"),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "0",
        rlang::expr(.data[["S5LocWorst7Days"]] == 0),
        "Alert upon arrival",
      "1",
        rlang::expr(.data[["S5LocWorst7Days"]] == 1),
        "Responds to voice",
      "2",
        rlang::expr(.data[["S5LocWorst7Days"]] == 2),
        "Responds to pain",
      "3",
        rlang::expr(.data[["S5LocWorst7Days"]] == 3),
        "Unresponsive on arrival"),
    csv_columns = "S5LocWorst7Days",
    measure_type = "discrete"),

  # NihssCompletion =================================================

  # F9.10-16 Completeness of NIHSS score
  NihssCompletion = audit_measure(
    stem_name = "NihssCompletion",
    description = "Completion of NIHSS score",
    exclusions = NULL,
    numerators = rlang::exprs(
      "LOCOnly" = !!ssnap_field[["NihssComponentsComplete"]] == 1,
      "Incomplete" = dplyr::between(
        !!ssnap_field[["NihssComponentsComplete"]], 2, 14),
      "Complete" = !!ssnap_field[["NihssComponentsComplete"]] == 15),
    numerator_descriptors = list(
      "LOCOnly" = "Level of consciousness (AVPU) only",
      "Incomplete" = "Incomplete",
      "Complete" = "Complete"),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "LOCOnly",
        rlang::expr(!!ssnap_field[["NihssComponentsComplete"]] == 1),
        "Level of consciousness (AVPU) only",
      "Incomplete",
        rlang::expr(dplyr::between(
          !!ssnap_field[["NihssComponentsComplete"]], 2, 14)),
        "Incomplete",
      "Complete",
        rlang::expr(!!ssnap_field[["NihssComponentsComplete"]] == 15),
        "Complete"),
    csv_columns = c("S2NihssArrivalLoc",
                    "S2NihssArrivalLocQuestions",
                    "S2NihssArrivalLocCommands",
                    "S2NihssArrivalBestGaze",
                    "S2NihssArrivalVisual",
                    "S2NihssArrivalFacialPalsy",
                    "S2NihssArrivalMotorArmLeft",
                    "S2NihssArrivalMotorArmRight",
                    "S2NihssArrivalMotorLegLeft",
                    "S2NihssArrivalMotorLegRight",
                    "S2NihssArrivalLimbAtaxia",
                    "S2NihssArrivalSensory",
                    "S2NihssArrivalBestLanguage",
                    "S2NihssArrivalDysarthria",
                    "S2NihssArrivalExtinctionInattention"),
    measure_type = "discrete"),

  # NihssOnArrival ==================================================

  # F9.28-30 NIHSS spread
  # G1.17 Median NIHSS on arrival
  # H1.17 Median NIHSS on arrival
  NihssOnArrival = audit_measure(
    stem_name = "NihssOnArrival",
    description = "NIHSS score on arrival",
    exclusions = NULL,
    numerators = rlang::expr(.data[["S2NihssArrival"]]),
    new_numerators = rlang::expr(.data[["S2NihssArrival"]]),
    csv_columns = "S2NihssArrival",
#    numerator_descriptors = list(
#      "NIHSS at arrival if fully completed"),
    measure_type = "continuous"),

  # NihssOnArrivalGrouped ===========================================

  # F9.17-27 NIHSS at arrival
  NihssOnArrivalGrouped = audit_measure(
    stem_name = "NihssOnArrivalGrouped",
    description = "NIHSS score on arrival",
    exclusions = NULL,
    numerators = rlang::exprs(
      "0" = .data[["S2NihssArrival"]] == 0,
      "1To4" = dplyr::between(.data[["S2NihssArrival"]], 1, 4),
      "5To15" = dplyr::between(.data[["S2NihssArrival"]], 5, 15),
      "16To20" = dplyr::between(.data[["S2NihssArrival"]], 16, 20),
      "21OrMore" = .data[["S2NihssArrival"]] >= 21),
    numerator_descriptors = list(
      "0" = "0",
      "1To4" = "1 - 4",
      "5To15" = "5 - 15",
      "16To20" = "16 - 20",
      "21OrMore" = "21 or more"),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "0",
        rlang::expr(.data[["S2NihssArrival"]] == 0),
        "0",
      "1To4",
        rlang::expr(dplyr::between(.data[["S2NihssArrival"]], 1, 4)),
        "1 - 4",
      "5To15",
        rlang::expr(dplyr::between(.data[["S2NihssArrival"]], 5, 15)),
        "5 - 15",
      "16To20",
        rlang::expr(dplyr::between(.data[["S2NihssArrival"]], 16, 20)),
        "16 - 20",
      "21OrMore",
        rlang::expr(.data[["S2NihssArrival"]] >= 21),
        "21 or more"),
    csv_columns = "S2NihssArrival",
    measure_type = "discrete"),

  # TODO NIHSS LOC if NIHSS incomplete

# * -----------------------------------------------------------------
# * Palliative Care -------------------------------------------------
# * -----------------------------------------------------------------

  # EndOfLifeWithin72hrs ============================================

  # F10.1-3 Palliative care in 72hrs
  # G1.18 Palliative care in 72hrs
  # H1.18 Palliative care in 72hrs
  EndOfLifeWithin72hrs = audit_measure(
    stem_name = "EndOfLifeWithin72hrs",
    description = "End of life care within 72hrs",
    exclusions = NULL,
    numerators = rlang::expr(!is.na(
      .data[["S3PalliativeCareDecisionDate"]])),
    new_numerators = rlang::expr(!is.na(
      .data[["S3PalliativeCareDecisionDate"]])),
    csv_columns = "S3PalliativeCareDecisionDate",
    measure_type = "discrete"),

  # EndOfLifePathway ================================================

  # F10.4-6 End of life pathway use
  EndOfLifePathway = audit_measure(
    stem_name = "EndOfLifePathway",
    description = "Is the patient on an end of life pathway",
    exclusions = rlang::expr(is.na(
      .data[["S3PalliativeCareDecisionDate"]])),
    numerators = rlang::expr(!is.na(
      .data[["S3EndOfLifePathway"]])),
    new_numerators =
      rlang::expr(!is.na(.data[["S3EndOfLifePathway"]])),
    csv_columns = c("S3EndOfLifePathway",
                    "S3PalliativeCareDecisionDate"),
    measure_type = "discrete"),

  # ClockStartToPalliativeCare72Hours ===============================

  # F10.7 Clock start to palliative care decision (days) spread
  ClockStartToPalliativeCare72Hours = audit_measure(
    stem_name = "ClockStartToPalliativeCare72Hours",
    description = "Is the patient on an end of life pathway",
    exclusions = NULL,
    numerators = ssnap_field[["ClockStartToPalliativeCare72Days"]],
    new_numerators =
      ssnap_field[["ClockStartToPalliativeCare72Days"]],
    csv_columns = c("S1PatientClockStartDateTime",
                    "S3PalliativeCareDecisionDate"),
    measure_type = "continuous"),

# * -----------------------------------------------------------------
# * In hospital stroke ----------------------------------------------
# * -----------------------------------------------------------------

  # F11.1-3 Was the stroke in hospital?
  OnsetInHospital = audit_measure(
    stem_name = "OnsetInHospital",
    description = "Onset in hospital",
    exclusions = NULL,
    numerators = rlang::expr(!is.na(.data[["S1OnsetInHospital"]])),
    new_numerators =
      rlang::expr(!is.na(.data[["S1OnsetInHospital"]])),
    csv_columns = "S1OnsetInHospital",
    measure_type = "discrete"),

# * Onset of symptoms -----------------------------------------------
# * -----------------------------------------------------------------

  # OnsetDateAccuracy ===============================================

  # G2.1-2.7 Date of symptom onset
  # H2.1-2.7 Date of symptom onset
  OnsetDateAccuracy = audit_measure(
    stem_name = "OnsetDate",
    description = "Accuracy of onset date",
    exclusions = NULL,
    numerators = rlang::exprs(
      "Precise" = .data[["S1OnsetDateIsPrecise"]],
      "Estimate" = !.data[["S1OnsetDateIsPrecise"]],
      "DuringSleep" = is.na(.data[["S1OnsetDateIsPrecise"]])),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "Precise",
        rlang::expr(.data[["S1OnsetDateIsPrecise"]]),
        "Precise",
      "Estimate",
        rlang::expr(!.data[["S1OnsetDateIsPrecise"]]),
        "Best estimate",
      "DuringSleep",
        rlang::expr(is.na(.data[["S1OnsetDateIsPrecise"]])),
        "During sleep"),
    csv_columns = "S1OnsetDateIsPrecise",
    measure_type = "discrete"),

  # OnsetTimeAccuracy ===============================================

  # G2.8-14 Time of symptom onset
  # H2.8-14 Time of symptom onset
  OnsetTimeAccuracy = audit_measure(
    stem_name = "OnsetTime",
    description = "Accuracy of onset time",
    exclusions = NULL,
    numerators = rlang::exprs(
      "Precise" = .data[["S1OnsetTimeIsPrecise"]],
      "Estimate" = !.data[["S1OnsetTimeIsPrecise"]],
      "DuringSleep" = is.na(.data[["S1OnsetTimeIsPrecise"]])),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "Precise",
        rlang::expr(.data[["S1OnsetTimeIsPrecise"]]),
        "Precise",
      "Estimate",
        rlang::expr(!.data[["S1OnsetTimeIsPrecise"]]),
        "Best estimate",
      "DuringSleep",
        rlang::expr(is.na(.data[["S1OnsetTimeIsPrecise"]])),
        "During sleep"),
    csv_columns = "S1OnsetTimeIsPrecise",
    measure_type = "discrete"),

  # OnsetTimeKnown ==================================================

  # G2.15-17 Time of symptom onset known
  # H2.15-17 Time of symptom onset known
  OnsetTimeKnown = audit_measure(
    stem_name = "OnsetTimeKnown",
    description = "Onset time is known",
    exclusions = NULL,
    numerators = rlang::expr(!is.na(.data[["S1OnsetTimeIsPrecise"]])),
    new_numerators =
      rlang::expr(!is.na(.data[["S1OnsetTimeIsPrecise"]])),
    csv_columns = "S1OnsetTimeIsPrecise",
    measure_type = "discrete"),

  # OnsetToArrivalGrouped ===========================================

  # H2.18-28 Onset to arrival times (grouped)
  OnsetToArrivalGrouped = audit_measure(
    stem_name = "OnsetToArrival",
    description = "Onset to arrival",
    exclusions = NULL,
    numerators = rlang::exprs(
      "Within4hrs" = !!ssnap_field[["OnsetToArrivalTimeMins"]]
        <= (4 * 60),
      "4To12hrs" = dplyr::between(
        !!ssnap_field[["OnsetToArrivalTimeMins"]],
        (4 * 60), (12 * 60)),
      "12To24hrs" = dplyr::between(
        !!ssnap_field[["OnsetToArrivalTimeMins"]],
        (12 * 60), (24 * 60)),
      "24To72hrs" = dplyr::between(
        !!ssnap_field[["OnsetToArrivalTimeMins"]],
        (24 * 60), (72 * 60)),
      "Over72hrs" = !!ssnap_field[["OnsetToArrivalTimeMins"]]
        > (72 * 60)),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "Within4hrs",
        rlang::expr(
          !!ssnap_field[["OnsetToArrivalTimeMins"]] <= (4 * 60)),
        "Within 4 hours",
      "4To12hrs",
        rlang::expr(dplyr::between(
          !!ssnap_field[["OnsetToArrivalTimeMins"]],
          (4 * 60), (12 * 60))),
        "4 to 12 hours",
      "12To24hrs",
        rlang::expr(dplyr::between(
          !!ssnap_field[["OnsetToArrivalTimeMins"]],
          (12 * 60), (24 * 60))),
        "12 to 24 hours",
      "24To72hrs",
        rlang::expr(dplyr::between(
          !!ssnap_field[["OnsetToArrivalTimeMins"]],
          (24 * 60), (72 * 60))),
        "24 to 72 hours",
      "Over72hrs",
        rlang::expr(
          !!ssnap_field[["OnsetToArrivalTimeMins"]] > (72 * 60)),
        "Over 72 hours"),
    csv_columns = c("S1OnsetInHospital",
                    "S1OnsetDateIsPrecise",
                    "S1OnsetTimeIsPrecise",
                    "S1OnsetDateTime",
                    "S1PatientClockStartDateTime"),
    measure_type = "discrete"),

  # OnsetToArrivalTimeMins ==========================================

  OnsetToArrivalTimeMins = audit_measure(
    stem_name = "OnsetToArrivalMins",
    description = "Onset to arrival",
    exclusions = NULL,
    numerators = ssnap_field[["OnsetToArrivalTimeMins"]],
    new_numerators = ssnap_field[["OnsetToArrivalTimeMins"]],
    csv_columns = c("S1OnsetInHospital",
                    "S1OnsetDateIsPrecise",
                    "S1OnsetTimeIsPrecise",
                    "S1OnsetDateTime",
                    "S1PatientClockStartDateTime"),
    measure_type = "continuous"),

  # OnsetToFirstStrokeUnitMins ======================================

  OnsetToFirstStrokeUnitMins = audit_measure(
    stem_name = "OnsetToFirstStrokeUnitMins",
    description = "Onset to first stroke unit",
    exclusions = NULL,
    numerators = ssnap_field[["OnsetToFirstStrokeUnitMins"]],
    new_numerators = ssnap_field[["OnsetToFirstStrokeUnitMins"]],
    csv_columns = c("S1OnsetDateTime",
                    "S1FirstStrokeUnitArrivalDateTime"),
    measure_type = "continuous"),

  # OnsetToBrainImagingMins =========================================

  OnsetToBrainImagingMins = audit_measure(
    stem_name = "OnsetToBrainImagingMins",
    description = "Onset to brain imaging",
    exclusions = NULL,
    numerators = ssnap_field[["OnsetToBrainImagingMins"]],
    new_numerators = ssnap_field[["OnsetToBrainImagingMins"]],
    csv_columns = c("S1OnsetDateTime",
                    "S2BrainImagingDateTime"),
    measure_type = "continuous"),

  # OnsetToThrombolysisMins =========================================

  OnsetToThrombolysisMins = audit_measure(
    stem_name = "OnsetToThrombolysisMins",
    description = "Onset to thrombolysis",
    exclusions = NULL,
    numerators = ssnap_field[["OnsetToThrombolysisMins"]],
    new_numerators = ssnap_field[["OnsetToThrombolysisMins"]],
    csv_columns = c("S1OnsetDateTime",
                    "S2ThrombolysisDateTime"),
    measure_type = "continuous"),

# * -----------------------------------------------------------------
# * Ambulance -------------------------------------------------------
# * -----------------------------------------------------------------

  ArrivalByAmbulance = audit_measure(
    stem_name = "ArrivalByAmbulance",
    description = "Arrival by ambulance",
    exclusions = rlang::expr(.data[["S1OnsetInHospital"]]),
    numerators = rlang::expr(.data[["S1ArriveByAmbulance"]]),
    new_numerators = rlang::expr(.data[["S1ArriveByAmbulance"]]),
    csv_columns = c("S1ArriveByAmbulance",
                    "S1OnsetInHospital"),
    measure_type = "discrete"),

# * Time and day of arrival -----------------------------------------
# * -----------------------------------------------------------------

#TODO Time and day of arrival needs first arrival breaking down to
# In hours, out of hours, weekends and bank holidays

# TODO DayTypeOfArrival needs to also include bank holidays.
# Output also should be a factor rather than character
#DayTypeOfArrival = dplyr::case_when(
#  .data[["S1OnsetInHospital"]] ~ NA_character_,
#  lubridate::wday(.data[["S1FirstArrivalDateTime"]]) > 5 ~
#    "Weekend",
#  TRUE ~ "Weekday"),
#TimeRangeOfArrival = dplyr::case_when(
#  .data[["S1OnsetInHospital"]] ~ NA_character_,
#  dplyr::between(lubridate::hour(.data[["S1FirstArrivalDateTime"]]),
#                 8, 18) ~ "In-hours",
#  TRUE ~ "Out of hours"
#),


# * Scanning --------------------------------------------------------
# * -----------------------------------------------------------------

  # ClockStartToBrainImagingMins ====================================

  # G6.4-6 Clock start to first brain imaging
  # H6.4-6 Clock start to first brain imaging
  ClockStartToBrainImagingMins = audit_measure(
    stem_name = "ClockStartToBrainImagingMins",
    description = "Clock start to first brain imaging (mins)",
    exclusions = NULL,
    numerators = ssnap_field[["ClockStartToBrainImagingMins"]],
    new_numerators = ssnap_field[["ClockStartToBrainImagingMins"]],
    csv_columns = c("S1PatientClockStartDateTime",
                    "S2BrainImagingDateTime"),
    measure_type = "continuous"),

  # BrainImagingGrouped =============================================

  # G6.1-3 Scanned
  # H6.1-3 Scanned
  # G6.7-9 Scanned within 1 hour of clock start
  # H6.7-9 Scanned within 1 hour of clock start
  # G6.10-12 Scanned within 12 hours of clock start
  # H6.10-12 Scanned within 12 hours of clock start
  # G6.13-15 Scanned within 24 hours of clock start
  # H6.13-15 Scanned within 24 hours of clock start
  BrainImagingGrouped = audit_measure(
    stem_name = "BrainImaging",
    description = "Brain imaging",
    exclusions = NULL,
    numerators = rlang::exprs(
      "Scanned" = !is.na(
        !! ssnap_field[["ClockStartToBrainImagingMins"]]),
      "Within1hr" = !! ssnap_field[["ClockStartToBrainImagingMins"]]
        <= 60,
      "Within12hrs" = !! ssnap_field[["ClockStartToBrainImagingMins"]]
        <= (60 * 12),
      "Within24hrs" = !! ssnap_field[["ClockStartToBrainImagingMins"]]
        <= (60 * 24)),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "Scanned", rlang::expr(
        !is.na(!! ssnap_field[["ClockStartToBrainImagingMins"]])),
        "Scanned",
      "Within1hr", rlang::expr(
        !! ssnap_field[["ClockStartToBrainImagingMins"]] <= 60),
        "Scanned within 1hr",
      "Within12hrs", rlang::expr(
        !! ssnap_field[["ClockStartToBrainImagingMins"]] <= (60 * 12)),
        "Scanned within 12hrs",
      "Within24hrs", rlang::expr(
        !! ssnap_field[["ClockStartToBrainImagingMins"]] <= (60 * 24)),
        "Scanned within 24hrs"),
    csv_columns = c("S1PatientClockStartDateTime",
                    "S2BrainImagingDateTime"),
    measure_type = "discrete"),

# * -----------------------------------------------------------------
# * Admission to stroke unit -----------------------------------------
# * -----------------------------------------------------------------

  # WentToAStrokeUnit ===============================================

  # G7.1-3 Patient went to a stroke unit (SU) at any point at the
  # first admitting hospital
  # H7.1-3 Patient went to a stroke unit (SU) at any point at the
  # first admitting hospital
  WentToAStrokeUnit = audit_measure(
    stem_name = "WentToAStrokeUnit",
    description = "Went to a stroke unit",
    exclusions = NULL,
    numerators = rlang::expr(!is.na(
      !! ssnap_field[["ClockStartToFirstStrokeUnitMins"]])),
    new_numerators = rlang::expr(!is.na(
      !! ssnap_field[["ClockStartToFirstStrokeUnitMins"]])),
    csv_columns = c("S1PatientClockStartDateTime",
                    "S1FirstStrokeUnitArrivalDateTime"),
    measure_type = "discrete"),

  # ClockStartToFirstStrokeUnitMins =================================

  # G7.4-6 Clock start to first stroke unit admission (mins)
  # H7.4-6 Clock start to first stroke unit admission (mins)
  ClockStartToFirstStrokeUnitMins = audit_measure(
    stem_name = "ClockStartToFirstStrokeUnitMins",
    description = "Clock start to first stroke unit (mins)",
    exclusions = NULL,
    numerators = ssnap_field[["ClockStartToFirstStrokeUnitMins"]],
    new_numerators =
      ssnap_field[["ClockStartToFirstStrokeUnitMins"]],
    csv_columns = c("S1PatientClockStartDateTime",
                    "S1FirstStrokeUnitArrivalDateTime"),
    measure_type = "continuous"),

  # FirstWard =======================================================

  # G7.7-15 First ward of admission
  # H7.7-15 First ward of admission
  FirstWard = audit_measure(
    stem_name = "FirstWard",
    description = "First ward",
    exclusions = NULL,
    numerators = rlang::exprs(
      "MAU_AAU_CDU" = (.data[["S1FirstWard"]] == "MAC"),
      "StrokeUnit"  = (.data[["S1FirstWard"]] == "SU"),
      "ICU_CCU_HDU" = (.data[["S1FirstWard"]] == "ICH"),
      "Other"       = (.data[["S1FirstWard"]] == "O")),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "MAU_AAU_CDU",
        rlang::expr(.data[["S1FirstWard"]] == "MAC"),
        "Admitted to an assessment area",
      "StrokeUnit",
        rlang::expr(.data[["S1FirstWard"]] == "SU"),
        "Admitted to a stroke unit",
      "ICU_CCU_HDU",
        rlang::expr(.data[["S1FirstWard"]] == "ICH"),
        "Admitted to a high dependency area (ICU, CCU, HDU)",
      "Other",
        rlang::expr(.data[["S1FirstWard"]] == "O"),
        "Other"),
    csv_columns = "S1FirstWard",
    measure_type = "discrete"),

  # FirstSUWithin4hrs ===============================================

  # G 7.16.1-18.1 First stroke unit within 4 hours
  # H 7.16.1-18.1 First stroke unit within 4 hours
  FirstSUWithin4hrs = audit_measure(
    stem_name = "FirstSUWithin4hrs",
    description = "First stroke unit within 4 hours",
    exclusions = rlang::expr( (.data[["S1FirstWard"]] == "ICH") |
      (!is.na(.data[["S2IAI"]]) & .data[["S2IAI"]])),
    numerators = rlang::expr(.data[["S1FirstWard"]] == "SU" &
      (!! ssnap_field[["ClockStartToFirstStrokeUnitMins"]]
      <= (4 * 60))),
    new_numerators =
      rlang::expr(.data[["S1FirstWard"]] == "SU" &
        (!! ssnap_field[["ClockStartToFirstStrokeUnitMins"]]
        <= (4 * 60))),
    csv_columns = c("S1FirstWard",
                    "S1PatientClockStartDateTime",
                    "S1FirstStrokeUnitArrivalDateTime",
                    "S1FirstWard",
                    "S2IAI"),
    measure_type = "discrete"),

  # TODO Out of hours and SU within 4 hrs.

# * -----------------------------------------------------------------
# * Stroke nurse assessment -----------------------------------------
# * -----------------------------------------------------------------

  # StrokeNurseReview ===============================================

  # G 8.1-3 Stroke nurse within 24hrs
  # H 8.1-3 Stroke nurse within 24hrs
  # G 8.4-6 Stroke nurse within 72hrs
  # H 8.4-6 Stroke nurse within 72hrs
  # G 8.7-13 Stroke nurse review times grouped by time
  # H 8.7-13 Stroke nurse review times grouped by time
  StrokeNurseReview = audit_measure(
    stem_name = "StrokeNurseReview",
    description = "Onset to stroke nurse review",
    exclusions = NULL,
    numerators = rlang::exprs(
      "Within12hrs" = !! ssnap_field[["ClockStartToStrokeNurseMins"]]
        <= (12 * 60),
      "Within24hrs" = !! ssnap_field[["ClockStartToStrokeNurseMins"]]
        <= (24 * 60),
      "Within72hrs" = !! ssnap_field[["ClockStartToStrokeNurseMins"]]
        <= (72 * 60),
      "12To24hrs" = dplyr::between(
        !! ssnap_field[["ClockStartToStrokeNurseMins"]],
        (12 * 60), (24 * 60)),
      "24To72hrs" = dplyr::between(
        !! ssnap_field[["ClockStartToStrokeNurseMins"]],
        (24 * 60), (72 * 60))),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "Within12hrs", rlang::expr(
        !! ssnap_field[["ClockStartToStrokeNurseMins"]] <= (12 * 60)),
        "Within 12 hours",
      "Within24hrs", rlang::expr(
        !! ssnap_field[["ClockStartToStrokeNurseMins"]] <= (24 * 60)),
        "Within 24 hours",
      "Within72hrs", rlang::expr(
        !! ssnap_field[["ClockStartToStrokeNurseMins"]] <= (72 * 60)),
        "Within 72 hours",
      "12To24hrs",
        rlang::expr(dplyr::between(
          !! ssnap_field[["ClockStartToStrokeNurseMins"]],
          (12 * 60), (24 * 60))),
        "In 12 to 24 hours",
      "24To72hrs",
        rlang::expr(dplyr::between(
          !! ssnap_field[["ClockStartToStrokeNurseMins"]],
          (24 * 60), (72 * 60))),
        "In 24 to 72 hours"),
    csv_columns = c("S1PatientClockStartDateTime",
                    "S3StrokeNurseAssessedDateTime"),
    measure_type = "discrete"),

  # ClockStartToStrokeNurseMins =====================================

  # G 8.14-16 Clock start to stroke nurse (minutes)
  # H 8.14-16 Clock start to stroke nurse (minutes)
  ClockStartToStrokeNurseMins = audit_measure(
    stem_name = "ClockStartToStrokeNurseMins",
    description = "Clock start to stroke nurse (mins)",
    exclusions = NULL,
    numerators = ssnap_field[["ClockStartToStrokeNurseMins"]],
    new_numerators = ssnap_field[["ClockStartToStrokeNurseMins"]],
    csv_columns = c("S1PatientClockStartDateTime",
                    "S3StrokeNurseAssessedDateTime"),
    measure_type = "continuous"),


# * -----------------------------------------------------------------
# * Stroke consultant assessment ------------------------------------
# * -----------------------------------------------------------------

  # StrokeConsultantReviewGrouped ===================================

  # G 9.1-3 Stroke consultant within 24hrs
  # H 9.1-3 Stroke consultant within 24hrs
  # G 9.4-6 Stroke consultant within 72hrs
  # H 9.4-6 Stroke consultant within 72hrs
  # G 9.7-13 Stroke consultant review times grouped by time
  # H 9.7-13 Stroke consultant review times grouped by time
  # G 9.17-19 Stroke consultant within 14hrs
  # H 9.17-19 Stroke consultant within 14hrs
  StrokeConsultantReview = audit_measure(
    stem_name = "StrokeConsultantReview",
    description = "Onset to consultant review",
    exclusions = NULL,
    numerators = rlang::exprs(
      "Within12hrs" = !! ssnap_field[["ClockStartToConsultantMins"]]
      <= (12 * 60),
      "Within14hrs" = !! ssnap_field[["ClockStartToConsultantMins"]]
      <= (14 * 60),
      "Within24hrs" = !! ssnap_field[["ClockStartToConsultantMins"]]
      <= (24 * 60),
      "Within72hrs" = !! ssnap_field[["ClockStartToConsultantMins"]]
      <= (72 * 60),
      "12To24hrs" = dplyr::between(
        !! ssnap_field[["ClockStartToConsultantMins"]],
        (12 * 60), (24 * 60)),
      "24To72hrs" = dplyr::between(
        !! ssnap_field[["ClockStartToConsultantMins"]],
        (24 * 60), (72 * 60))),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "Within12hrs",
      rlang::expr(!! ssnap_field[["ClockStartToConsultantMins"]] <= (12 * 60)),
      "Within 12 hours",
      "Within14hrs",
      rlang::expr(!! ssnap_field[["ClockStartToConsultantMins"]] <= (14 * 60)),
      "Within 14 hours",
      "Within24hrs",
      rlang::expr(!! ssnap_field[["ClockStartToConsultantMins"]] <= (24 * 60)),
      "Within 24 hours",
      "Within72hrs",
      rlang::expr(!! ssnap_field[["ClockStartToConsultantMins"]] <= (72 * 60)),
      "Within 72 hours",
      "12To24hrs", rlang::expr(dplyr::between(
        !! ssnap_field[["ClockStartToConsultantMins"]], (12 * 60), (24 * 60))),
      "12 to 24 hours",
      "24To72hrs", rlang::expr(dplyr::between(
        !! ssnap_field[["ClockStartToConsultantMins"]], (24 * 60), (72 * 60))),
      "24 to 72 hours"),
    csv_columns = c("S1PatientClockStartDateTime",
                    "S3StrokeConsultantAssessedDateTime"),
    measure_type = "discrete"),

  # ClockStartToConsultantMins ======================================

  # G 9.14-16 Clock start to stroke consultant (minutes)
  # H 9.14-16 Clock start to stroke consultant (minutes)
  ClockStartToConsultantMins = audit_measure(
    stem_name = "ClockStartToConsultantMins",
    description = "Clock start to consultant review (mins)",
    exclusions = NULL,
    numerators = ssnap_field[["ClockStartToConsultantMins"]],
    new_numerators = ssnap_field[["ClockStartToConsultantMins"]],
    csv_columns = c("S1PatientClockStartDateTime",
                    "S3StrokeConsultantAssessedDateTime"),
    measure_type = "continuous"),

# * -----------------------------------------------------------------
# * Occupational therapy --------------------------------------------
# * -----------------------------------------------------------------

  # OccTherReviewGrouped ============================================

  # G 10.1-15 Occupational therapy assessment breakdown
  # H 10.1-15 Occupational therapy assessment breakdown
  OccTherReviewGrouped = audit_measure(
    stem_name = "OccTherReview",
    description = "Onset to occupational therapy initial assessment",
    exclusions = NULL,
    numerators = rlang::exprs(
      "Within24hrs" = !! ssnap_field[["ClockStartToOTMins"]]
        <= (24 * 60),
      "24To72hrs" = dplyr::between(
        !! ssnap_field[["ClockStartToOTMins"]], (24 * 60), (72 * 60)),
      "NotDoneOrg" =
        .data[["S3OccTherapist72HrsNotAssessedReason"]] == "OR",
      "NotDoneRefused" =
        .data[["S3OccTherapist72HrsNotAssessedReason"]] == "PR",
      "NotDoneUnwell" =
        .data[["S3OccTherapist72HrsNotAssessedReason"]] == "MU",
      "NotDoneNoDeficit" =
        .data[["S3OccTherapist72HrsNotAssessedReason"]] == "ND",
      "NotDoneNotKnown" =
        .data[["S3OccTherapist72HrsNotAssessedReason"]] == "NK"),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "Within24hrs",
      rlang::expr(!! ssnap_field[["ClockStartToOTMins"]] <= (24 * 60)),
      "Within 24 hours",
      "24To72hrs", rlang::expr(dplyr::between(
        !! ssnap_field[["ClockStartToOTMins"]], (24 * 60), (72 * 60))),
      "24 to 72 hours",
      "NotDoneOrg", rlang::expr(
        .data[["S3OccTherapist72HrsNotAssessedReason"]] == "OR"),
      "Not done (organisational reasons)",
      "NotDoneRefused", rlang::expr(
        .data[["S3OccTherapist72HrsNotAssessedReason"]] == "PR"),
      "Not done (patient refused)",
      "NotDoneUnwell", rlang::expr(
        .data[["S3OccTherapist72HrsNotAssessedReason"]] == "MU"),
      "Not done (unwell)",
      "NotDoneNoDeficit", rlang::expr(
        .data[["S3OccTherapist72HrsNotAssessedReason"]] == "ND"),
      "Not done (no deficit)",
      "NotDoneNotKnown", rlang::expr(
        .data[["S3OccTherapist72HrsNotAssessedReason"]] == "NK"),
      "Not done (not known)"),
    csv_columns = c("S1PatientClockStartDateTime",
                    "S3OccTherapist72HrsDateTime",
                    "S3OccTherapist72HrsNotAssessedReason"),
    measure_type = "discrete"),

  # ClockStartToOTMins ==============================================

  # G 10.16-18 Clock start to occupational therapy assessment (mins)
  # H 10.16-18 Clock start to occupational therapy assessment (mins)
  ClockStartToOTMins = audit_measure(
    stem_name = "ClockStartToOTMins",
    description = "Clock start to initial OT assessment (mins)",
    exclusions = NULL,
    numerators = ssnap_field[["ClockStartToOTMins"]],
    new_numerators = ssnap_field[["ClockStartToOTMins"]],
    csv_columns = c("S1PatientClockStartDateTime",
                    "S3OccTherapist72HrsDateTime"),
    measure_type = "continuous"),

  # OccTherApplicability ============================================

  # G 10.19-21 Applicability for occupational therapy
  # H 10.19-21 Applicability for occupational therapy
  OccTherApplicability = audit_measure(
    stem_name = "OccTherApplicability",
    description = "Applicability for occupational therapy",
    exclusions = NULL,
    numerators = rlang::expr(!(
      .data[["S3OccTherapist72HrsNotAssessedReason"]]
      %in% c("PR", "PU", "ND"))),
    new_numerators = rlang::expr(!(
      .data[["S3OccTherapist72HrsNotAssessedReason"]]
      %in% c("PR", "PU", "ND"))),
    csv_columns = "S3OccTherapist72HrsNotAssessedReason",
    measure_type = "discrete"),

  # OccTherApplicableWithin72hrs ====================================

  # G 10.22-24 If applicable, OT assessment within 72 hours
  # H 10.22-24 If applicable, OT assessment within 72 hours
  OccTherApplicableWithin72hrs = audit_measure(
    stem_name = "OccTherApplicableWithin72hrs",
    description = "Applicability for occupational therapy",
    exclusions = rlang::expr(
      .data[["S3OccTherapist72HrsNotAssessedReason"]]
      %in% c("PR", "PU", "ND")),
    numerators = rlang::expr(!! ssnap_field[["ClockStartToOTMins"]]
      <= (72 * 60)),
    new_numerators =
      rlang::expr(!! ssnap_field[["ClockStartToOTMins"]] <= (72 * 60)),
    csv_columns = c("S1PatientClockStartDateTime",
                    "S3OccTherapist72HrsDateTime",
                    "S3OccTherapist72HrsNotAssessedReason"),
    measure_type = "discrete"),

  # OccTherApplicableWithin24hrs ====================================

  # G 10.25-27 If applicable, OT assessment within 24 hours
  # H 10.25-27 If applicable, OT assessment within 24 hours
  OccTherApplicableWithin24hrs = audit_measure(
    stem_name = "OccTherApplicableWithin24hrs",
    description = "Applicability for occupational therapy",
    exclusions = rlang::expr(
      .data[["S3OccTherapist72HrsNotAssessedReason"]]
      %in% c("PR", "PU", "ND")),
    numerators = rlang::expr(!! ssnap_field[["ClockStartToOTMins"]]
      <= (24 * 60)),
    new_numerators =
      rlang::expr(!! ssnap_field[["ClockStartToOTMins"]] <= (24 * 60)),
    csv_columns = c("S1PatientClockStartDateTime",
                    "S3OccTherapist72HrsDateTime",
                    "S3OccTherapist72HrsNotAssessedReason"),
    measure_type = "discrete"),

  # OTAssessment72HrsComplete =======================================

  # C6.4-6.6 Reason for no OT assessment within 72hrs is known
  OTAssessment72HrsComplete = audit_measure(
    stem_name = "OTAssessment72HrsComplete",
    description = "OT assessment within 72hrs known",
    exclusions = NULL,
    numerators = rlang::expr(
      (.data[["S3OccTherapist72HrsNotAssessedReason"]] != "NK") |
      is.na(.data[["S3OccTherapist72HrsNotAssessedReason"]])),
    new_numerators = rlang::expr(
      (.data[["S3OccTherapist72HrsNotAssessedReason"]] != "NK") |
        is.na(.data[["S3OccTherapist72HrsNotAssessedReason"]])),
    csv_columns = "S3OccTherapist72HrsNotAssessedReason",
    measure_type = "discrete"),

# * -----------------------------------------------------------------
# * Physiotherapy ---------------------------------------------------
# * -----------------------------------------------------------------

  # PhysioReviewGrouped =============================================

  # G 11.1-15 Physiotherapy assessment breakdown
  # H 11.1-15 Physiotherapy assessment breakdown
  PhysioReviewGrouped = audit_measure(
    stem_name = "PhysioReview",
    description = "Onset to physiotherapy initial assessment",
    exclusions = NULL,
    numerators = rlang::exprs(
      "Within24hrs" = !! ssnap_field[["ClockStartToPTMins"]]
        <= (24 * 60),
      "24To72hrs" = dplyr::between(
        !! ssnap_field[["ClockStartToPTMins"]], (24 * 60), (72 * 60)),
      "NotDoneOrg" =
        .data[["S3Physio72HrsNotAssessedReason"]] == "OR",
      "NotDoneRefused" =
        .data[["S3Physio72HrsNotAssessedReason"]] == "PR",
      "NotDoneUnwell" =
        .data[["S3Physio72HrsNotAssessedReason"]] == "MU",
      "NotDoneNoDeficit" =
        .data[["S3Physio72HrsNotAssessedReason"]] == "ND",
      "NotDoneNotKnown" =
        .data[["S3Physio72HrsNotAssessedReason"]] == "NK"),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "Within24hrs", rlang::expr(
        !! ssnap_field[["ClockStartToPTMins"]] <= (24 * 60)),
        "Within 24 hours",
      "24To72hrs", rlang::expr(dplyr::between(
        !! ssnap_field[["ClockStartToPTMins"]], (24 * 60), (72 * 60))),
        "24 to 72 hours",
      "NotDoneOrg", rlang::expr(
        .data[["S3Physio72HrsNotAssessedReason"]] == "OR"),
        "Not done (organisational reasons)",
      "NotDoneRefused", rlang::expr(
        .data[["S3Physio72HrsNotAssessedReason"]] == "PR"),
        "Not done (patient refused)",
      "NotDoneUnwell", rlang::expr(
        .data[["S3Physio72HrsNotAssessedReason"]] == "MU"),
        "Not done (medically unwell)",
      "NotDoneNoDeficit", rlang::expr(
        .data[["S3Physio72HrsNotAssessedReason"]] == "ND"),
        "Not done (no deficits)",
      "NotDoneNotKnown", rlang::expr(
        .data[["S3Physio72HrsNotAssessedReason"]] == "NK"),
        "Not done (not known)"),
    csv_columns = c("S1PatientClockStartDateTime",
                    "S3Physio72HrsDateTime",
                    "S3Physio72HrsNotAssessedReason"),
    measure_type = "discrete"),

  # ClockStartToPTMins ==============================================

  # G 11.16-18 Clock start to physiotherapy assessment (mins)
  # H 11.16-18 Clock start to physiotherapy assessment (mins)
  ClockStartToPTMins = audit_measure(
    stem_name = "ClockStartToPTMins",
    description = "Clock start to initial PT assessment (mins)",
    exclusions = NULL,
    numerators = ssnap_field[["ClockStartToPTMins"]],
    new_numerators = ssnap_field[["ClockStartToPTMins"]],
    csv_columns = c("S1PatientClockStartDateTime",
                    "S3Physio72HrsDateTime"),
    measure_type = "continuous"),

  # PhysioApplicability =============================================

  # G 11.19-21 Applicability for physiotherapy
  # H 11.19-21 Applicability for physiotherapy
  PhysioApplicability = audit_measure(
    stem_name = "PhysioApplicability",
    description = "Applicability for physiotherapy",
    exclusions = NULL,
    numerators = rlang::expr(!(
      .data[["S3Physio72HrsNotAssessedReason"]]
      %in% c("PR", "PU", "ND"))),
    new_numerators = rlang::expr(!(
      .data[["S3Physio72HrsNotAssessedReason"]]
      %in% c("PR", "PU", "ND"))),
    csv_columns = "S3Physio72HrsNotAssessedReason",
    measure_type = "discrete"),

  # PhysioApplicableWithin72hrs =====================================

  # G 11.22-24 If applicable, PT assessment within 72 hours
  # H 11.22-24 If applicable, PT assessment within 72 hours
  PhysioApplicableWithin72hrs = audit_measure(
    stem_name = "PhysioApplicableWithin72hrs",
    description = "Applicability for physiotherapy",
    exclusions = rlang::expr(.data[["S3Physio72HrsNotAssessedReason"]]
      %in% c("PR", "PU", "ND")),
    numerators = rlang::expr(!! ssnap_field[["ClockStartToPTMins"]]
      <= (72 * 60)),
    new_numerators = rlang::expr(!! ssnap_field[["ClockStartToPTMins"]]
                             <= (72 * 60)),
    csv_columns = c("S1PatientClockStartDateTime",
                    "S3Physio72HrsDateTime",
                    "S3Physio72HrsNotAssessedReason"),
    measure_type = "discrete"),

  # PhysioApplicableWithin24hrs =====================================

  # G 11.25-27 If applicable, PT assessment within 24 hours
  # H 11.25-27 If applicable, PT assessment within 24 hours
  PhysioApplicableWithin24hrs = audit_measure(
  stem_name = "PhysioApplicableWithin24hrs",
  description = "Applicability for physiotherapy",
  exclusions = rlang::expr(.data[["S3Physio72HrsNotAssessedReason"]]
    %in% c("PR", "PU", "ND")),
  numerators = rlang::expr(!! ssnap_field[["ClockStartToPTMins"]]
    <= (24 * 60)),
  new_numerators = rlang::expr(!! ssnap_field[["ClockStartToPTMins"]]
                           <= (24 * 60)),
  csv_columns = c("S1PatientClockStartDateTime",
                  "S3Physio72HrsDateTime",
                  "S3Physio72HrsNotAssessedReason"),
  measure_type = "discrete"),

  # PTAssessment72HrsComplete =======================================

  # C6.13-15 Reason for no PT assessment within 72hrs is known
  PTAssessment72HrsComplete = audit_measure(
    stem_name = "PTAssessment72HrsComplete",
    description = "PT assessment within 72hrs known",
    exclusions = NULL,
    numerators = rlang::expr(
      (.data[["S3Physio72HrsNotAssessedReason"]] != "NK") |
        is.na(.data[["S3Physio72HrsNotAssessedReason"]])),
    new_numerators = rlang::expr(
      (.data[["S3Physio72HrsNotAssessedReason"]] != "NK") |
        is.na(.data[["S3Physio72HrsNotAssessedReason"]])),
    csv_columns = "S3Physio72HrsNotAssessedReason",
    measure_type = "discrete"),

# * -----------------------------------------------------------------
# * Speech and language therapy (communication) ---------------------
# * -----------------------------------------------------------------

# SLTCommReviewGrouped ==============================================

  # G 12.1-15 Speech and language communication assessment breakdown
  # H 12.1-15 Speech and language communication assessment breakdown
  SLTCommReviewGrouped = audit_measure(
    stem_name = "SLTCommReview",
  description = "Onset to SLT communication initial assessment",
  exclusions = NULL,
  numerators = rlang::exprs(
    "Within24hrs" = !! ssnap_field[["ClockStartToSLTCommMins"]]
    <= (24 * 60),
    "24To72hrs" = dplyr::between(
      !! ssnap_field[["ClockStartToSLTCommMins"]],
      (24 * 60), (72 * 60)),
    "NotDoneOrg" =
      .data[["S3SpLangTherapistComm72HrsNotAssessedReason"]] == "OR",
    "NotDoneRefused" =
      .data[["S3SpLangTherapistComm72HrsNotAssessedReason"]] == "PR",
    "NotDoneUnwell" =
      .data[["S3SpLangTherapistComm72HrsNotAssessedReason"]] == "MU",
    "NotDoneNoDeficit" =
      .data[["S3SpLangTherapistComm72HrsNotAssessedReason"]] == "ND",
    "NotDoneNotKnown" =
      .data[["S3SpLangTherapistComm72HrsNotAssessedReason"]] == "NK"),
  new_numerators = tibble::tribble(
    ~numerator, ~fun, ~descriptor,
    "Within24hrs", rlang::expr(
      !! ssnap_field[["ClockStartToSLTCommMins"]] <= (24 * 60)),
      "Within 24 hours",
    "24To72hrs", rlang::expr(dplyr::between(
      !! ssnap_field[["ClockStartToSLTCommMins"]],
      (24 * 60), (72 * 60))),
      "24 to 72 hours",
    "NotDoneOrg", rlang::expr(
      .data[["S3SpLangTherapistComm72HrsNotAssessedReason"]] == "OR"),
      "Not done (organisational reasons)",
    "NotDoneRefused", rlang::expr(
      .data[["S3SpLangTherapistComm72HrsNotAssessedReason"]] == "PR"),
      "Not done (patient refused)",
    "NotDoneUnwell", rlang::expr(
      .data[["S3SpLangTherapistComm72HrsNotAssessedReason"]] == "MU"),
      "Not done (patient unwell)",
    "NotDoneNoDeficit", rlang::expr(
      .data[["S3SpLangTherapistComm72HrsNotAssessedReason"]] == "ND"),
      "Not done (no deficit)",
    "NotDoneNotKnown", rlang::expr(
      .data[["S3SpLangTherapistComm72HrsNotAssessedReason"]] == "NK"),
      "Not done (not known)"),
  csv_columns = c("S1PatientClockStartDateTime",
                  "S3SpLangTherapistComm72HrsDateTime",
                  "S3SpLangTherapistComm72HrsNotAssessedReason"),
  measure_type = "discrete"),

  # ClockStartToSLTCommMins =========================================

  # G 12.16-18 Clock start to SLT communication assessment (mins)
  # H 12.16-18 Clock start to SLT communication assessment (mins)
  ClockStartToSLTCommMins = audit_measure(
    stem_name = "ClockStartToSLTCommMins",
    description =
      "Clock start to initial SLT communication assessment (mins)",
    exclusions = NULL,
    numerators = ssnap_field[["ClockStartToSLTCommMins"]],
    new_numerators = ssnap_field[["ClockStartToSLTCommMins"]],
    csv_columns = c("S1PatientClockStartDateTime",
                    "S3SpLangTherapistComm72HrsDateTime"),
    measure_type = "continuous"),

  # SLTCommApplicability ============================================

  # G 12.19-21 Applicability for SLT communication therapy
  # H 12.19-21 Applicability for SLT communication therapy
  SLTCommApplicability = audit_measure(
    stem_name = "SLTCommApplicability",
    description = "Applicability for SLT communication therapy",
    exclusions = NULL,
    numerators = rlang::expr(!(
      .data[["S3SpLangTherapistComm72HrsNotAssessedReason"]]
      %in% c("PR", "PU", "ND"))),
    new_numerators = rlang::expr(!(
      .data[["S3SpLangTherapistComm72HrsNotAssessedReason"]]
      %in% c("PR", "PU", "ND"))),
    csv_columns = "S3SpLangTherapistComm72HrsNotAssessedReason",
    measure_type = "discrete"),

  # SLTCommApplicableWithin72hrs ====================================

  # G 12.22-24 If applicable, SLT Comm assessment within 72 hours
  # H 12.22-24 If applicable, SLT Comm assessment within 72 hours
  SLTCommApplicableWithin72hrs = audit_measure(
    stem_name = "SLTCommApplicableWithin72hrs",
    description = "Applicability for SLT communication therapy",
    exclusions = rlang::expr(
      .data[["S3SpLangTherapistComm72HrsNotAssessedReason"]]
      %in% c("PR", "PU", "ND")),
    numerators = rlang::expr(!! ssnap_field[["ClockStartToSLTCommMins"]]
      <= (72 * 60)),
    new_numerators = rlang::expr(!! ssnap_field[["ClockStartToSLTCommMins"]]
                             <= (72 * 60)),
    csv_columns = c("S1PatientClockStartDateTime",
                    "S3SpLangTherapistComm72HrsDateTime",
                    "S3SpLangTherapistComm72HrsNotAssessedReason"),
    measure_type = "discrete"),

  # SLTCommApplicableWithin24hrs ====================================

  # G 12.25-27 If applicable, SLT Comm assessment within 24 hours
  # H 12.25-27 If applicable, SLT Comm assessment within 24 hours
  SLTCommApplicableWithin24hrs = audit_measure(
    stem_name = "SLTCommApplicableWithin24hrs",
    description = "Applicability for SLT communication therapy",
    exclusions = rlang::expr(
      .data[["S3SpLangTherapistComm72HrsNotAssessedReason"]]
      %in% c("PR", "PU", "ND")),
    numerators = rlang::expr(ssnap_field[["ClockStartToSLTCommMins"]]
      <= (24 * 60)),
    new_numerators = rlang::expr(ssnap_field[["ClockStartToSLTCommMins"]]
                             <= (24 * 60)),
    csv_columns = "S3SpLangTherapistComm72HrsNotAssessedReason",
    measure_type = "discrete"),

  # SLTCommAssessment72HrsComplete ==================================

  # C6.16-18 Reason for no SLT Comm assessment within 72hrs is known
  SLTCommAssessment72HrsComplete = audit_measure(
    stem_name = "SLTCommAssessment72HrsComplete",
    description = "SLT Comm assessment within 72hrs known",
    exclusions = NULL,
    numerators = rlang::expr(
      (.data[["S3SpLangTherapistComm72HrsNotAssessedReason"]]
      != "NK") | is.na(
      .data[["S3SpLangTherapistComm72HrsNotAssessedReason"]])),
    new_numerators = rlang::expr(
      (.data[["S3SpLangTherapistComm72HrsNotAssessedReason"]]
       != "NK") | is.na(
         .data[["S3SpLangTherapistComm72HrsNotAssessedReason"]])),
    csv_columns = "S3SpLangTherapistComm72HrsNotAssessedReason",
    measure_type = "discrete"),

# * -----------------------------------------------------------------
# * First 72hrs Bundle ----------------------------------------------
# * -----------------------------------------------------------------

  # G 13.1-3 Seen by nurse in 24hrs, one therapist in 24hrs and all
  # therapists in 72hrs.
  # H 13.1-3 Seen by nurse in 24hrs, one therapist in 24hrs and all
  # therapists in 72hrs.
# TODO This needs to be implemented

# * Swallow Screening -----------------------------------------------
# * -----------------------------------------------------------------

  # SwallowScreen4hrsGrouped ========================================

  # G 14.1-11 Swallow screening within 4 hours breakdown
  # H 14.1-11 Swallow screening within 4 hours breakdown
  SwallowScreen4hrsGrouped = audit_measure(
    stem_name = "SwallowScreen4hrs",
    description = "Swallow screen within 4hrs",
    exclusions = NULL,
    numerators = rlang::exprs(
      "Within4hrs" =
        !! ssnap_field[["ClockStartToSwallowScreen4hrsMins"]]
        <= (4 * 60),
    "NotDoneOrg" =
      .data[["S2SwallowScreening4HrsNotPerformedReason"]] == "OR",
    "NotDoneRefused" =
      .data[["S2SwallowScreening4HrsNotPerformedReason"]] == "PR",
    "NotDoneUnwell" =
      .data[["S2SwallowScreening4HrsNotPerformedReason"]] == "MU",
    "NotDoneNotKnown" =
      .data[["S2SwallowScreening4HrsNotPerformedReason"]] == "NK"),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "Within4hrs",
        !! ssnap_field[["ClockStartToSwallowScreen4hrsMins"]] <= (4 * 60),
        "Within 4 hours",
      "NotDoneOrg",
        .data[["S2SwallowScreening4HrsNotPerformedReason"]] == "OR",
        "Not done (organisational reasons)",
      "NotDoneRefused",
        .data[["S2SwallowScreening4HrsNotPerformedReason"]] == "PR",
        "Not done (refused)",
      "NotDoneUnwell",
        .data[["S2SwallowScreening4HrsNotPerformedReason"]] == "MU",
        "Not done (unwell)",
      "NotDoneNotKnown",
        .data[["S2SwallowScreening4HrsNotPerformedReason"]] == "NK",
        "Not done (not known)"),
    csv_columns = c("S1PatientClockStartDateTime",
                    "S2SwallowScreening4HrsDateTime",
                    "S2SwallowScreening4HrsNotPerformedReason"),
    measure_type = "discrete"),

  # ClockStartToSwallowScreen4hrsMins ===============================

  # G 14.12-14 Clock start to swallow screening if done within 4hrs
  # H 14.12-14 Clock start to swallow screening if done within 4hrs
  ClockStartToSwallowScreen4hrsMins = audit_measure(
    stem_name = "ClockStartToSwallowScreen4hrsMins",
    description =
      "Clock start to swallow screening if done within 4hrs (mins)",
    exclusions = NULL,
    numerators = ssnap_field[["ClockStartToSwallowScreen4hrsMins"]],
    new_numerators = ssnap_field[["ClockStartToSwallowScreen4hrsMins"]],
    csv_columns = c("S1PatientClockStartDateTime",
                    "S2SwallowScreening4HrsDateTime"),
    measure_type = "continuous"),

  # SwallowScreen4hrsApplicability ==================================

  # G 14.15-17 Applicability for swallow screening within 4hrs
  # H 14.15-17 Applicability for swallow screening within 4hrs
  SwallowScreen4hrsApplicability = audit_measure(
    stem_name = "SwallowScreen4hrsApplicability",
    description = "Applicability for swallow screening within 4hrs",
    exclusions = NULL,
    numerators = rlang::expr(!(
      .data[["S2SwallowScreening4HrsNotPerformedReason"]] %in%
        c("PR", "PU"))),
    new_numerators = rlang::expr(!(
      .data[["S2SwallowScreening4HrsNotPerformedReason"]] %in%
        c("PR", "PU"))),
    csv_columns = "S2SwallowScreening4HrsNotPerformedReason",
    measure_type = "discrete"),

  # SwallowScreenWithin4hrs =========================================

  # G 14.18-20 Swallow screen done if applicable within 4hrs
  # H 14.18-20 Swallow screen done if applicable within 4hrs
  SwallowScreenWithin4hrs = audit_measure(
    stem_name = "SwallowScreenWithin4hrs",
    description = "Swallow screen within 4 hours",
    exclusions = rlang::expr(
      .data[["S2SwallowScreening4HrsNotPerformedReason"]] %in%
        c("PR", "PU")),
    numerators = rlang::expr(
      !! ssnap_field[["ClockStartToSwallowScreen4hrsMins"]] <= (4 * 60)),
    new_numerators = rlang::expr(
      !! ssnap_field[["ClockStartToSwallowScreen4hrsMins"]] <= (4 * 60)),
    csv_columns = c("S1PatientClockStartDateTime",
                    "S2SwallowScreening4HrsDateTime",
                    "S2SwallowScreening4HrsNotPerformedReason"),
    measure_type = "discrete"),

  # SwallowScreen4hrsComplete =======================================

  # C6.4-6 Reason for no swallow screening within 4hrs is known
  SwallowScreen4hrsComplete = audit_measure(
    stem_name = "SwallowScreen4hrsComplete",
    description = "Swallow screening at 4 hrs known",
    exclusions = NULL,
    numerators = rlang::expr(
      (.data[["S2SwallowScreening4HrsNotPerformedReason"]] != "NK") |
        is.na(.data[["S2SwallowScreening4HrsNotPerformedReason"]])),
    new_numerators = rlang::expr(
      (.data[["S2SwallowScreening4HrsNotPerformedReason"]] != "NK") |
        is.na(.data[["S2SwallowScreening4HrsNotPerformedReason"]])),
    csv_columns = "S2SwallowScreening4HrsNotPerformedReason",
    measure_type = "discrete"),

  # ClockStartToSwallowScreen72hrsMins ==============================

  # G 14.21-23 Clock start to swallow screening if done within 72hrs
  # H 14.21-23 Clock start to swallow screening if done within 72hrs
  ClockStartToSwallowScreen72hrsMins = audit_measure(
    stem_name = "ClockStartToSwallowScreen72hrsMins",
    description =
      "Clock start to swallow screening if done within 72hrs (mins)",
    exclusions = NULL,
    numerators = ssnap_field[["ClockStartToSwallowScreen72hrsMins"]],
    new_numerators = ssnap_field[["ClockStartToSwallowScreen72hrsMins"]],
    csv_columns = c("S2SwallowScreening4HrsDateTime",
                    "S1PatientClockStartDateTime",
                    "S3SwallowScreening72HrsDateTime"),
    measure_type = "continuous"),

  # SwallowScreen72hrsGrouped ========================================

  # G 14.24-40 Swallow screening within 72 hours breakdown
  # H 14.24-40 Swallow screening within 72 hours breakdown
  SwallowScreen72hrsGrouped = audit_measure(
    stem_name = "SwallowScreen72hrs",
    description = "Swallow screen within 72hrs",
    exclusions = NULL,
    numerators = rlang::exprs(
      "Within4hrs" =
        !! ssnap_field[["ClockStartToSwallowScreen72hrsMins"]]
      <= (4 * 60),
      "4To12hrs" = dplyr::between(
        !! ssnap_field[["ClockStartToSwallowScreen72hrsMins"]],
        (4 * 60), (12 * 60)),
      "12To24hrs" = dplyr::between(
        !! ssnap_field[["ClockStartToSwallowScreen72hrsMins"]],
        (12 * 60), (24 * 60)),
      "24To72hrs" = dplyr::between(
        !! ssnap_field[["ClockStartToSwallowScreen72hrsMins"]],
        (24 * 60), (72 * 60)),
      "NotDoneOrg" =
        .data[["S3SwallowScreening72HrsNotPerformedReason"]] == "OR",
      "NotDoneRefused" =
        .data[["S3SwallowScreening72HrsNotPerformedReason"]] == "PR",
      "NotDoneUnwell" =
        .data[["S3SwallowScreening72HrsNotPerformedReason"]] == "MU",
      "NotDoneNotKnown" =
        .data[["S3SwallowScreening72HrsNotPerformedReason"]] == "NK"),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "Within4hrs", rlang::expr(
        !! ssnap_field[["ClockStartToSwallowScreen72hrsMins"]] <= (4 * 60)),
        "Within 4 hours",
      "4To12hrs", rlang::expr(dplyr::between(
        !! ssnap_field[["ClockStartToSwallowScreen72hrsMins"]], 
        (4 * 60), (12 * 60))),
        "4 to 12 hours",
      "12To24hrs", rlang::expr(dplyr::between(
        !! ssnap_field[["ClockStartToSwallowScreen72hrsMins"]],
        (12 * 60), (24 * 60))),
        "12 to 24 hours",
      "24To72hrs", rlang::expr(dplyr::between(
        !! ssnap_field[["ClockStartToSwallowScreen72hrsMins"]],
        (24 * 60), (72 * 60))),
        "24 to 72 hours",
      "NotDoneOrg", rlang::expr(
        .data[["S3SwallowScreening72HrsNotPerformedReason"]] == "OR"),
        "Not done (organisational reasons)",
      "NotDoneRefused", rlang::expr(
        .data[["S3SwallowScreening72HrsNotPerformedReason"]] == "PR"),
        "Not done (refused)",
      "NotDoneUnwell", rlang::expr(
        .data[["S3SwallowScreening72HrsNotPerformedReason"]] == "MU"),
        "Not done (medically unwell)",
      "NotDoneNotKnown", rlang::expr(
        .data[["S3SwallowScreening72HrsNotPerformedReason"]] == "NK"),
        "Not done (not known)"),
    csv_columns = c("S2SwallowScreening4HrsDateTime",
                    "S1PatientClockStartDateTime",
                    "S3SwallowScreening72HrsDateTime",
                    "S3SwallowScreening72HrsNotPerformedReason"),
    measure_type = "discrete"),

  # SwallowScreen72hrsApplicability =================================

  # G 14.41-43 Applicability for swallow screening within 72hrs
  # H 14.41-43 Applicability for swallow screening within 72hrs
  SwallowScreen72hrsApplicability = audit_measure(
    stem_name = "SwallowScreen72hrsApplicability",
    description = "Applicability for swallow screening within 72hrs",
    exclusions = NULL,
    numerators = rlang::expr(!(
      .data[["S3SwallowScreening72HrsNotPerformedReason"]] %in%
        c("PR", "PU"))),
    new_numerators = rlang::expr(!(
      .data[["S3SwallowScreening72HrsNotPerformedReason"]] %in%
        c("PR", "PU"))),
    csv_columns = "S3SwallowScreening72HrsNotPerformedReason",
    measure_type = "discrete"),

  # SwallowScreenWithin72hrs ========================================

  # G 14.44-46 Swallow screen done if applicable within 72hrs
  # H 14.44-46 Swallow screen done if applicable within 72hrs
  SwallowScreenWithin72hrs = audit_measure(
    stem_name = "SwallowScreenWithin72hrs",
    description = "Swallow screen within 72 hours",
    exclusions = rlang::expr(
      .data[["S3SwallowScreening72HrsNotPerformedReason"]] %in%
        c("PR", "PU")),
    numerators = rlang::expr(
      !! ssnap_field[["ClockStartToSwallowScreen72hrsMins"]]
      <= (72 * 60)),
    new_numerators = rlang::expr(
      !! ssnap_field[["ClockStartToSwallowScreen72hrsMins"]]
      <= (72 * 60)),
    csv_columns = c("S2SwallowScreening4HrsDateTime",
                    "S1PatientClockStartDateTime",
                    "S3SwallowScreening72HrsDateTime",
                    "S3SwallowScreening72HrsNotPerformedReason"),
    measure_type = "discrete"),

  # SwallowScreen72hrsComplete ======================================

  # C6/7-9 Swallow screening at 72hrs is known.
  SwallowScreen72hrsComplete = audit_measure(
    stem_name = "SwallowScreen72hrsComplete",
    description = "Swallow screening at 72hrs known",
    exclusions = NULL,
    numerators = rlang::expr(
      (.data[["S3SwallowScreening72HrsNotPerformedReason"]] != "NK") |
        is.na(.data[["S3SwallowScreening72HrsNotPerformedReason"]])),
    new_numerators = rlang::expr(
      (.data[["S3SwallowScreening72HrsNotPerformedReason"]] != "NK") |
        is.na(.data[["S3SwallowScreening72HrsNotPerformedReason"]])),
    csv_columns = "S3SwallowScreening72HrsNotPerformedReason",
    measure_type = "discrete"),

# * -----------------------------------------------------------------
# * Formal swallow assessment ---------------------------------------
# * -----------------------------------------------------------------

  # ClockStartToSpLangTherapistSwallowMins ==========================

  # G 15.1-3 Clock start to formal swallow assessment
  # H 15.1-3 Clock start to formal swallow assessment
  ClockStartToSpLangTherapistSwallowMins = audit_measure(
    stem_name = "ClockStartToSpLangTherapistSwallowMins",
    description =
      "Clock start to formal swallow assessment (mins)",
    exclusions = NULL,
    numerators =
      ssnap_field[["ClockStartToSpLangTherapistSwallowMins"]],
    new_numerators =
      ssnap_field[["ClockStartToSpLangTherapistSwallowMins"]],
    csv_columns = c("S1PatientClockStartDateTime",
                    "S3SpLangTherapistSwallow72HrsDateTime"),
    measure_type = "continuous"),

  # SpLangTherapistSwallow72hrsGrouped ==============================

  # G 15.4-18 SLT Swallow assessment breakdown
  # H 15.4-18 SLT Swallow assessment breakdown
  SpLangTherapistSwallow72hrsGrouped = audit_measure(
    stem_name = "SpLangTherapistSwallow72hrs",
    description = "Speech and language swallow within 72hrs",
    exclusions = NULL,
    numerators = rlang::exprs(
    "Within12hrs" =
      !! ssnap_field[["ClockStartToSpLangTherapistSwallowMins"]]
    <= (12 * 60),
    "12To24hrs" = dplyr::between(
      !! ssnap_field[["ClockStartToSpLangTherapistSwallowMins"]],
      (12 * 60), (24 * 60)),
    "24To72hrs" = dplyr::between(
      !! ssnap_field[["ClockStartToSpLangTherapistSwallowMins"]],
      (24 * 60), (72 * 60)),
    "NotDoneOrg" =
      .data[["S3SpLangTherapistSwallow72HrsNotAssessedReason"]]
      == "OR",
    "NotDoneRefused" =
      .data[["S3SpLangTherapistSwallow72HrsNotAssessedReason"]]
      == "PR",
    "NotDoneUnwell" =
      .data[["S3SpLangTherapistSwallow72HrsNotAssessedReason"]]
      == "PU",
    "NotDonePassedSwallow" =
      .data[["S3SpLangTherapistSwallow72HrsNotAssessedReason"]]
      == "PS",
    "NotDoneNotKnown" =
      .data[["S3SpLangTherapistSwallow72HrsNotAssessedReason"]]
      == "NK"),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "Within12hrs", rlang::expr(
        !! ssnap_field[["ClockStartToSpLangTherapistSwallowMins"]] <= (12 * 60)),
        "Within 12 hours",
      "12To24hrs", rlang::expr(dplyr::between(
        !! ssnap_field[["ClockStartToSpLangTherapistSwallowMins"]],
        (12 * 60), (24 * 60))),
        "12 to 24 hours",
      "24To72hrs", rlang::expr(dplyr::between(
        !! ssnap_field[["ClockStartToSpLangTherapistSwallowMins"]],
        (24 * 60), (72 * 60))),
        "24 to 72 hours",
      "NotDoneOrg", rlang::expr(
        .data[["S3SpLangTherapistSwallow72HrsNotAssessedReason"]] == "OR"),
        "Not done (organisational reasons)",
      "NotDoneRefused", rlang::expr(
        .data[["S3SpLangTherapistSwallow72HrsNotAssessedReason"]] == "PR"),
        "Not done (refused)",
      "NotDoneUnwell", rlang::expr(
        .data[["S3SpLangTherapistSwallow72HrsNotAssessedReason"]] == "PU"),
        "Not done (medically unwell)",
      "NotDonePassedSwallow", rlang::expr(
        .data[["S3SpLangTherapistSwallow72HrsNotAssessedReason"]] == "PS"),
        "Not done (passed swallow)",
      "NotDoneNotKnown", rlang::expr(
        .data[["S3SpLangTherapistSwallow72HrsNotAssessedReason"]] == "NK"),
        "Not done (not known)"),
    csv_columns = c("S1PatientClockStartDateTime",
                    "S3SpLangTherapistSwallow72HrsDateTime",
                    "S3SpLangTherapistSwallow72HrsNotAssessedReason"),
    measure_type = "discrete"),

  # SpLangTherapistSwallowApplicability =============================

  # G 15.19-22 Applicability for swallow assessment
  # H 15.19-22 Applicability for swallow assessment
  SpLangTherapistSwallowApplicability = audit_measure(
    stem_name = "SpLangTherapistSwallowApplicability",
    description = "Applicability for swallow assessment",
    exclusions = NULL,
    numerators = rlang::expr(!(
      .data[["S3SpLangTherapistSwallow72HrsNotAssessedReason"]] %in%
        c("PR", "PU", "PS"))),
    new_numerators = rlang::expr(!(
      .data[["S3SpLangTherapistSwallow72HrsNotAssessedReason"]] %in%
        c("PR", "PU", "PS"))),
    csv_columns = "S3SpLangTherapistSwallow72HrsNotAssessedReason",
    measure_type = "discrete"),

  # SpLangTherapistSwallowWithin72hrs ===============================

  # G 15.23-25 Swallow assessment done if applicable within 72hrs
  # H 14.23-25 Swallow assessment done if applicable within 72hrs
  SpLangTherapistSwallowWithin72hrs = audit_measure(
    stem_name = "SpLangTherapistSwallowWithin72hrs",
    description = "Swallow assessment if applicable within 72 hours",
    exclusions = rlang::expr(
      .data[["S3SpLangTherapistSwallow72HrsNotAssessedReason"]] %in%
        c("PR", "PU", "PS")),
    numerators = rlang::expr(
      !! ssnap_field[["ClockStartToSpLangTherapistSwallowMins"]]
      <= (72 * 60)),
    new_numerators = rlang::expr(
      !! ssnap_field[["ClockStartToSpLangTherapistSwallowMins"]]
      <= (72 * 60)),
    csv_columns = c("S1PatientClockStartDateTime",
                    "S3SpLangTherapistSwallow72HrsDateTime",
                    "S3SpLangTherapistSwallow72HrsNotAssessedReason"),
    measure_type = "discrete"),

  # SwallowAssessment72hrsComplete ==================================

  # C 6.19.-21 Reason for formal swallow assessment known
  SwallowAssessment72hrsComplete = audit_measure(
    stem_name = "SwallowAssessment72hrsComplete",
    description = "Swallow assessment within 72hrs known",
    exclusions = NULL,
    numerators = rlang::expr(
      (.data[["S3SpLangTherapistSwallow72HrsNotAssessedReason"]] !=
         "NK") | is.na(
           .data[["S3SpLangTherapistSwallow72HrsNotAssessedReason"]])
      ),
    new_numerators = rlang::expr(
      (.data[["S3SpLangTherapistSwallow72HrsNotAssessedReason"]] !=
         "NK") | is.na(
           .data[["S3SpLangTherapistSwallow72HrsNotAssessedReason"]])),
    csv_columns = "S3SpLangTherapistSwallow72HrsNotAssessedReason",
    measure_type = "discrete"),

# * -----------------------------------------------------------------
# * Thrombolysis ----------------------------------------------------
# * -----------------------------------------------------------------

  # Thrombolysis ====================================================

  # G 16.1-41 Was thrombolysis given
  # H 16.1-41 Was thrombolysis given
  # G 16.66-68 Thrombolysed?
  # H 16.66-68 Thrombolysed?
  Thrombolysis = audit_measure(
    stem_name = "Thrombolysis",
    description = "Thrombolysis",
    exclusions = NULL,
    numerators = rlang::exprs(
      "Yes" = .data[["S2Thrombolysis"]] == "Y",
      "No" = .data[["S2Thrombolysis"]] == "N",
      "NoUnavailable" = (.data[["S2Thrombolysis"]] == "N") &
        (.data[["S2ThrombolysisNoReason"]] == "TNA"),
      "NoUnableToScanInTime" = (.data[["S2Thrombolysis"]] == "N") &
        (.data[["S2ThrombolysisNoReason"]] == "USQE"),
      "NoOutsideServiceHours" = (.data[["S2Thrombolysis"]] == "N") &
        (.data[["S2ThrombolysisNoReason"]] == "OTSH"),
      "NoReasonUnknown" = (.data[["S2Thrombolysis"]] == "N") &
        (.data[["S2ThrombolysisNoReason"]] == "N"),
      "NoBut" = .data[["S2Thrombolysis"]] == "NB",
      "NoButHaemorrhagic" = (.data[["S2Thrombolysis"]] == "NB") &
        bitwAnd(.data[["S2ThrombolysisNoBut"]],
          ssnapinterface::tpa_no_but["Haemorrhagic"]),
      "NoButImproving" = (.data[["S2Thrombolysis"]] == "NB") &
        bitwAnd(.data[["S2ThrombolysisNoBut"]],
          ssnapinterface::tpa_no_but["Improving"]),
      "NoButComorbidity" = (.data[["S2Thrombolysis"]] == "NB") &
        bitwAnd(.data[["S2ThrombolysisNoBut"]],
          ssnapinterface::tpa_no_but["Comorbidity"]),
      "NoButMedication" = (.data[["S2Thrombolysis"]] == "NB") &
        bitwAnd(.data[["S2ThrombolysisNoBut"]],
          ssnapinterface::tpa_no_but["Medication"]),
      "NoButRefusal" = (.data[["S2Thrombolysis"]] == "NB") &
        bitwAnd(.data[["S2ThrombolysisNoBut"]],
          ssnapinterface::tpa_no_but["Refusal"]),
      "NoButOtherMedical" = (.data[["S2Thrombolysis"]] == "NB") &
        bitwAnd(.data[["S2ThrombolysisNoBut"]],
          ssnapinterface::tpa_no_but["OtherMedical"]),
      "NoButAge" = (.data[["S2Thrombolysis"]] == "NB") &
        bitwAnd(.data[["S2ThrombolysisNoBut"]],
          ssnapinterface::tpa_no_but["Age"]),
      "NoButTooMildSevere" = (.data[["S2Thrombolysis"]] == "NB") &
        bitwAnd(.data[["S2ThrombolysisNoBut"]],
          ssnapinterface::tpa_no_but["TooMildSevere"]),
      "NoButTimeUnknownWakeUp" = (.data[["S2Thrombolysis"]] == "NB") &
        bitwAnd(.data[["S2ThrombolysisNoBut"]],
          ssnapinterface::tpa_no_but["TimeUnknownWakeUp"]),
      "NoButTimeWindow" = (.data[["S2Thrombolysis"]] == "NB") &
        bitwAnd(.data[["S2ThrombolysisNoBut"]],
          ssnapinterface::tpa_no_but["TimeWindow"])),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "Yes",
        rlang::expr(.data[["S2Thrombolysis"]] == "Y"),
        "Yes",
      "No",
        rlang::expr(.data[["S2Thrombolysis"]] == "N"),
        "No",
      "NoUnavailable", rlang::expr((.data[["S2Thrombolysis"]] == "N") &
        (.data[["S2ThrombolysisNoReason"]] == "TNA")),
        "No (unavailable)",
      "NoUnableToScanInTime", rlang::expr((.data[["S2Thrombolysis"]] == "N") &
        (.data[["S2ThrombolysisNoReason"]] == "USQE")),
        "No (unable to scan in time)",
      "NoOutsideServiceHours", rlang::expr((.data[["S2Thrombolysis"]] == "N") &
        (.data[["S2ThrombolysisNoReason"]] == "OTSH")),
        "No (outside service hours)",
      "NoReasonUnknown", rlang::expr((.data[["S2Thrombolysis"]] == "N") &
        (.data[["S2ThrombolysisNoReason"]] == "N")),
        "No (reason unknown)",
      "NoBut",
        rlang::expr( .data[["S2Thrombolysis"]] == "NB"),
        "No but",
      "NoButHaemorrhagic", rlang::expr((.data[["S2Thrombolysis"]] == "NB") &
        bitwAnd(.data[["S2ThrombolysisNoBut"]],
                ssnapinterface::tpa_no_but["Haemorrhagic"])),
        "No but (haemorrhagic stroke)",
      "NoButImproving", rlang::expr((.data[["S2Thrombolysis"]] == "NB") &
        bitwAnd(.data[["S2ThrombolysisNoBut"]],
                ssnapinterface::tpa_no_but["Improving"])),
        "No but (improving)",
      "NoButComorbidity", rlang::expr((.data[["S2Thrombolysis"]] == "NB") &
        bitwAnd(.data[["S2ThrombolysisNoBut"]],
                ssnapinterface::tpa_no_but["Comorbidity"])),
        "No but (comorbidity)",
      "NoButMedication", rlang::expr((.data[["S2Thrombolysis"]] == "NB") &
        bitwAnd(.data[["S2ThrombolysisNoBut"]],
                ssnapinterface::tpa_no_but["Medication"])),
        "No but (medication)",
      "NoButRefusal", rlang::expr((.data[["S2Thrombolysis"]] == "NB") &
        bitwAnd(.data[["S2ThrombolysisNoBut"]],
                ssnapinterface::tpa_no_but["Refusal"])),
        "No but (refusal)",
      "NoButOtherMedical", rlang::expr((.data[["S2Thrombolysis"]] == "NB") &
        bitwAnd(.data[["S2ThrombolysisNoBut"]],
                ssnapinterface::tpa_no_but["OtherMedical"])),
        "No but (other medical)",
      "NoButAge", rlang::expr((.data[["S2Thrombolysis"]] == "NB") &
        bitwAnd(.data[["S2ThrombolysisNoBut"]],
                ssnapinterface::tpa_no_but["Age"])),
        "No but (age)",
      "NoButTooMildSevere", rlang::expr((.data[["S2Thrombolysis"]] == "NB") &
        bitwAnd(.data[["S2ThrombolysisNoBut"]],
                ssnapinterface::tpa_no_but["TooMildSevere"])),
        "No but (too mild)",
      "NoButTimeUnknownWakeUp", rlang::expr((.data[["S2Thrombolysis"]] == "NB") &
        bitwAnd(.data[["S2ThrombolysisNoBut"]],
                ssnapinterface::tpa_no_but["TimeUnknownWakeUp"])),
        "No but (unknown onset time or wake-up stroke)",
      "NoButTimeWindow", rlang::expr((.data[["S2Thrombolysis"]] == "NB") &
        bitwAnd(.data[["S2ThrombolysisNoBut"]],
                ssnapinterface::tpa_no_but["TimeWindow"])),
        "No but (outside time window)"),
    csv_columns = c("S2Thrombolysis",
                    "S2ThrombolysisNoReason",
                    "S2ThrombolysisNoBut"),
    measure_type = "discrete"),

  # ClockStartToThrombolysisMins ====================================

  # G 16.1-42-4 Clock start to thrombolysis (mins)
  # H 16.1-42-4 Clock start to thrombolysis (mins)
  ClockStartToThrombolysisMins = audit_measure(
    stem_name = "ClockStartToThrombolysisMins",
    description = "Clock start to thrombolysis (mins)",
    exclusions = NULL,
    numerators = ssnap_field[["ClockStartToThrombolysisMins"]],
    new_numerators = ssnap_field[["ClockStartToThrombolysisMins"]],
    csv_columns = c("S1PatientClockStartDateTime",
                    "S2ThrombolysisDateTime"),
    measure_type = "continuous"),

  # ThrombolysedOnsetToClockStartMins ===============================

  # G 16.45 If thrombolysed, time from onset to clock start (mins)
  # H 16.45 If thrombolysed, time from onset to clock start (mins)
  ThrombolysedOnsetToClockStartMins = audit_measure(
    stem_name = "ThrombolysedOnsetToClockStartMins",
    description = "If thrombolysed, onset to clock start (mins)",
    exclusions = rlang::expr(.data[["S2Thrombolysis"]] != "Y"),
    numerators = ssnap_field[["OnsetToArrivalTimeMins"]],
    new_numerators = ssnap_field[["OnsetToArrivalTimeMins"]],
    csv_columns = c("S1OnsetInHospital",
                    "S1OnsetDateIsPrecise",
                    "S1OnsetTimeIsPrecise",
                    "S1OnsetDateTime",
                    "S1PatientClockStartDateTime",
                    "S2Thrombolysis"),
    measure_type = "continuous"),

  # ThrombolysedClockStartToBrainImagingMins ========================

  # G 16.46 If thrombolysed, clock start to brain imaging (mins)
  # H 16.46 If thrombolysed, clock start to brain imaging (mins)
  ThrombolysedClockStartToBrainImagingMins = audit_measure(
    stem_name = "ThrombolysedClockStartToBrainImagingMins",
    description = "If thrombolysed, clock start to scan (mins)",
    exclusions = rlang::expr(.data[["S2Thrombolysis"]] != "Y"),
    numerators = ssnap_field[["ClockStartToBrainImagingMins"]],
    new_numerators = ssnap_field[["ClockStartToBrainImagingMins"]],
    csv_columns = c("S1PatientClockStartDateTime",
                    "S2BrainImagingDateTime",
                    "S2Thrombolysis"),
    measure_type = "continuous"),

  # ThrombolysedBrainImagingToNeedleMins ============================

  # G 16.47 If thrombolysed, brain imaging to needle (mins)
  # H 16.47 If thrombolysed, brain imaging to needle (mins)
  ThrombolysedBrainImagingToNeedleMins = audit_measure(
    stem_name = "ThrombolysedBrainImagingToNeedleMins",
    description = "If thrombolysed, brain imaging to needle (mins)",
    exclusions = rlang::expr(.data[["S2Thrombolysis"]] != "Y"),
    numerators = ssnap_field[["BrainImagingToNeedleMins"]],
    new_numerators = ssnap_field[["BrainImagingToNeedleMins"]],
    csv_columns = "S2Thrombolysis",
    measure_type = "continuous"),

  # EligibleFortPAByRCPCriteria =====================================

  # G 16.48-52 Eligible for tPA by RCP criteria
  # H 16.48-52 Eligible for tPA by RCP criteria
  EligibleFortPAByRCPCriteria = audit_measure(
  stem_name = "EligibleFortPAByRCPCriteria",
  description = "Eligible for tPA by RCP criteria",
  exclusions = NULL,
  numerators = rlang::exprs(
    "Eligible" = !! ssnap_field[["EligibleFortPAByRCPCriteria"]],
    "NotEligible" =
      !(!! ssnap_field[["EligibleFortPAByRCPCriteria"]])),
  new_numerators = tibble::tribble(
    ~numerator, ~fun, ~descriptor,
    "Eligible",
      rlang::expr(!! ssnap_field[["EligibleFortPAByRCPCriteria"]]),
      "Eligible",
    "NotEligible",
      rlang::expr(!(!! ssnap_field[["EligibleFortPAByRCPCriteria"]])),
      "Not eligible"),
  csv_columns = c("S1OnsetInHospital",
                  "S1OnsetTimeIsPrecise",
                  "S1OnsetDateTime",
                  "S1PatientClockStartDateTime",
                  "S1AgeOnArrival"),
  measure_type = "discrete"),

  # tPAIfMeetsRCPCriteria ===========================================

  # G 16.53-57 If eligible for tPA, was thrombolysis received?
  # H 16.53-57 If eligible for tPA, was thrombolysis received?
  tPAIfMeetsRCPCriteria = audit_measure(
    stem_name = "tPAIfMeetsRCPCriteria",
    description = "Thrombolysis meeting RCP criteria",
    exclusions = rlang::expr(
      !! ssnap_field[["ThrombolysisNoButConsistent"]] != 0),
    numerators = rlang::exprs(
      "Yes" = .data[["S2Thrombolysis"]] == "Y",
      "No" = .data[["S2Thrombolysis"]] != "Y"),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "Yes",
        rlang::expr(.data[["S2Thrombolysis"]] == "Y"),
        "Yes",
      "No",
        rlang::expr(.data[["S2Thrombolysis"]] != "Y"),
        "No"),
    csv_columns = c("S2Thrombolysis",
                    "S2ThrombolysisNoBut",
                    "S1AgeOnArrival",
                    "S1OnsetTimeIsPrecise",
                    "S1OnsetInHospital",
                    "S1OnsetDateTime",
                    "S1PatientClockStartDateTime",
                    "S2NihssArrival",
                    "S1OnsetDateIsPrecise"),
    measure_type = "discrete"),

  # tPAIfDoesntMeetRCPCriteria ======================================

  # G 16.58-62 If not eligible for tPA, was thrombolysis received?
  # H 16.58-62 If not eligible for tPA, was thrombolysis received?
  tPAIfDoesntMeetRCPCriteria = audit_measure(
    stem_name = "tPAIfDoesntMeetRCPCriteria",
    description = "If not eligible for tPA, was tPA given?",
    exclusions = rlang::expr(
      !! ssnap_field[["ThrombolysisNoButConsistent"]] == 0),
    numerators = rlang::exprs(
      "Yes" = .data[["S2Thrombolysis"]] == "Y",
      "No" = .data[["S2Thrombolysis"]] != "Y"),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "Yes",
      rlang::expr(.data[["S2Thrombolysis"]] == "Y"),
      "Yes",
      "No",
      rlang::expr(.data[["S2Thrombolysis"]] != "Y"),
      "No"),
    csv_columns = c("S2Thrombolysis",
                    "S2ThrombolysisNoBut",
                    "S1AgeOnArrival",
                    "S1OnsetTimeIsPrecise",
                    "S1OnsetInHospital",
                    "S1OnsetDateTime",
                    "S1PatientClockStartDateTime",
                    "S2NihssArrival",
                    "S1OnsetDateIsPrecise"),
    measure_type = "discrete"),

  # EligibleOrThrombolysed ==========================================

  # G 16.63-65 Was tPA given (measuring either those eligible by RCP
  # criteria or those thrombolysed outside RCP criteria
  # H 16.63-65 Was tPA given (measuring either those eligible by RCP
  # criteria or those thrombolysed outside RCP criteria
  EligibleOrThrombolysed = audit_measure(
    stem_name = "EligibleOrThrombolysed",
    description = paste0("Was tPA given (measuring either those ",
      "eligible by RCP criteria or those thrombolysed outside RCP ",
      "criteria"),
    exclusions = rlang::expr(
      (!(!! ssnap_field[["EligibleFortPAByRCPCriteria"]])) &
        (.data[["S2Thrombolysis"]] != "Y")),
    numerators = rlang::exprs(
      "Yes" = .data[["S2Thrombolysis"]] == "Y",
      "No" = .data[["S2Thrombolysis"]] != "Y"),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "Yes",
      rlang::expr(.data[["S2Thrombolysis"]] == "Y"),
      "Yes",
      "No",
      rlang::expr(.data[["S2Thrombolysis"]] != "Y"),
      "No"),
    csv_columns = c("S2Thrombolysis",
                    "S1OnsetInHospital",
                    "S1OnsetTimeIsPrecise",
                    "S1OnsetDateTime",
                    "S1PatientClockStartDateTime",
                    "S1AgeOnArrival",
                    "S2Thrombolysis"),
    measure_type = "discrete"),

  # EligibleExcludingNoButs =========================================

  # G 16.69-71 If eligible, patient thrombolysed (excludes "no but"s)
  # H 16.69-71 If eligible, patient thrombolysed (excludes "no but"s)
  EligibleExcludingNoButs = audit_measure(
    stem_name = "EligibleExcludingNoButs",
    description = paste0("If eligible, patient thrombolysed ",
      "(excludes 'no but's)"),
    exclusions = rlang::expr(
      (!(!! ssnap_field[["EligibleFortPAByRCPCriteria"]])) |
        (.data[["S2Thrombolysis"]] == "NB")),
    numerators = rlang::exprs(
      "Yes" = .data[["S2Thrombolysis"]] == "Y",
      "No" = .data[["S2Thrombolysis"]] != "Y"),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "Yes",
      rlang::expr(.data[["S2Thrombolysis"]] == "Y"),
      "Yes",
      "No",
      rlang::expr(.data[["S2Thrombolysis"]] != "Y"),
      "No"),
    csv_columns = c("S2Thrombolysis",
                    "S1OnsetInHospital",
                    "S1OnsetTimeIsPrecise",
                    "S1OnsetDateTime",
                    "S1PatientClockStartDateTime",
                    "S1AgeOnArrival",
                    "S2Thrombolysis"),
    measure_type = "discrete"),

  # tPAWithin1hr ====================================================

  # G 16.72-74 Thrombolysis given within 1 hour
  # H 16.72-74 Thrombolysis given within 1 hour
  tPAWithin1hr = audit_measure(
    stem_name = "tPAWithin1hr",
    description = "Thrombolysis within 1 hour",
    exclusions = rlang::expr(.data[["S2Thrombolysis"]] != "Y"),
    numerators = rlang::expr(
      !! ssnap_field[["ClockStartToThrombolysisMins"]] <= 60),
    new_numerators = rlang::expr(
      !! ssnap_field[["ClockStartToThrombolysisMins"]] <= 60),
    csv_columns = c("S1PatientClockStartDateTime",
                    "S2ThrombolysisDateTime",
                    "S2Thrombolysis"),
    measure_type = "discrete"),

  # tPAAndSUIn4hrs ==================================================

  # G 16.75.1-77.1 Thrombolysis if needed and stroke unit within 4hrs
  # H 16.75.1-77.1 Thrombolysis if needed and stroke unit within 4hrs
  tPAAndSUIn4hrs = audit_measure(
    stem_name = "tPAAndSUIn4hrs",
    description = "Thrombolysis and stroke unit in 4 hours",
    exclusions = rlang::expr( (.data[["S1FirstWard"]] == "ICH") |
      (!is.na(.data[["S2IAI"]]) & .data[["S2IAI"]])),
    numerators = rlang::expr( (.data[["S2Thrombolysis"]] != "N") &
      !! ssnap_field[["ClockStartToFirstStrokeUnitMins"]]
      <= (4 * 60)),
    new_numerators = rlang::expr( (.data[["S2Thrombolysis"]] != "N") &
                                !! ssnap_field[["ClockStartToFirstStrokeUnitMins"]]
                              <= (4 * 60)),
    csv_columns = c("S2Thrombolysis",
                    "S1PatientClockStartDateTime",
                    "S1FirstStrokeUnitArrivalDateTime",
                    "S1FirstWard",
                    "S2IAI"),
    measure_type = "discrete"),

# * -----------------------------------------------------------------
# * Thrombolysis complications --------------------------------------
# * -----------------------------------------------------------------

# ThrombolysisComplications =========================================

  # G 17.1-3 Patient had complications following thrombolysis
  # H 17.1-3 Patient had complications following thrombolysis
  ThrombolysisComplications = audit_measure(
    stem_name = "ThrombolysisComplications",
    description = "Complications following thrombolysis",
    exclusions = rlang::expr(.data[["S2Thrombolysis"]] != "Y"),
    numerators = rlang::expr(.data[["S2ThrombolysisComplications"]]
      >= 0),
    new_numerators = rlang::expr(.data[["S2ThrombolysisComplications"]]
                             >= 0),
    csv_columns = c("S2ThrombolysisComplications",
                    "S2Thrombolysis"),
    measure_type = "discrete"),

# ThrombolysisComplicationsGrouped ==================================

  # G 17.4-12 Number of each type of thrombolysis complication
  # H 17.4-12 Number of each type of thrombolysis complication
  ThrombolysisComplicationsGrouped = audit_measure(
    stem_name = "ThrombolysisComplications",
    description = "Thrombolysis complications grouped by type",
    exclusions = rlang::expr(.data[["S2Thrombolysis"]] != "Y"),
    numerators = rlang::exprs(
      "SymptomaticICH" =
        bitwAnd(.data[["S2ThrombolysisComplications"]],
          ssnapinterface::tpa_complications["SymptomaticICH"]) ==
          ssnapinterface::tpa_complications["SymptomaticICH"],
      "Angiooedema" =
        bitwAnd(.data[["S2ThrombolysisComplications"]],
          ssnapinterface::tpa_complications["Angiooedema"]) ==
          ssnapinterface::tpa_complications["Angiooedema"],
      "ExtracranialBleed" =
        bitwAnd(.data[["S2ThrombolysisComplications"]],
          ssnapinterface::tpa_complications["ExtracranialBleed"]) ==
          ssnapinterface::tpa_complications["ExtracranialBleed"],
      "Other" = bitwAnd(.data[["S2ThrombolysisComplications"]],
          ssnapinterface::tpa_complications["Other"]) ==
          ssnapinterface::tpa_complications["Other"]),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "SymptomaticICH",
        rlang::expr(bitwAnd(.data[["S2ThrombolysisComplications"]],
                ssnapinterface::tpa_complications["SymptomaticICH"]) ==
        ssnapinterface::tpa_complications["SymptomaticICH"]),
        "Symptomatic intracerebral haemorrhage",
      "Angiooedema",
        rlang::expr(bitwAnd(.data[["S2ThrombolysisComplications"]],
                ssnapinterface::tpa_complications["Angiooedema"]) ==
        ssnapinterface::tpa_complications["Angiooedema"]),
        "Angiooedema",
      "ExtracranialBleed",
        rlang::expr(bitwAnd(.data[["S2ThrombolysisComplications"]],
                ssnapinterface::tpa_complications["ExtracranialBleed"]) ==
        ssnapinterface::tpa_complications["ExtracranialBleed"]),
        "Extracranial bleed",
      "Other",
        rlang::expr(bitwAnd(.data[["S2ThrombolysisComplications"]],
                    ssnapinterface::tpa_complications["Other"]) ==
        ssnapinterface::tpa_complications["Other"]),
        "Other"),
    csv_columns = c("S2ThrombolysisComplications",
                    "S2Thrombolysis"),
    measure_type = "discrete"),

# * -----------------------------------------------------------------
# * NIHSS following thrombolysis ------------------------------------
# * -----------------------------------------------------------------

  # Nihss24HrsKnown =================================================

  # G 18.1-3 NIHSS at 24hrs is known
  # H 18.1-3 NIHSS at 24hrs is known
  # C 3.1-3 NIHSS at 24hrs is known
  Nihss24HrsKnown = audit_measure(
    stem_name = "Nihss24HrsKnown",
    description = "NIHSS at 24 hrs known",
    exclusions = rlang::expr(!( (.data[["S2Thrombolysis"]] == "Y") |
      .data[["S2IAI"]])),
    numerators = rlang::expr(!.data[["S2Nihss24HrsNK"]]),
    new_numerators = rlang::expr(!.data[["S2Nihss24HrsNK"]]),
    csv_columns = c("S2Nihss24HrsNK",
                    "S2Thrombolysis",
                    "S2IAI"),
    measure_type = "discrete"),

  # Nihss24hrsSeverity ==============================================

  # G 18.4-14 Severity groups of NIHSS 24 hours after thrombolysis
  # H 18.4-14 Severity groups of NIHSS 24 hours after thrombolysis
  Nihss24hrsSeverity = audit_measure(
    stem_name = "Nihss24hrsSeverity",
    description = "NIHSS Severity at 24hrs",
    exclusions = rlang::expr( (.data[["S2Thrombolysis"]] != "Y") |
                              .data[["S2Nihss24HrsNK"]]),
    numerators = rlang::exprs(
      "0" = .data[["S2Nihss24Hrs"]] == 0,
      "1To4" = dplyr::between(.data[["S2Nihss24Hrs"]], 1, 4),
      "5To15" = dplyr::between(.data[["S2Nihss24Hrs"]], 5, 15),
      "16To20" = dplyr::between(.data[["S2Nihss24Hrs"]], 16, 20),
      "21To42" = dplyr::between(.data[["S2Nihss24Hrs"]], 21, 42)),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "0",
        rlang::expr(.data[["S2Nihss24Hrs"]] == 0),
        "0",
      "1To4",
        rlang::expr(dplyr::between(.data[["S2Nihss24Hrs"]], 1, 4)),
        "1 to 4",
      "5To15",
        rlang::expr(dplyr::between(.data[["S2Nihss24Hrs"]], 5, 15)),
        "5 to 15",
      "16To20",
        rlang::expr(dplyr::between(.data[["S2Nihss24Hrs"]], 16, 20)),
        "16 to 20",
      "21To42",
        rlang::expr(dplyr::between(.data[["S2Nihss24Hrs"]], 21, 42)),
        "21 to 42"),
    csv_columns = c("S2Nihss24Hrs",
                    "S2Thrombolysis",
                    "S2Nihss24HrsNK"),
    measure_type = "discrete"),

  # ChangeInNIHSSAt24hrs ============================================

  # G 18.15-21 Change in NIHSS from arrival to 24 hours after
  # thrombolysis
  # H 18.15-21 Change in NIHSS from arrival to 24 hours after
  # thrombolysis
  ChangeInNIHSSAt24hrs = audit_measure(
    stem_name = "ChangeInNIHSSAt24hrs",
    description = "Change in NIHSS at 24hrs",
    exclusions = rlang::expr( (.data[["S2Thrombolysis"]] != "Y") |
                             .data[["S2Nihss24HrsNK"]]),
    numerators = rlang::exprs(
      "Improved" = .data[["S2Nihss24Hrs"]] <
                   .data[["S2NihssArrival"]],
      "NoChange" = .data[["S2Nihss24Hrs"]] ==
                   .data[["S2NihssArrival"]],
      "Worsened" = .data[["S2Nihss24Hrs"]] >
                   .data[["S2NihssArrival"]]),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "Improved",
        rlang::expr(.data[["S2Nihss24Hrs"]] <
        .data[["S2NihssArrival"]]),
        "Improved",
      "NoChange",
        rlang::expr(.data[["S2Nihss24Hrs"]] ==
        .data[["S2NihssArrival"]]),
        "No change",
      "Worsened",
      rlang::expr(.data[["S2Nihss24Hrs"]] >
        .data[["S2NihssArrival"]]),
        "Worsened"),
    csv_columns = c("S2Nihss24Hrs",
                    "S2NihssArrival",
                    "S2Thrombolysis",
                    "S2Nihss24HrsNK"),
    measure_type = "discrete"),

  # Nihss24hrsScoreChange ===========================================

  # G 18.22-44 Change in NIHSS from arrival to 24 hours after
  # thrombolysis
  # H 18.22-44 Change in NIHSS from arrival to 24 hours after
  # thrombolysis
  Nihss24hrsScoreChange = audit_measure(
    stem_name = "Nihss24hrsScoreChange",
    description = "Change in NIHSS score at 24 hrs",
    exclusions = rlang::expr( (.data[["S2Thrombolysis"]] != "Y") |
                             .data[["S2Nihss24HrsNK"]]),
    numerators = rlang::exprs(
      "Minus13OrMore" = (.data[["S2NihssArrival"]] -
                         .data[["S2Nihss24Hrs"]]) < -12,
      "Minus9To12" = dplyr::between( (.data[["S2NihssArrival"]] -
        .data[["S2Nihss24Hrs"]]), -12, -9),
      "Minus5To8" = dplyr::between( (.data[["S2NihssArrival"]] -
        .data[["S2Nihss24Hrs"]]), -8, -5),
      "Minus3To4" = dplyr::between( (.data[["S2NihssArrival"]] -
        .data[["S2Nihss24Hrs"]]), -4, -3),
      "Minus1To2" = dplyr::between( (.data[["S2NihssArrival"]] -
        .data[["S2Nihss24Hrs"]]), -2, -1),
      "NoChange" = .data[["S2NihssArrival"]] ==
        .data[["S2Nihss24Hrs"]],
      "1To2" = dplyr::between( (.data[["S2NihssArrival"]] -
        .data[["S2Nihss24Hrs"]]), 1, 2),
      "3To4" = dplyr::between( (.data[["S2NihssArrival"]] -
        .data[["S2Nihss24Hrs"]]), 3, 4),
      "5To8" = dplyr::between( (.data[["S2NihssArrival"]] -
        .data[["S2Nihss24Hrs"]]), 5, 8),
      "9To12" = dplyr::between( (.data[["S2NihssArrival"]] -
        .data[["S2Nihss24Hrs"]]), 9, 12),
      "13OrMore" = (.data[["S2NihssArrival"]] -
        .data[["S2Nihss24Hrs"]]) > 12),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "Minus13OrMore",
        rlang::expr((.data[["S2NihssArrival"]] -
                    .data[["S2Nihss24Hrs"]]) < -12),
        "-13 or better",
      "Minus9To12",
        rlang::expr(dplyr::between( (.data[["S2NihssArrival"]] -
                    .data[["S2Nihss24Hrs"]]), -12, -9)),
        "-9 to -12",
      "Minus5To8",
        rlang::expr(dplyr::between( (.data[["S2NihssArrival"]] -
                    .data[["S2Nihss24Hrs"]]), -8, -5)),
        "-5 to -8",
      "Minus3To4",
        rlang::expr(dplyr::between( (.data[["S2NihssArrival"]] -
                    .data[["S2Nihss24Hrs"]]), -4, -3)),
        "-3 to -4",
      "Minus1To2",
        rlang::expr(dplyr::between( (.data[["S2NihssArrival"]] -
                    .data[["S2Nihss24Hrs"]]), -2, -1)),
        "-2 to -1",
      "NoChange",
        rlang::expr(.data[["S2NihssArrival"]] == .data[["S2Nihss24Hrs"]]),
        "No change",
      "1To2",
        rlang::expr(dplyr::between( (.data[["S2NihssArrival"]] -
          .data[["S2Nihss24Hrs"]]), 1, 2)),
        "+1 to +2",
      "3To4",
        rlang::expr(dplyr::between( (.data[["S2NihssArrival"]] -
                                  .data[["S2Nihss24Hrs"]]), 3, 4)),
        "+3 to +4",
      "5To8",
        rlang::expr(dplyr::between( (.data[["S2NihssArrival"]] -
                                  .data[["S2Nihss24Hrs"]]), 5, 8)),
        "+5 to +8",
      "9To12",
        rlang::expr(dplyr::between( (.data[["S2NihssArrival"]] -
                                   .data[["S2Nihss24Hrs"]]), 9, 12)),
        "+9 to +12",
      "13OrMore",
        rlang::expr((.data[["S2NihssArrival"]] -
                      .data[["S2Nihss24Hrs"]]) > 12),
        "+13 or worse"),
    csv_columns = c("S2NihssArrival",
                    "S2Nihss24Hrs",
                    "S2Thrombolysis",
                    "S2Nihss24HrsNK"),
    measure_type = "discrete"),

# * -----------------------------------------------------------------
# * Onset to arrival ------------------------------------------------
# * -----------------------------------------------------------------

  # G 19.1-11 Days from onset to arrival
  # H 19.1-11 Days from onset to arrival
  DaysOnsetToArrival = audit_measure(
    stem_name = "DaysOnsetToArrival",
    description = "Days from onset to arrival",
    exclusions = rlang::expr(is.na(.data[["S1OnsetDateIsPrecise"]]) |
      .data[["S1OnsetInHospital"]]),
    numerators = rlang::exprs(
      "SameDay" = !! ssnap_field[["DaysOnsetToArrival"]] == 0,
      "PreviousDay" = !! ssnap_field[["DaysOnsetToArrival"]] == 1,
      "2Days" = !! ssnap_field[["DaysOnsetToArrival"]] == 2,
      "3To7Days" = dplyr::between(
        !! ssnap_field[["DaysOnsetToArrival"]], 3, 7),
      "Over7Days" = !! ssnap_field[["DaysOnsetToArrival"]] > 7),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
        "SameDay",
          rlang::expr(!! ssnap_field[["DaysOnsetToArrival"]] == 0),
          "Same day",
        "PreviousDay",
          rlang::expr(!! ssnap_field[["DaysOnsetToArrival"]] == 1),
          "Previous day",
        "2Days",
          rlang::expr(!! ssnap_field[["DaysOnsetToArrival"]] == 2),
          "2 days",
        "3To7Days",
          rlang::expr(dplyr::between(
            !! ssnap_field[["DaysOnsetToArrival"]], 3, 7)),
          "3 to 7 days",
        "Over7Days",
          rlang::expr(!! ssnap_field[["DaysOnsetToArrival"]] > 7),
          "Over 7 days"),
    csv_columns = c("S1FirstArrivalDateTime",
                    "S1OnsetDateTime",
                    "S1OnsetDateIsPrecise",
                    "S1OnsetInHospital"),
    measure_type = "discrete"),

# * Thrombectomy ----------------------------------------------------
# * -----------------------------------------------------------------

  # Thrombectomy ====================================================

  # G 20.1-3 Thrombectomy performed
  # H 20.1-3 Thrombectomy performed
  Thrombectomy = audit_measure(
    stem_name = "Thrombectomy",
    description = "Thrombectomy performed",
    exclusions = NULL,
    numerators = rlang::expr(.data[["S2IAI"]]),
    new_numerators = rlang::expr(.data[["S2IAI"]]),
    csv_columns = "S2IAI",
    measure_type = "discrete"),

  # OnsetToIAIPunctureMins ==========================================

  # G 20.4-6 Onset to groin puncture (mins)
  # H 20.4-6 Onset to groin puncture (mins)
  OnsetToIAIPunctureMins = audit_measure(
    stem_name = "OnsetToIAIPunctureMins",
    description = "Onset to groin puncture (mins)",
    exclusions = NULL,
    numerators = ssnap_field[["OnsetToIAIPunctureMins"]],
    new_numerators = ssnap_field[["OnsetToIAIPunctureMins"]],
    csv_columns = c("S1OnsetDateTime",
                    "S2IAIArterialPunctureDateTime"),
    measure_type = "continuous"),

  # OnsetToIAICompletionMins ========================================

  # G 20.7-9 Onset to thrombectomy completion (mins)
  # H 20.7-9 Onset to thrombectomy completion (mins)
  OnsetToIAICompletionMins = audit_measure(
    stem_name = "OnsetToIAICompletionMins",
    description = "Onset to thrombectomy completion (mins)",
    exclusions = NULL,
    numerators = ssnap_field[["OnsetToIAICompletionMins"]],
    new_numerators = ssnap_field[["OnsetToIAICompletionMins"]],
    csv_columns = c("S1OnsetDateTime",
                    "S2IAIEndOfProcedureDateTime"),
    measure_type = "continuous"),

  # ClockStartToIAIPunctureMins =====================================

  # G 20.10-12 Clock start to groin puncture (mins)
  # G 20.10-12 Clock start to groin puncture (mins)
  ClockStartToIAIPunctureMins = audit_measure(
    stem_name = "ClockStartToIAIPunctureMins",
    description = "Clock start to groin puncture (mins)",
    exclusions = NULL,
    numerators = ssnap_field[["ClockStartToIAIPunctureMins"]],
    new_numerators = ssnap_field[["ClockStartToIAIPunctureMins"]],
    csv_columns = c("S1PatientClockStartDateTime",
                    "S2IAIArterialPunctureDateTime"),
    measure_type = "continuous"),

  # PunctureToDeploymentMins ========================================

  # G 20.13-15 Groin puncture to stent retriever deployment (mins)
  # H 20.13-15 Groin puncture to stent retriever deployment (mins)
  PunctureToDeploymentMins = audit_measure(
    stem_name = "PunctureToDeploymentMins",
    description = "Groin puncture to stent deployment (mins)",
    exclusions = NULL,
    numerators = ssnap_field[["PunctureToDeploymentMins"]],
    new_numerators = ssnap_field[["PunctureToDeploymentMins"]],
    csv_columns = c("S2IAIArterialPunctureDateTime",
                    "S2IAIThrombectomyAspirationDeviceDateTime"),
    measure_type = "continuous"),

  # PunctureToIAICompletionMins =====================================

  # G 20.16-18 Groin puncture to procedure completion (mins)
  # H 20.16-18 Groin puncture to procedure completion (mins)
  PunctureToIAICompletionMins = audit_measure(
    stem_name = "PunctureToIAICompletionMins",
    description = "Groin puncture to procedure completion (mins)",
    exclusions = NULL,
    numerators = ssnap_field[["PunctureToIAICompletionMins"]],
    new_numerators = ssnap_field[["PunctureToIAICompletionMins"]],
    csv_columns = c("S2IAIArterialPunctureDateTime",
                    "S2IAIEndOfProcedureDateTime"),
    measure_type = "continuous"),

# * -----------------------------------------------------------------
# * NIHSS after Thrombectomy ----------------------------------------
# * -----------------------------------------------------------------

  # G 21.1-3 NIHSS 24hrs after thrombectomy is known
  # H 21.1-3 NIHSS 24hrs after thrombectomy is known
  Nihss24hrsAfterIAIKnown = audit_measure(
    stem_name = "Thrombectomy",
    description = "Thrombectomy performed",
    exclusions = rlang::expr(!.data[["S2IAI"]]),
    numerators = rlang::expr(.data[["S2Nihss24HrsNK"]]),
    new_numerators = rlang::expr(.data[["S2Nihss24HrsNK"]]),
    csv_columns = c("S2Nihss24HrsNK",
                    "S2IAI"),
    measure_type = "discrete"),


# * -----------------------------------------------------------------
# * Occupational therapy --------------------------------------------
# * -----------------------------------------------------------------

  # OccTherApplicable ===============================================

  # J 3.1-3 Applicability for patients to receive occupational
  # therapy at least once during their inpatient stay
  OccTherApplicable = audit_measure(
    stem_name = "OccTherApplicable",
    description = "Occupational Therapy applicability",
    exclusions = NULL,
    numerators = rlang::expr(.data[["S4OccTher"]]),
    new_numerators = rlang::expr(.data[["S4OccTher"]]),
    csv_columns = "S4OccTher",
    measure_type = "discrete"),

  # PCOccTherDays ===================================================

  # J 3.4 Percentage of days of occupational therapy delivered of
  # those where it is needed
  PCOccTherDays = audit_measure(
    stem_name = "PCOccTherDays",
    description = "Percent of days occupational therapy received",
    exclusions = NULL,
    numerators = ssnap_field[["PCOccTherDays"]],
    new_numerators = ssnap_field[["PCOccTherDays"]],
    csv_columns = c("S4OccTher",
                    "S4OccTherDays",
                    "S1TeamClockStartDateTime",
                    "S4OccTherEndDate"),
    measure_type = "continuous"),

  # OccTherMinutes ==================================================

  # J 3.5-7 Minutes of occupational therapy received
  OccTherMinutes = audit_measure(
    stem_name = "OccTherMinutes",
    description = "Minutes of occupational therapy received",
    exclusions = NULL,
    numerators = ssnap_field[["OccTherMinutesPerDay"]],
    new_numerators = ssnap_field[["PCOccTherDays"]],
    csv_columns = c("S4OccTherDays",
                    "S4OccTherMinutes"),
    measure_type = "continuous"),

  # OccTherComplianceMinutes ========================================

  # J 3.8 Average number of minutes of OT per day across all patients
  # (Proportion of patients applicable for OT*Median percentage of
  # days in hospital that OT is received*Median number of minutes per
  # day on which OT is received)
  OccTherComplianceMinutes = audit_measure(
    stem_name = "OccTherComplianceMinutes",
    description = "Minutes of occupational therapy received",
    exclusions = NULL,
    numerators = rlang::expr( (.data[["S4OccTher"]] / 100) *
      !! ssnap_field[["OccTherMinutesPerDay"]] *
      (!! ssnap_field[["PCOccTherDays"]] / 100)),
    new_numerators = rlang::expr( (.data[["S4OccTher"]] / 100) *
                                !! ssnap_field[["OccTherMinutesPerDay"]] *
                                (!! ssnap_field[["PCOccTherDays"]] / 100)),
    csv_columns = c("S4OccTher",
                    "S4OccTherDays",
                    "S4OccTherMinutes",
                    "S1TeamClockStartDateTime",
                    "S4OccTherEndDate"),
    measure_type = "continuous"),

  # OccTherComplianceTarget =========================================

  # J 3.9 Occupational therapy compliance target
  OccTherComplianceTarget = audit_measure(
    stem_name = "OccTherComplianceTarget",
    description = "Occupational therapy compliance target",
    exclusions = NULL,
    numerators = rlang::expr(0.8 * 45 * 5 / 7),
    new_numerators = rlang::expr(0.8 * 45 * 5 / 7),
    measure_type = "continuous"),

  # OccTherCompliance ===============================================

  # J 3.10 Occupational therapy compliance
  OccTherCompliance = audit_measure(
    stem_name = "OccTherCompliance",
    description = "Occupational therapy compliance",
    exclusions = NULL,
    numerators = rlang::expr(
      ( (.data[["S4OccTher"]] / 100) *
        !! ssnap_field[["OccTherMinutesPerDay"]] *
        (!! ssnap_field[["PCOccTherDays"]] / 100)) /
      (0.8 * 45 * 5 / 7) * 100),
    new_numerators = rlang::expr(
      ( (.data[["S4OccTher"]] / 100) *
          !! ssnap_field[["OccTherMinutesPerDay"]] *
          (!! ssnap_field[["PCOccTherDays"]] / 100)) /
        (0.8 * 45 * 5 / 7) * 100),
    csv_columns = c("S4OccTher",
                    "S4OccTherDays",
                    "S4OccTherMinutes",
                    "S1TeamClockStartDateTime",
                    "S4OccTherEndDate"),
    measure_type = "continuous"),

  # OccTherEquivalentMinutes ========================================

  # J 3.11 Average number of minutes of OT per day as equivalent to
  # 45 minutes on 5 days per week for 80% of patients
  OccTherEquivalentMinutes = audit_measure(
    stem_name = "OccTherEquivalentMinutes",
    description = "Occupational therapy equivalent minutes",
    exclusions = NULL,
    numerators = rlang::expr( ( (.data[["S4OccTher"]] / 100) *
      !! ssnap_field[["OccTherMinutesPerDay"]] *
      (!! ssnap_field[["PCOccTherDays"]] / 100)) /
      (0.8 * 5 / 7)),
    new_numerators = rlang::expr( ( (.data[["S4OccTher"]] / 100) *
                                  !! ssnap_field[["OccTherMinutesPerDay"]] *
                                  (!! ssnap_field[["PCOccTherDays"]] / 100)) /
                                (0.8 * 5 / 7)),
    csv_columns = c("S4OccTher",
                    "S4OccTherDays",
                    "S4OccTherMinutes",
                    "S1TeamClockStartDateTime",
                    "S4OccTherEndDate"),
    measure_type = "continuous"),

# TODO J 3.12-18 for All Teams


# * -----------------------------------------------------------------
# * Physiotherapy ---------------------------------------------------
# * -----------------------------------------------------------------

  # PhysioApplicable ===============================================

  # J 4.1-3 Applicability for patients to receive physiotherapy at
  # least once during their inpatient stay
  PhysioApplicable = audit_measure(
    stem_name = "PhysioApplicable",
    description = "Physiotherapy applicability",
    exclusions = NULL,
    numerators = rlang::expr(.data[["S4Physio"]]),
    new_numerators = rlang::expr(.data[["S4Physio"]]),
    csv_columns = "S4Physio",
    measure_type = "discrete"),

  # PCPhysioDays ====================================================

  # J 4.4 Percentage of days of physiotherapy delivered of those
  # where it is needed
  PCPhysioDays = audit_measure(
    stem_name = "PCPhysioDays",
    description = "Percent of days physio received",
    exclusions = NULL,
    numerators = ssnap_field[["PCPhysioDays"]],
    new_numerators = ssnap_field[["PCPhysioDays"]],
    csv_columns = c("S4Physio",
                    "S4PhysioDays",
                    "S1TeamClockStartDateTime",
                    "S4PhysioEndDate"),
    measure_type = "continuous"),

  # PhysioMinutes ==================================================

  # J 4.5-7 Minutes of physiotherapy received
  PhysioMinutes = audit_measure(
    stem_name = "PhysioMinutes",
    description = "Minutes of physio received",
    exclusions = NULL,
    numerators = ssnap_field[["PhysioMinutesPerDay"]],
    new_numerators = ssnap_field[["PhysioMinutesPerDay"]],
    csv_columns = c("S4PhysioDays",
                    "S4PhysioMinutes"),
    measure_type = "continuous"),

  # PhysioComplianceMinutes =========================================

  # J 4.8 Average number of minutes of OT per day across all patients
  # (Proportion of patients applicable for PT*Median percentage of
  # days in hospital that PT is received*Median number of minutes per
  # day on which PT is received)
  PhysioComplianceMinutes = audit_measure(
    stem_name = "PhysioComplianceMinutes",
    description = "Minutes of physiotherapy received",
    exclusions = NULL,
    numerators = rlang::expr( (.data[["S4Physio"]] / 100) *
      !! ssnap_field[["PhysioMinutesPerDay"]] *
      (!! ssnap_field[["PCPhysioDays"]] / 100)),
    new_numerators = rlang::expr( (.data[["S4Physio"]] / 100) *
                                !! ssnap_field[["PhysioMinutesPerDay"]] *
                                (!! ssnap_field[["PCPhysioDays"]] / 100)),
    csv_columns = c("S4Physio",
                    "S4PhysioDays",
                    "S4PhysioMinutes",
                    "S1TeamClockStartDateTime",
                    "S4PhysioEndDate"),
    measure_type = "continuous"),

  # PhysioComplianceTarget ==========================================

  # J 4.9 Physiotherapy compliance target
  PhysioComplianceTarget = audit_measure(
    stem_name = "PhysioComplianceTarget",
    description = "Physiotherapy compliance target",
    exclusions = NULL,
    numerators = rlang::expr(0.85 * 45 * 5 / 7),
    new_numerators = rlang::expr(0.85 * 45 * 5 / 7),
    measure_type = "continuous"),

  # PhysioCompliance ================================================

  # J 4.10 Physiotherapy compliance
  PhysioCompliance = audit_measure(
    stem_name = "PhysioCompliance",
    description = "Physiotherapy compliance",
    exclusions = NULL,
    numerators = rlang::expr(
      ( (.data[["S4Physio"]] / 100) *
        !! ssnap_field[["PhysioMinutesPerDay"]] *
        (!! ssnap_field[["PCPhysioDays"]] / 100)) /
        (0.85 * 45 * 5 / 7) * 100),
    new_numerators = rlang::expr(
      ( (.data[["S4Physio"]] / 100) *
          !! ssnap_field[["PhysioMinutesPerDay"]] *
          (!! ssnap_field[["PCPhysioDays"]] / 100)) /
        (0.85 * 45 * 5 / 7) * 100),
    csv_columns = c("S4Physio",
                    "S4PhysioDays",
                    "S4PhysioMinutes",
                    "S1TeamClockStartDateTime",
                    "S4PhysioEndDate"),
    measure_type = "continuous"),

  # PhysioEquivalentMinutes ========================================

  # J 4.11 Average number of minutes of PT per day as equivalent to
  # 45 minutes on 5 days per week for 85% of patients
  PhysioEquivalentMinutes = audit_measure(
    stem_name = "PhysioEquivalentMinutes",
    description = "Physiotherapy equivalent minutes",
    exclusions = NULL,
    numerators = rlang::expr( ( (.data[["S4Physio"]] / 100) *
      !! ssnap_field[["PhysioMinutesPerDay"]] *
      (!! ssnap_field[["PCPhysioDays"]] / 100)) /
      (0.85 * 5 / 7)),
    new_numerators = rlang::expr( ( (.data[["S4Physio"]] / 100) *
                                  !! ssnap_field[["PhysioMinutesPerDay"]] *
                                  (!! ssnap_field[["PCPhysioDays"]] / 100)) /
                                (0.85 * 5 / 7)),
    csv_columns = c("S4Physio",
                    "S4PhysioDays",
                    "S4PhysioMinutes",
                    "S1TeamClockStartDateTime",
                    "S4PhysioEndDate"),
    measure_type = "continuous"),

  # TODO J 4.12-18 for All Teams


# * -----------------------------------------------------------------
# * Speech and language therapy -------------------------------------
# * -----------------------------------------------------------------

  # SpeechLangApplicable ============================================

  # J 5.1-3 Applicability for patients to receive speech and language
  # therapy at least once during their inpatient stay
  SpeechLangApplicable = audit_measure(
    stem_name = "SpeechLangApplicable",
    description = "Speech and language Therapy applicability",
    exclusions = NULL,
    numerators = rlang::expr(.data[["S4SpeechLang"]]),
    new_numerators = rlang::expr(.data[["S4SpeechLang"]]),
    csv_columns = "S4SpeechLang",
    measure_type = "discrete"),

  # PCSpeechLangDays ================================================

  # J 5.4 Percentage of days of physiotherapy delivered of those
  # where it is needed
  PCSpeechLangDays = audit_measure(
    stem_name = "PCSpeechLangDays",
    description = "Percent of days speech and language received",
    exclusions = NULL,
    numerators = ssnap_field[["PCSpeechLangDays"]],
    new_numerators = ssnap_field[["PCSpeechLangDays"]],
    csv_columns = c("S4SpeechLang",
                    "S4SpeechLangDays",
                    "S1TeamClockStartDateTime",
                    "S4SpeechLangEndDate"),
    measure_type = "continuous"),

  # SpeechLangMinutes ===============================================

  # J 5.5-7 Minutes of physiotherapy received
  SpeechLangMinutes = audit_measure(
    stem_name = "SpeechLangMinutes",
    description = "Minutes of speech and language therapy received",
    exclusions = NULL,
    numerators = ssnap_field[["SpeechLangMinutesPerDay"]],
    new_numerators = ssnap_field[["SpeechLangMinutesPerDay"]],
    csv_columns = c("S4SpeechLangDays",
                    "S4SpeechLangMinutes"),
    measure_type = "continuous"),

  # SpeechLangComplianceMinutes =====================================

  # J 4.8 Average number of minutes of SLT per day across all patients
  # (Proportion of patients applicable for SLT*Median percentage of
  # days in hospital that SLT is received*Median number of minutes per
  # day on which PT is received)
  SpeechLangComplianceMinutes = audit_measure(
    stem_name = "SpeechLangComplianceMinutes",
    description = "Minutes of speech and language therapy received",
    exclusions = NULL,
    numerators = rlang::expr( (.data[["S4SpeechLang"]] / 100) *
      !! ssnap_field[["SpeechLangMinutesPerDay"]] *
      (!! ssnap_field[["PCSpeechLangDays"]] / 100)),
    new_numerators = rlang::expr( (.data[["S4SpeechLang"]] / 100) *
                                !! ssnap_field[["SpeechLangMinutesPerDay"]] *
                                (!! ssnap_field[["PCSpeechLangDays"]] / 100)),
    csv_columns = c("S4SpeechLang",
                    "S4SpeechLangDays",
                    "S4SpeechLangMinutes",
                    "S1TeamClockStartDateTime",
                    "S4SpeechLangEndDate"),
    measure_type = "continuous"),


  # SpeechLangComplianceTarget ======================================

  # J 5.9 Speech and language therapy compliance target
  SpeechLangComplianceTarget = audit_measure(
    stem_name = "SpeechLangComplianceTarget",
    description = "Speech and language therapy compliance target",
    exclusions = NULL,
    numerators = rlang::expr(0.5 * 45 * 5 / 7),
    new_numerators = rlang::expr(0.5 * 45 * 5 / 7),
    measure_type = "continuous"),

  # SpeechLangCompliance ============================================

  # J 5.10 Speech and language therapy compliance
  SpeechLangCompliance = audit_measure(
  stem_name = "SpeechLangCompliance",
  description = "Speech and language therapy compliance",
  exclusions = NULL,
  numerators = rlang::expr(
    ( (.data[["S4SpeechLang"]] / 100) *
        !! ssnap_field[["SpeechLangMinutesPerDay"]] *
        (!! ssnap_field[["PCSpeechLangDays"]] / 100)) /
      (0.5 * 45 * 5 / 7) * 100),
  new_numerators = rlang::expr(
    ( (.data[["S4SpeechLang"]] / 100) *
        !! ssnap_field[["SpeechLangMinutesPerDay"]] *
        (!! ssnap_field[["PCSpeechLangDays"]] / 100)) /
      (0.5 * 45 * 5 / 7) * 100),
  csv_columns = c("S4SpeechLang",
                  "S4SpeechLangDays",
                  "S4SpeechLangMinutes",
                  "S1TeamClockStartDateTime",
                  "S4SpeechLangEndDate"),
  measure_type = "continuous"),

  # SpeechLangEquivalentMinutes =====================================

  # J 5.11 Average number of minutes of SLT per day as equivalent to
  # 45 minutes on 5 days per week for 50% of patients
  SpeechLangEquivalentMinutes = audit_measure(
    stem_name = "SpeechLangEquivalentMinutes",
    description = "Speech and language therapy equivalent minutes",
    exclusions = NULL,
    numerators = rlang::expr( ( (.data[["S4SpeechLang"]] / 100) *
      !! ssnap_field[["SpeechLangMinutesPerDay"]] *
      (!! ssnap_field[["PCSpeechLangDays"]] / 100)) /
      (0.5 * 5 / 7)),
    new_numerators = rlang::expr( ( (.data[["S4SpeechLang"]] / 100) *
                                  !! ssnap_field[["SpeechLangMinutesPerDay"]] *
                                  (!! ssnap_field[["PCSpeechLangDays"]] / 100)) /
                                (0.5 * 5 / 7)),
    csv_columns = c("S4SpeechLang",
                    "S4SpeechLangDays",
                    "S4SpeechLangMinutes",
                    "S1TeamClockStartDateTime",
                    "S4SpeechLangEndDate"),
    measure_type = "continuous"),

  # TODO J 5.12-18 for All Teams


# * -----------------------------------------------------------------
# * All therapies ---------------------------------------------------
# * -----------------------------------------------------------------

  # AnyTherapyApplicable ============================================

  # J 6.1-3 Applicability for patients to receive any therapy
  # discipline at least once during their inpatient stay
  AnyTherapyApplicable = audit_measure(
    stem_name = "AnyTherapyApplicable",
    description = "Any therapy applicability",
    exclusions = NULL,
    numerators = rlang::expr(.data[["S4OccTher"]] |
                            .data[["S4Physio"]] |
                            .data[["S4SpeechLang"]]),
    new_numerators = rlang::expr(.data[["S4OccTher"]] |
                               .data[["S4Physio"]] |
                               .data[["S4SpeechLang"]]),
    csv_columns = c("S4OccTher",
                    "S4Physio",
                    "S4SpeechLang"),
    measure_type = "discrete"),

  # AllTherapyMinutesMet ==============================================

  # J 6.4-6 For all therapy disciplines applicable, did the patient
  # receive 45 mins therapy/day for at least 5 days/week?
  AllTherapyMinutesMet = audit_measure(
    stem_name = "AllTherapyMinutesMet",
    description = "Any therapy applicability",
    exclusions = rlang::expr( (!(.data[["S4OccTher"]])) &
                             (!(.data[["S4Physio"]])) &
                             (!(.data[["S4SpeechLang"]]))),
    numerators = rlang::expr(
      ( !(.data[["S4OccTher"]]) |
        ( (!! ssnap_field[["OccTherMinutesPerDay"]] *
           !! ssnap_field[["PCOccTherDays"]] / 100) >=
          (45 * 5 / 7))) &
      ( !(.data[["S4Physio"]]) |
        ( (!! ssnap_field[["PhysioMinutesPerDay"]] *
           !! ssnap_field[["PCPhysioDays"]] / 100) >=
          (45 * 5 / 7))) &
      ( !(.data[["S4SpeechLang"]]) |
        ( (!! ssnap_field[["SpeechLangMinutesPerDay"]] *
           !! ssnap_field[["PCSpeechLangDays"]] / 100) >=
          (45 * 5 / 7)))
      ),
    new_numerators = rlang::expr(
      ( !(.data[["S4OccTher"]]) |
          ( (!! ssnap_field[["OccTherMinutesPerDay"]] *
               !! ssnap_field[["PCOccTherDays"]] / 100) >=
              (45 * 5 / 7))) &
        ( !(.data[["S4Physio"]]) |
            ( (!! ssnap_field[["PhysioMinutesPerDay"]] *
                 !! ssnap_field[["PCPhysioDays"]] / 100) >=
                (45 * 5 / 7))) &
        ( !(.data[["S4SpeechLang"]]) |
            ( (!! ssnap_field[["SpeechLangMinutesPerDay"]] *
                 !! ssnap_field[["PCSpeechLangDays"]] / 100) >=
                (45 * 5 / 7)))
    ),
    
    csv_columns = c("S4OccTher",
                    "S4OccTherDays",
                    "S4OccTherMinutes",
                    "S1TeamClockStartDateTime",
                    "S4OccTherEndDate",
                    "S4Physio",
                    "S4PhysioDays",
                    "S4PhysioMinutes",
                    "S4PhysioEndDate",
                    "S4SpeechLang",
                    "S4SpeechLangDays",
                    "S4SpeechLangMinutes",
                    "S4SpeechLangEndDate",
                    "S4OccTher",
                    "S4Physio",
                    "S4SpeechLang"),
    measure_type = "discrete"),

# * -----------------------------------------------------------------
# * Psychology ------------------------------------------------------
# * -----------------------------------------------------------------

# PsychologyApplicable ==============================================

# J 7.1-3 Applicability for patients to receive psychology at
# least once during their inpatient stay
PsychologyApplicable = audit_measure(
  stem_name = "PsychologyApplicable",
  description = "Psychology applicability",
  exclusions = NULL,
  numerators = rlang::expr(.data[["S4Psychology"]]),
  new_numerators = rlang::expr(.data[["S4Psychology"]]),
  csv_columns = "S4Psychology",
  measure_type = "discrete"),

  # PCPsychologyDays ================================================

  # J 7.4 Percentage of days of psychology delivered of those
  # where it is needed
  PCPsychologyDays = audit_measure(
    stem_name = "PCPsychologyDays",
    description = "Percent of days psychology received",
    exclusions = NULL,
    numerators = ssnap_field[["PCPsychologyDays"]],
    new_numerators = ssnap_field[["PCPsychologyDays"]],
    csv_columns = c("S4Psychology",
                    "S4PsychologyDays",
                    "S1TeamClockStartDateTime",
                    "S4PsychologyEndDate"),
    measure_type = "continuous"),

  # PsychologyMinutes ===============================================

  # J 4.5-7 Minutes of psychology received
  PsychologyMinutes = audit_measure(
    stem_name = "PsychologyMinutes",
    description = "Minutes of psychology received",
    exclusions = NULL,
    numerators = ssnap_field[["PsychologyMinutesPerDay"]],
    new_numerators = ssnap_field[["PsychologyMinutesPerDay"]],
    csv_columns = c("S4PsychologyDays",
                    "S4PsychologyMinutes"),
    measure_type = "continuous"),

# TODO Length of stay


# * -----------------------------------------------------------------
# * Discharge destination -------------------------------------------
# * -----------------------------------------------------------------

  # DischargeDestinationGrouped =====================================

  # J 9.1-11.4 Discharge destination (grouped)
  DischargeDestinationGrouped = audit_measure(
    stem_name = "Discharge",
    description = "Discharge destination",
    exclusions = NULL,
    numerators = rlang::exprs(
      "Died" = .data[["S7DischargeType"]] == "D",
      "CareHome" = .data[["S7DischargeType"]] == "CH",
      "Home" = .data[["S7DischargeType"]] == "H",
      "SomewhereElse" = .data[["S7DischargeType"]] == "SE",
      "InpatientTransfer" = .data[["S7DischargeType"]] == "T",
      "CommunityTransfer" = .data[["S7DischargeType"]] == "TC",
      "InpatientTransferNonSSNAP" =
        .data[["S7DischargeType"]] == "TN",
      "CommunityTransferNonSSNAP" =
        .data[["S7DischargeType"]] == "TCN"),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "Died",
        rlang::expr(.data[["S7DischargeType"]] == "D"),
        "Died",
      "CareHome",
        rlang::expr(.data[["S7DischargeType"]] == "CH"),
        "Care home",
      "Home",
        rlang::expr(.data[["S7DischargeType"]] == "H"),
        "Home",
      "SomewhereElse",
        rlang::expr(.data[["S7DischargeType"]] == "SE"),
        "Somewhere else",
      "InpatientTransfer",
        rlang::expr(.data[["S7DischargeType"]] == "T"),
        "Inpatient transfer",
      "CommunityTransfer",
        rlang::expr(.data[["S7DischargeType"]] == "TC"),
        "Community transfer",
      "InpatientTransferNonSSNAP",
        rlang::expr(.data[["S7DischargeType"]] == "TN"),
        "Inpatient transfer (to a team not on SSNAP)",
      "CommunityTransferNonSSNAP",
        rlang::expr(.data[["S7DischargeType"]] == "TCN"),
        "Community transfer (to a team not on SSNAP)"),
    csv_columns = "S7DischargeType",
    measure_type = "discrete"),

  # DeathByDischarge ================================================

  # J 9.12-18 Death by discharge
  DeathByDischarge = audit_measure(
    stem_name = "DeathByDischarge",
    description = "Death by discharge",
    exclusions = NULL,
    numerators = rlang::exprs(
      "No" = is.na(.data[["S7StrokeUnitDeath"]]),
      "StrokeUnit" = (.data[["S7StrokeUnitDeath"]] == TRUE),
      "NotStrokeUnit" = (.data[["S7StrokeUnitDeath"]] == FALSE)),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "No",
        rlang::expr(is.na(.data[["S7StrokeUnitDeath"]])),
        "No",
      "StrokeUnit",
        rlang::expr(.data[["S7StrokeUnitDeath"]] == TRUE),
        "On a stroke unit",
      "NotStrokeUnit",
        rlang::expr(.data[["S7StrokeUnitDeath"]] == FALSE),
        "Not on a stroke unit"),
    csv_columns = "S7StrokeUnitDeath",
    measure_type = "discrete"),

# DischargedHome ====================================================

# J 9.19-25 Discharged home
  DischargedHome = audit_measure(
    stem_name = "DischargedHome",
    description = "Discharged home",
    exclusions = rlang::expr(is.na(.data[["S7HomeDischargeType"]])),
    numerators = rlang::exprs(
      "Alone" = .data[["S7HomeDischargeType"]] == "LA",
      "NotAlone" = .data[["S7HomeDischargeType"]] == "NLA",
      "NotKnown" = .data[["S7HomeDischargeType"]] == "NK"),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "Alone",
        rlang::expr(.data[["S7HomeDischargeType"]] == "LA"),
        "Alone",
      "NotAlone",
        rlang::expr(.data[["S7HomeDischargeType"]] == "NLA"),
        "Not alone",
      "NotKnown",
        rlang::expr(.data[["S7HomeDischargeType"]] == "NK"),
        "Not known"),
    csv_columns = c("S7HomeDischargeType",
                    "S7HomeDischargeType"),
    measure_type = "discrete"),

  # DischargedCareHome ==============================================

  # J 9.26-30 Discharged to a care home
  DischargedCareHome = audit_measure(
    stem_name = "DischargedCareHome",
    description = "Discharged to a care home",
    exclusions = rlang::expr(is.na(
      .data[["S7CareHomeDischargePermanentResidence"]])),
    numerators = rlang::exprs(
      "New" =
        (.data[["S7CareHomeDischargePermanentResidence"]] == TRUE),
      "NotNew" =
        (.data[["S7CareHomeDischargePermanentResidence"]] == FALSE)),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "New",
        rlang::expr(.data[["S7CareHomeDischargePermanentResidence"]] == TRUE),
        "New",
      "NotNew",
        rlang::expr(.data[["S7CareHomeDischargePermanentResidence"]] == FALSE),
        "Not new"),
    csv_columns = c("S7CareHomeDischargePermanentResidence",
                    "S7CareHomeDischargePermanentResidence"),
    measure_type = "discrete"),

  # NewlyInstitutionalised ==========================================

  # J 9.31-33 Newly institutionalised
  NewlyInstitutionalised = audit_measure(
    stem_name = "NewlyInstitutionalised",
    description = "Newly institutionalised following stroke",
    exclusions = rlang::expr(.data[["S7DischargeType"]] == "D"),
    numerators = rlang::expr(
      .data[["S7CareHomeDischargeToNewResidence"]] == TRUE),
    new_numerators = rlang::expr(
      .data[["S7CareHomeDischargeToNewResidence"]] == TRUE),
    csv_columns = c("S7CareHomeDischargeToNewResidence",
                    "S7DischargeType"),
    measure_type = "discrete"),

  # NewTemporaryInstitution =========================================

  # J 9.34-38 If newly institutionalised, temporary or permanent?
  NewTemporaryInstitution = audit_measure(
    stem_name = "NewTemporaryInstitution",
    description = "Is new institutionalisation temporary?",
    exclusions = rlang::expr(
      .data[["S7CareHomeDischargeToNewResidence"]] != TRUE),
    numerators = rlang::exprs(
      "Temporary" =
        (.data[["S7CareHomeDischargePermanentResidence"]] == FALSE),
      "Permanent" =
        (.data[["S7CareHomeDischargePermanentResidence"]] == TRUE)),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "Temporary",
        rlang::expr(.data[["S7CareHomeDischargePermanentResidence"]] == FALSE),
        "Temporary",
      "Permanent",
        rlang::expr(.data[["S7CareHomeDischargePermanentResidence"]] == TRUE),
        "Permanent"),
    csv_columns = c("S7CareHomeDischargePermanentResidence",
                    "S7CareHomeDischargeToNewResidence"),
    measure_type = "discrete"),


# * -----------------------------------------------------------------
# * ESD -------------------------------------------------------------
# * -----------------------------------------------------------------

  # DischargedESD ===================================================

  # J 10.1-7 If discharged alive, discharged with early supported
  # discharge (ESD) team:
  DischargedESD = audit_measure(
    stem_name = "DischargedESD",
  description = "Discharged with ESD?",
  exclusions = rlang::expr(.data[["S7DischargeType"]] == "D"),
  numerators = rlang::exprs(
    "Specialist" =
      (.data[["S7DischargedSpecialistEsdmt"]] == TRUE),
    "NotSpecialist" =
      (.data[["S7DischargedSpecialistEsdmt"]] == FALSE),
    "No" = is.na(.data[["S7DischargedSpecialistEsdmt"]])),
  new_numerators = tibble::tribble(
    ~numerator, ~fun, ~descriptor,
    "Specialist",
      rlang::expr(.data[["S7DischargedSpecialistEsdmt"]] == TRUE),
      "Specialist",
    "NotSpecialist",
      rlang::expr(.data[["S7DischargedSpecialistEsdmt"]] == FALSE),
      "Not specialist",
    "No",
      rlang::expr(is.na(.data[["S7DischargedSpecialistEsdmt"]])),
      "No"),
  csv_columns = c("S7DischargedSpecialistEsdmt",
                  "S7DischargeType"),
  measure_type = "discrete"),


# * -----------------------------------------------------------------
# * CRT -------------------------------------------------------------
# * -----------------------------------------------------------------

  # DischargedCRT ===================================================

  # J 11.1-7 If discharged alive, discharged with community rehab
  # team (CRT):
  DischargedCRT = audit_measure(
    stem_name = "DischargedCRT",
    description = "Discharged with CRT?",
    exclusions = rlang::expr(.data[["S7DischargeType"]] == "D"),
    numerators = rlang::exprs(
      "Specialist" =
        (.data[["S7DischargedSpecialistMcrt"]] == TRUE),
      "NotSpecialist" =
        (.data[["S7DischargedSpecialistMcrt"]] == FALSE),
      "No" = is.na(.data[["S7DischargedSpecialistMcrt"]])),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "Specialist",
        rlang::expr(.data[["S7DischargedSpecialistMcrt"]] == TRUE),
        "Specialist",
      "NotSpecialist",
        rlang::expr(.data[["S7DischargedSpecialistMcrt"]] == FALSE),
        "Not specialist",
      "No",
        rlang::expr(is.na(.data[["S7DischargedSpecialistMcrt"]])),
        "No"),
    csv_columns = c("S7DischargedSpecialistMcrt",
                    "S7DischargeType"),
    measure_type = "discrete"),


# * -----------------------------------------------------------------
# * ESD or CRT ------------------------------------------------------
# * -----------------------------------------------------------------

  # DischargedSpecialistESDOrCRT ====================================

  # J 12.1-3 If discharged alive, discharged with a stroke/neurology
  # specific service (ESD and/or CRT)
  DischargedSpecialistESDOrCRT = audit_measure(
    stem_name = "DischargedSpecialistESDOrCRT",
    description = "Discharged with speclist ESD or CRT?",
    exclusions = rlang::expr(.data[["S7DischargeType"]] == "D"),
    numerators = rlang::expr(
      (.data[["S7DischargedSpecialistEsdmt"]] == TRUE) |
      (.data[["S7DischargedSpecialistMcrt"]] == TRUE)),
    new_numerators = rlang::expr(
      (.data[["S7DischargedSpecialistEsdmt"]] == TRUE) |
        (.data[["S7DischargedSpecialistMcrt"]] == TRUE)),
    csv_columns = c("S7DischargedSpecialistEsdmt",
                    "S7DischargedSpecialistMcrt",
                    "S7DischargeType"),
    measure_type = "discrete"),


# * -----------------------------------------------------------------
# * Rehab goals -----------------------------------------------------
# * -----------------------------------------------------------------

  # RehabGoals ======================================================

  # J 13.1-9 Rehabilitation goals by discharge from inpatient care
  RehabGoals = audit_measure(
    stem_name = "RehabGoals",
    description = "Rehabilitation goals by inpatient discharge?",
    exclusions = rlang::expr(.data[["S7DischargeType"]] == "D"),
    numerators = rlang::exprs(
      "Within5Days" =
        (!!ssnap_field[["ClockStartToRehabGoalsDays"]] <= 5),
      "Over5Days" =
        (!!ssnap_field[["ClockStartToRehabGoalsDays"]] > 5),
      "ApplicableNotSet" = is.na(.data[["S4RehabGoalsDate"]]) &
        ( (.data[["S4RehabGoalsNoneReason"]] == "OR") |
           (.data[["S4RehabGoalsNoneReason"]] == "NK")),
      "NotApplicable" = is.na(.data[["S4RehabGoalsDate"]]) &
        (.data[["S4RehabGoalsNoneReason"]] != "OR") &
           (.data[["S4RehabGoalsNoneReason"]] != "NK")),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "Within5Days",
        rlang::expr(!!ssnap_field[["ClockStartToRehabGoalsDays"]] <= 5),
        "Within 5 days",
      "Over5Days",
        rlang::expr(!!ssnap_field[["ClockStartToRehabGoalsDays"]] > 5),
        "Over 5 days",
      "ApplicableNotSet",
        rlang::expr(is.na(.data[["S4RehabGoalsDate"]]) &
          ( (.data[["S4RehabGoalsNoneReason"]] == "OR") |
            (.data[["S4RehabGoalsNoneReason"]] == "NK"))),
        "Applicable but not set",
      "NotApplicable",
        rlang::expr(is.na(.data[["S4RehabGoalsDate"]]) &
          (.data[["S4RehabGoalsNoneReason"]] != "OR") &
          (.data[["S4RehabGoalsNoneReason"]] != "NK")),
        "Not applicable"),
    csv_columns = c("S1PatientClockStartDateTime",
                    "S4RehabGoalsDate",
                    "S4RehabGoalsNoneReason",
                    "S7DischargeType"),
    measure_type = "discrete"),

  # RehabGoalsApplicable ============================================

# J 13.10-12 Applicability for rehabilitation goals within 5 days of
# clock start
RehabGoalsApplicable = audit_measure(
  stem_name = "RehabGoalsApplicable",
  description = "Applicability for rehab goals within 5 days",
  exclusions = rlang::expr(.data[["S7DischargeType"]] == "D"),
  numerators = rlang::expr(
    (.data[["S4RehabGoalsNoneReason"]] != "OR") &
    (.data[["S4RehabGoalsNoneReason"]] != "NK")),
  new_numerators = rlang::expr(
    (.data[["S4RehabGoalsNoneReason"]] != "OR") &
      (.data[["S4RehabGoalsNoneReason"]] != "NK")),
  csv_columns = c("S4RehabGoalsNoneReason",
                  "S7DischargeType"),
  measure_type = "discrete"),

# RehabGoalsSetIfApplicable ============================================

# J 13.13-15 Rehab goals set if applicable
RehabGoalsSetIfApplicable = audit_measure(
  stem_name = "RehabGoalsSetIfApplicable",
  description = "Rehab goals set within 5 days if applicable",
  exclusions = rlang::expr( (.data[["S7DischargeType"]] == "D") |
    (.data[["S4RehabGoalsNoneReason"]] != "OR") |
    (.data[["S4RehabGoalsNoneReason"]] != "NK")),
  numerators = rlang::expr(
    !!ssnap_field[["ClockStartToRehabGoalsDays"]] <= 5),
  new_numerators = rlang::expr(
    !!ssnap_field[["ClockStartToRehabGoalsDays"]] <= 5),
  csv_columns = c("S1PatientClockStartDateTime",
                  "S4RehabGoalsDate",
                  "S7DischargeType",
                  "S4RehabGoalsNoneReason"),
  measure_type = "discrete"),

# TODO J14 - composite measure.

# * -----------------------------------------------------------------
# * Post-stroke infection -------------------------------------------
# * -----------------------------------------------------------------

# UrinaryTractInfection =============================================

  # K5.1-7 Urinary tract infection in first 7 days
  UrinaryTractInfection = audit_measure(
    stem_name = "UrinaryTractInfection",
    description =
      "Urinary tract infection in the first 7 days from clock start",
    exclusions = NULL,
    numerators = rlang::exprs(
      "Yes" = (.data[["S5UrinaryTractInfection7Days"]] == TRUE),
      "No" = (.data[["S5UrinaryTractInfection7Days"]] == FALSE),
      "Not known" = is.na(.data[["S5UrinaryTractInfection7Days"]])),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
        "Yes",
          rlang::expr(.data[["S5UrinaryTractInfection7Days"]] == TRUE),
          "Yes",
        "No",
          rlang::expr(.data[["S5UrinaryTractInfection7Days"]] == FALSE),
          "No",
        "Not known",
          rlang::expr(is.na(.data[["S5UrinaryTractInfection7Days"]])),
          "Not known"
    ),
    csv_columns = "S5UrinaryTractInfection7Days",
    measure_type = "discrete"),

# PneumoniaAntibiotics =============================================

  # K6.1-7 Antibiotics for newly acquired pneumonia in first 7 days
  PneumoniaAntibiotics = audit_measure(
    stem_name = "PneumoniaAntibiotics",
    description =
      "Antibiotics for newly acquired pneumonia in the first 7 days from clock start",
    exclusions = NULL,
    numerators = rlang::exprs(
      "Yes" = (.data[["S5PneumoniaAntibiotics7Days"]] == TRUE),
      "No" = (.data[["S5PneumoniaAntibiotics7Days"]] == FALSE),
      "Not known" = is.na(.data[["S5PneumoniaAntibiotics7Days"]]),
    ),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "Yes",
        rlang::expr(.data[["S5PneumoniaAntibiotics7Days"]] == TRUE),
        "Yes",
      "No",
        rlang::expr(.data[["S5PneumoniaAntibiotics7Days"]] == FALSE),
        "No",
      "Not known",
        rlang::expr(is.na(.data[["S5PneumoniaAntibiotics7Days"]])),
        "Not known"),
    csv_columns = "S5PneumoniaAntibiotics7Days",
    measure_type = "discrete"),


# * -----------------------------------------------------------------
# * Continence plan -------------------------------------------------
# * -----------------------------------------------------------------

  # ContinencePlanGrouped ===========================================

  # J 15.1-11 Urinary continence plan by discharge from inpatient care
  # K 2.1-11 Urinary continence plan by discharge from inpatient care

  ContinencePlanGrouped = audit_measure(
    stem_name = "ContinencePlanGrouped",
    description = "Continence plan if applicable",
    exclusions = NULL,
    numerators = rlang::exprs(
      "PlanDrawnUp" = !is.na(.data[["S6UrinaryContinencePlanDate"]]),
      "NoPlanOrg" =
        .data[["S6UrinaryContinencePlanNoPlanReason"]] == "OR",
      "NoPlanRefused" =
        .data[["S6UrinaryContinencePlanNoPlanReason"]] == "PR",
      "NoPlanContinent" =
        .data[["S6UrinaryContinencePlanNoPlanReason"]] == "PC",
      "NoPlanNotKnown" =
        .data[["S6UrinaryContinencePlanNoPlanReason"]] == "NK"),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "PlanDrawnUp",
        rlang::expr(!is.na(.data[["S6UrinaryContinencePlanDate"]])),
        "Plan drawn up",
      "NoPlanOrg",
        rlang::expr(.data[["S6UrinaryContinencePlanNoPlanReason"]] == "OR"),
        "No plan (organisational reasons)",
      "NoPlanRefused",
        rlang::expr(.data[["S6UrinaryContinencePlanNoPlanReason"]] == "PR"),
        "No plan (patient refused)",
      "NoPlanContinent",
        rlang::expr(.data[["S6UrinaryContinencePlanNoPlanReason"]] == "PC"),
        "No plan (patient continent)",
      "NoPlanNotKnown",
        rlang::expr(.data[["S6UrinaryContinencePlanNoPlanReason"]] == "NK"),
        "No plan (reason not known)"),
    csv_columns = c("S6UrinaryContinencePlanDate",
                    "S6UrinaryContinencePlanNoPlanReason"),
    measure_type = "discrete"),

  # ClockStartToContinencePlanHours =================================

  # J 15.12-14 Time from clock start to continence plan drawn up (hrs)
  # K 2.12-14 Time from clock start to continence plan drawn up (hrs)

  ClockStartToContinencePlanHours = audit_measure(
    stem_name = "ClockStartToContinencePlanHours",
    description = "Clock start to continence plan (hours)",
    exclusions = NULL,
    numerators = ssnap_field[["ClockStartToContinencePlanHours"]],
    new_numerators = ssnap_field[["ClockStartToContinencePlanHours"]],
    csv_columns = c("S1PatientClockStartDateTime",
                    "S6UrinaryContinencePlanDate"),
    measure_type = "continuous"),

  # ContinencePlanApplicability =====================================

  # J 15.15-17 Patient applicability for continence plan
  # K 2.15-17 Patient applicability for continence plan

  ContinencePlanApplicability = audit_measure(
    stem_name = "ContinencePlanApplicability",
    description = "Continence plan applicability",
    exclusions = NULL,
    numerators = rlang::expr(!(
      .data[["S6UrinaryContinencePlanNoPlanReason"]] %in%
        c("PC", "PR"))),
    new_numerators = rlang::expr(!(
      .data[["S6UrinaryContinencePlanNoPlanReason"]] %in%
        c("PC", "PR"))),
    csv_columns = "S6UrinaryContinencePlanNoPlanReason",
    measure_type = "discrete"),

  # ContinencePlanIfApplicable ======================================

  # J 15.18-20 Patient applicability for continence plan
  # K 2.18-20 Patient applicability for continence plan

  ContinencePlanIfApplicable = audit_measure(
    stem_name = "ContinencePlanIfApplicable",
    description = "Continence plan if applicable",
    exclusions = rlang::expr(
      .data[["S6UrinaryContinencePlanNoPlanReason"]] %in%
      c("PC", "PR")),
    numerators = rlang::expr(
      !is.null(.data[["S6UrinaryContinencePlanDate"]])),
    new_numerators = rlang::expr(
      !is.null(.data[["S6UrinaryContinencePlanDate"]])),
    csv_columns = c("S6UrinaryContinencePlanDate",
                    "S6UrinaryContinencePlanNoPlanReason"),
    measure_type = "discrete"),

  # ContinencePlanIfApplicableIn21Days ==============================

  # J 15.21-23 Continence plan completed within 21 days if applicable

  ContinencePlanIfApplicableIn21Days = audit_measure(
    stem_name = "ContinencePlanIfApplicableIn21Days",
    description = "Continence plan if applicable",
    exclusions = rlang::expr(
      .data[["S6UrinaryContinencePlanNoPlanReason"]] %in%
        c("PC", "PR")),
    numerators = rlang::expr(
        !! ssnap_field[["ClockStartToContinencePlanHours"]] <=
      (21 * 24)),
    new_numerators = rlang::expr(
      !! ssnap_field[["ClockStartToContinencePlanHours"]] <=
        (21 * 24)),
    csv_columns = c("S1PatientClockStartDateTime",
                    "S6UrinaryContinencePlanDate",
                    "S6UrinaryContinencePlanNoPlanReason"),
    measure_type = "discrete"),

# * -----------------------------------------------------------------
# * Nutrition -------------------------------------------------------
# * -----------------------------------------------------------------

  # NutritionalScreeningByDischarge =================================

  # J 16.1-3 Nutritional screening by discharge from inpatient care
  # K 3.1-3 Nutritional screening by discharge from inpatient care

  NutritionalScreeningByDischarge = audit_measure(
    stem_name = "NutritionScreened",
    description = "Nutritional screening by discharge",
    exclusions = NULL,
    numerators = rlang::expr(
      !is.na(.data[["S6MalnutritionScreening"]])),
    new_numerators = rlang::expr(
      !is.na(.data[["S6MalnutritionScreening"]])),
    csv_columns = "S6MalnutritionScreening",
    measure_type = "discrete"),

  # NutritionalScreeningHighRisk ====================================

  # J 16.4-6 If received nutritional screening, were they high risk?
  # K 3.4-6 If received nutritional screening, were they high risk?

  NutritionalScreeningHighRisk = audit_measure(
    stem_name = "NutritionScreeningHighRisk",
    description = "High risk on nutritional screening",
    exclusions = rlang::expr(
      is.na(.data[["S6MalnutritionScreening"]])),
    numerators = rlang::expr(.data[["S6MalnutritionScreening"]]),
    new_numerators = rlang::expr(.data[["S6MalnutritionScreening"]]),
    csv_columns = c("S6MalnutritionScreening",
                    "S6MalnutritionScreening"),
    measure_type = "discrete"),

  # NutritionSeenDietitian ==========================================

  # J 16.7-9 If high risk on nutritional screening, seen dietitian?
  # K 3.7-9 If high risk on nutritional screening, seen dietitian?

  NutritionSeenDietitian = audit_measure(
    stem_name = "NutritionScreened",
    description = "High risk on nutritional screening",
    exclusions = rlang::expr(
      .data[["S6MalnutritionScreening"]] != TRUE),
    numerators = rlang::expr(
      !is.na(.data[["S6MalnutritionScreeningDietitianDate"]])),
    new_numerators = rlang::expr(
      !is.na(.data[["S6MalnutritionScreeningDietitianDate"]])),
    csv_columns = c("S6MalnutritionScreeningDietitianDate",
                    "S6MalnutritionScreening"),
    measure_type = "discrete"),

  # NutritionalApplicability ========================================

  # J 16.10.1-12.1 Applicability for nutritional screening?
  # K 3.10.1-12.1 Applicability for nutritional screening?

  NutritionalApplicability = audit_measure(
    stem_name = "NutritionalApplicability",
    description = "High risk on nutritional screening",
    exclusions = rlang::expr(
      !is.na(.data[["S3PalliativeCareDecisionDate"]]) |
      !is.na(.data[["S6PalliativeCareByDischargeDate"]])),
    numerators = rlang::expr(
      .data[["S6MalnutritionScreening"]] |
        is.na(.data[["S6MalnutritionScreening"]])),
    new_numerators = rlang::expr(
      .data[["S6MalnutritionScreening"]] |
        is.na(.data[["S6MalnutritionScreening"]])),
    csv_columns = c("S6MalnutritionScreening",
                    "S3PalliativeCareDecisionDate",
                    "S6PalliativeCareByDischargeDate"),
    measure_type = "discrete"),


  # SeenDietitianIfApplicable =======================================

  # J 16.13.1-15.1 Seen dietitian if applicable?
  # K 3.13.1-15.1 Seen dietitian if applicable?

  SeenDietitianIfApplicable = audit_measure(
    stem_name = "SeenDietitianIfApplicable",
    description = "Seen dietitian if applicable?",
    exclusions = rlang::expr(
      !is.na(.data[["S3PalliativeCareDecisionDate"]]) |
        !is.na(.data[["S6PalliativeCareByDischargeDate"]])),
    numerators = rlang::expr(
      (.data[["S6MalnutritionScreening"]] |
        is.na(.data[["S6MalnutritionScreening"]])) &
        !is.na(.data[["S6MalnutritionScreeningDietitianDate"]])),
    new_numerators = rlang::expr(
      (.data[["S6MalnutritionScreening"]] |
         is.na(.data[["S6MalnutritionScreening"]])) &
        !is.na(.data[["S6MalnutritionScreeningDietitianDate"]])),
    csv_columns = c("S6MalnutritionScreening",
                    "S6MalnutritionScreeningDietitianDate",
                    "S3PalliativeCareDecisionDate",
                    "S6PalliativeCareByDischargeDate"),
    measure_type = "discrete"),

# * -----------------------------------------------------------------
# * Mood screen -----------------------------------------------------
# * -----------------------------------------------------------------

# MoodScreenGrouped =================================================

# J 17.1-11 Mood screening by discharge from inpatient care
# K 12.1-11 Mood screening by discharge from inpatient care

MoodScreenGrouped = audit_measure(
  stem_name = "MoodScreenGrouped",
  description = "Mood screened if applicable",
  exclusions = NULL,
  numerators = rlang::exprs(
    "Screened" = !is.na(.data[["S6MoodScreeningDate"]]),
    "NoOrg" =
      .data[["S6MoodScreeningNoScreeningReason"]] == "OR",
    "NoRefused" =
      .data[["S6MoodScreeningNoScreeningReason"]] == "PR",
    "NoMedicallyUnwell" =
      .data[["S6MoodScreeningNoScreeningReason"]] == "MU",
    "NoNotKnown" =
      .data[["S6MoodScreeningNoScreeningReason"]] == "NK"),
  new_numerators = tibble::tribble(
    ~numerator, ~fun, ~descriptor,
    "Screened",
      rlang::expr(!is.na(.data[["S6MoodScreeningDate"]])),
      "Screened",
    "NoOrg",
      rlang::expr(.data[["S6MoodScreeningNoScreeningReason"]] == "OR"),
      "No (organisational reasons)",
    "NoRefused",
      rlang::expr(.data[["S6MoodScreeningNoScreeningReason"]] == "PR"),
      "No (patient refused)",
    "NoMedicallyUnwell",
      rlang::expr(.data[["S6MoodScreeningNoScreeningReason"]] == "MU"),
      "No (patient medically unwell)",
    "NoNotKnown",
      rlang::expr(.data[["S6MoodScreeningNoScreeningReason"]] == "NK"),
      "No (reason not known)"),
  csv_columns = c("S6MoodScreeningDate",
                  "S6MoodScreeningNoScreeningReason"),
  measure_type = "discrete"),


  # MoodScreenApplicability =========================================

  # K12.12 Patient applicability for mood screening by discharge
  # (excludes patients whose total length of stay was less than 7
  # days)

#  MoodScreenApplicability = audit_measure(
#    stem_name = "MoodScreenApplicability",
#    description = glue::glue(
#      "Patient applicability for mood screening by discharge
#      (excludes patients whose total length of stay was less than 7
#      days)"),
#    exclusions = NULL,
#    numerators = rlang::expr(
#      !! ssnap_field[["LengthOfFirstInptStayHrs"]] >
#        (7 * 24)),
#    csv_columns = c("S1PatientClockStartDateTime",
#                    "S7FirstInptEpisodeClockStopDateTime"),
#    measure_type = "discrete"),


  # CognitiveScreenApplicability ====================================

  # K12.12 Patient applicability for mood screening by discharge
  # (excludes patients whose total length of stay was less than 7
  # days)

#  CognitiveScreenApplicability = audit_measure(
#    stem_name = "CognitiveScreenApplicability",
#    description = glue::glue(
#      "Patient applicability for cognition screening by discharge
#        (excludes patients whose total length of stay was less than 7
#        days)"),
#    exclusions = NULL,
#    numerators = rlang::expr(
#      !! ssnap_field[["LengthOfFirstInptStayHrs"]] >
#        (7 * 24)),
#    csv_columns = c("S1PatientClockStartDateTime",
#                    "S7FirstInptEpisodeClockStopDateTime"),
#    measure_type = "discrete"),


CognitionAndMoodAssessed = audit_measure(
  stem_name = "CognitionAndMoodAssessed",
  description = "Cognition and mood assessed",
  exclusions = rlang::expr(
    # Exclude anyone who refuses or is unwell (mood)
    (.data[["S6MoodScreeningNoScreeningReason"]]
     %in% c("MU", "PR")) |
      # Exclude anyone who refuses or is unwell (cognition)
      (.data[["S6CognitionScreeningNoScreeningReason"]]
       %in% c("MU", "PR")) |
      # Exclude anyone whose total length of stay is < 7 days AND
      # had one or both screens omitted
      ( (.data[["S7TeamClockStopDateTime"]] <
           (as.Date(.data[["S1PatientClockStartDateTime"]]) + 7)) &
          is.na(.data[["S6MoodScreeningDate"]]) &
          is.na(.data[["S6CognitionScreeningDate"]]))),
  numerators = rlang::expr( (.data[["S6MoodScreeningDate"]] <
    (as.Date(.data[["S1PatientClockStartDateTime"]]) + 42)) &
    (.data[["S6CognitionScreeningDate"]] <
    (as.Date(.data[["S1PatientClockStartDateTime"]]) + 42))),
  new_numerators = rlang::expr( (.data[["S6MoodScreeningDate"]] <
                               (as.Date(.data[["S1PatientClockStartDateTime"]]) + 42)) &
                              (.data[["S6CognitionScreeningDate"]] <
                                 (as.Date(.data[["S1PatientClockStartDateTime"]]) + 42))),
  csv_columns = c("S6MoodScreeningDate",
                  "S1PatientClockStartDateTime",
                  "S6CognitionScreeningDate",
                  "S6MoodScreeningNoScreeningReason",
                  "S6CognitionScreeningNoScreeningReason",
                  "S7TeamClockStopDateTime",
                  "S1PatientClockStartDateTime",
                  "S6MoodScreeningDate",
                  "S6CognitionScreeningDate"),
  measure_type = "discrete"),


  StayOver90PCInStrokeUnit = audit_measure(
    stem_name = "StayOver90PCInStrokeUnit",
    description = "Stay over 90% on stroke unit",
    exclusions = rlang::expr( (.data[["S1FirstWard"]] == "ICH") |
        (!is.na(.data[["S7DeathDate"]]) &
         (lubridate::day(.data[["S7DeathDate"]]) ==
         lubridate::day(!! ssnap_field[["TeamClockStart"]])) &
       (lubridate::month(.data[["S7DeathDate"]]) ==
         lubridate::month(!! ssnap_field[["TeamClockStart"]])) &
       (lubridate::year(.data[["S7DeathDate"]]) ==
         lubridate::year(!! ssnap_field[["TeamClockStart"]])))
      ),
    numerators = rlang::expr(!! ssnap_field[["StayPercent"]] >= 90),
    new_numerators = rlang::expr(!! ssnap_field[["StayPercent"]] >= 90),
    csv_columns = c("S1TeamClockStartDateTime",
                    "S7TeamClockStopDateTime",
                    "S4StrokeUnitArrivalDateTime",
                    "S7StrokeUnitDischargeDateTime",
                    "S1FirstWard",
                    "S7DeathDate"),
    measure_type = "discrete"),

  JointCarePlanning = audit_measure(
    stem_name = "JointCarePlanning",
    description = "Joint care planning",
    exclusions = rlang::expr(
      # Exclude patients who die or whose last known records
      # indicates transfer to another inpatient team.
      .data[["S7DischargeType"]] %in% c("D", "T")),
    numerators = rlang::expr(.data[["S7DischargeJointCarePlanning"]]),
    new_numerators = rlang::expr(.data[["S7DischargeJointCarePlanning"]]),
    csv_columns = c("S7DischargeJointCarePlanning",
                    "S7DischargeType"),
    measure_type = "discrete"),

  DischargedWithESD = audit_measure(
    stem_name = "DischargedWithESD",
    description = "Discharged with ESD",
    exclusions = rlang::expr(
      # Exclude patients who die or whose last known records
      # indicates transfer to another inpatient team.
      .data[["S7DischargeType"]] %in% c("D", "T")),
    numerators = rlang::expr(dplyr::case_when(
      (is.na(.data[["S7DischargedSpecialistEsdmt"]]) &
        is.na(.data[["S7DischargedSpecialistMcrt"]])) ~ FALSE,
      TRUE ~ (.data[["S7DischargedSpecialistEsdmt"]] |
              .data[["S7DischargedSpecialistMcrt"]]))),
    new_numerators = rlang::expr(dplyr::case_when(
      (is.na(.data[["S7DischargedSpecialistEsdmt"]]) &
         is.na(.data[["S7DischargedSpecialistMcrt"]])) ~ FALSE,
      TRUE ~ (.data[["S7DischargedSpecialistEsdmt"]] |
                .data[["S7DischargedSpecialistMcrt"]]))),
    csv_columns = c("S7DischargedSpecialistEsdmt",
                    "S7DischargedSpecialistMcrt",
                    "S7DischargeType"),
    measure_type = "discrete"),

  AFWithAnticoagulation = audit_measure(
    stem_name = "AFWithAnticoagulation",
    description = "AF discharged with anticoagulation plan",
    exclusions = rlang::expr(
      # Exclude patients who die or whose last known records indicates
      # transfer to another inpatient team.
      # Exclude patients who are not in atrial fibrillation
      # Patients with "no but" for anticoagulation are excluded
      (.data[["S7DischargeType"]] %in% c("D", "T")) |
        (!.data[["S7DischargeAtrialFibrillation"]]) |
        (is.na(
          .data[["S7DischargeAtrialFibrillationAnticoagulation"]]))),
    numerators = rlang::expr(
      .data[["S7DischargeAtrialFibrillationAnticoagulation"]]),
    new_numerators = rlang::expr(
      .data[["S7DischargeAtrialFibrillationAnticoagulation"]]),
    csv_columns = c("S7DischargeAtrialFibrillationAnticoagulation",
                    "S7DischargeType",
                    "S7DischargeAtrialFibrillation",
                    "S7DischargeAtrialFibrillationAnticoagulation"),
    measure_type = "discrete"),


  DischargeNamedContact = audit_measure(
    stem_name = "DischargeNamedContact",
    description = "Discharged with named contact",
    exclusions = rlang::expr(
      # Exclude patients who die or whose last known records
      # indicates transfer to another inpatient team.
      .data[["S7DischargeType"]] %in% c("D", "T")),
    numerators = rlang::expr(.data[["S7DischargeNamedContact"]]),
    new_numerators = rlang::expr(.data[["S7DischargeNamedContact"]]),
    csv_columns = c("S7DischargeNamedContact",
                    "S7DischargeType"),
    measure_type = "discrete"),


  SixMonthFollowUp = audit_measure(
    stem_name = "SixMonthFollowUp",
    description = "Breakdown of six month follow-up provision",
    exclusions = NULL,
    numerators = rlang::exprs(
      "Yes" = .data[["S8FollowUp"]] == "Y",
      "DiedOnAcutePathway" = is.null(.data[["S8FollowUp"]]) &
        (.data[["S7DischargeType"]] == "D"),
      "DiedWithin6Months" = .data[["S8FollowUp"]] == "ND",
      "NoBut" = .data[["S8FollowUp"]] == "NB",
      "No" = .data[["S8FollowUp"]] == "N",
      "Blank" = is.null(.data[["S8FollowUp"]]) &
        (.data[["S7DischargeType"]] != "D")),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "Yes",
        rlang::expr(.data[["S8FollowUp"]] == "Y"),
        "Yes",
      "DiedOnAcutePathway",
        rlang::expr(is.null(.data[["S8FollowUp"]]) &
        (.data[["S7DischargeType"]] == "D")),
        "Died on acute pathway",
      "DiedWithin6Months",
        rlang::expr(.data[["S8FollowUp"]] == "ND"),
        "Died within 6 months",
      "NoBut",
        rlang::expr(.data[["S8FollowUp"]] == "NB"),
        "No (but reason given)",
      "No",
        rlang::expr(.data[["S8FollowUp"]] == "N"),
        "No (no reason given)",
      "Blank",
        rlang::expr(is.null(.data[["S8FollowUp"]]) &
        (.data[["S7DischargeType"]] != "D")),
        "Blank"),
    csv_columns = c("S8FollowUp",
                    "S7DischargeType"),
    measure_type = "discrete"),


  SixMonthAnsweredApplicability = audit_measure(
    stem_name = "SixMonthAnsweredApplicability",
    description = glue::glue(
      "Patient record deemed appropriate to be actively answered
      (excludes died in care)"),
    exclusions = NULL,
    numerators = rlang::expr(.data[["S7DischargeType"]] != "D"),
    new_numerators = rlang::expr(.data[["S7DischargeType"]] != "D"),
    csv_columns = "S7DischargeType",
    measure_type = "discrete"),


  SixMonthAnswered = audit_measure(
    stem_name = "SixMonthAnswered",
    description = glue::glue(
      "Section 8 has been actively answered, if record appropriate
      for completion"),
    exclusions = rlang::expr(.data[["S7DischargeType"]] == "D"),
    numerators = rlang::expr(!is.null(.data[["S8FollowUp"]])),
    new_numerators = rlang::expr(!is.null(.data[["S8FollowUp"]])),
    csv_columns = c("S8FollowUp",
                    "S7DischargeType"),
    measure_type = "discrete"),

  SixMonthFollowupApplicability = audit_measure(
    stem_name = "SixMonthFollowupApplicability",
    description = glue::glue(
      "Applicability for follow-up to be undertaken (excludes died
      in care, died within 6 months of admission, and 'no but')"),
    exclusions = NULL,
    numerators = rlang::expr( (.data[["S7DischargeType"]] != "D") &
                              (.data[["S8FollowUp"]] != "ND") &
                              (.data[["S8FollowUp"]] != "NB")),
    new_numerators = rlang::expr( (.data[["S7DischargeType"]] != "D") &
                                (.data[["S8FollowUp"]] != "ND") &
                                (.data[["S8FollowUp"]] != "NB")),
    csv_columns = c("S7DischargeType",
                    "S8FollowUp"),
    measure_type = "discrete"),

  SixMonthFollowupCompleted = audit_measure(
    stem_name = "SixMonthFollowupCompleted",
    description = glue::glue(
      "Six month followup completed"),
    exclusions = rlang::expr( (.data[["S7DischargeType"]] == "D") |
                              (.data[["S8FollowUp"]] == "ND") |
                              (.data[["S8FollowUp"]] == "NB")),
    numerators = rlang::expr(.data[["S7DischargeType"]] == "Y"),
    new_numerators = rlang::expr(.data[["S7DischargeType"]] == "Y"),
    csv_columns = c("S7DischargeType",
                    "S7DischargeType",
                    "S8FollowUp"),
    measure_type = "discrete"),

  # ClockStartTo6MonthAssessmentMonths ==============================

  ClockStartTo6MonthAssessmentMonths = audit_measure(
    stem_name = "ClockStartTo6MonthAssessmentMonths",
    description =
      "Number of months from Clock Start to six month assessment",
    exclusions = rlang::expr(.data[["S8FollowUp"]] != "Y"),
    numerators = ssnap_field[["ClockStartTo6MonthAssessmentMonths"]],
    new_numerators = ssnap_field[["ClockStartTo6MonthAssessmentMonths"]],
    csv_columns = c("S1PatientClockStartDateTime",
                    "S8FollowUpDate",
                    "S8FollowUp"),
    measure_type = "continuous"),

  DischargeTo6MonthAssessmentMonths = audit_measure(
    stem_name = "DischargeTo6MonthAssessmentMonths",
    description =
      "Number of months from discharge to six month assessment",
    exclusions = rlang::expr(.data[["S8FollowUp"]] != "Y"),
    numerators = ssnap_field[["DischargeTo6MonthAssessmentMonths"]],
    new_numerators = ssnap_field[["DischargeTo6MonthAssessmentMonths"]],
    csv_columns = c("S7HospitalDischargeDateTime",
                    "S8FollowUpDate",
                    "S8FollowUp"),
    measure_type = "continuous"),

  FollowUpType = audit_measure(
    stem_name = "FollowUpType",
    description = "Follow-up type",
    exclusions = rlang::expr(.data[["S8FollowUp"]] != "Y"),
    numerators = rlang::exprs(
      "InPerson"  = .data[["S8FollowUpType"]] == "IP",
      "Online"    = .data[["S8FollowUpType"]] == "O",
      "Telephone" = .data[["S8FollowUpType"]] == "T",
      "Post"      = .data[["S8FollowUpType"]] == "P"),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "InPerson",
        rlang::expr(.data[["S8FollowUpType"]] == "IP"),
        "In person",
      "Online",
        rlang::expr(.data[["S8FollowUpType"]] == "O"),
        "Online",
      "Telephone",
        rlang::expr(.data[["S8FollowUpType"]] == "T"),
        "Telephone",
      "Post",
        rlang::expr(.data[["S8FollowUpType"]] == "P"),
        "Post"),
    csv_columns = c("S8FollowUpType",
                    "S8FollowUp"),
    measure_type = "discrete"),

  FollowUpProvider = audit_measure(
    stem_name = "FollowUpProvider",
    description = "Follow-up provider",
    exclusions = rlang::expr(.data[["S8FollowUp"]] != "Y"),
    numerators = rlang::exprs(
      "GP"                = .data[["S8FollowUpBy"]] == "GP",
      "StrokeCoordinator" = .data[["S8FollowUpBy"]] == "SC",
      "Therapist"         = .data[["S8FollowUpBy"]] == "T",
      "CommunityNurse"    = .data[["S8FollowUpBy"]] == "DN",
      "Voluntary"         = .data[["S8FollowUpBy"]] == "VS",
      "SecondaryCare"     = .data[["S8FollowUpBy"]] == "SCC",
      "Other"             = .data[["S8FollowUpBy"]] == "O"),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "GP",
        rlang::expr(.data[["S8FollowUpBy"]] == "GP"),
        "GP",
      "StrokeCoordinator",
        rlang::expr(.data[["S8FollowUpBy"]] == "SC"),
        "Stroke coordinator",
      "Therapist",
        rlang::expr(.data[["S8FollowUpBy"]] == "T"),
        "Therapist",
      "CommunityNurse",
        rlang::expr(.data[["S8FollowUpBy"]] == "DN"),
        "Community nurse",
      "Voluntary",
        rlang::expr(.data[["S8FollowUpBy"]] == "VS"),
        "Voluntary",
      "SecondaryCare",
        rlang::expr(.data[["S8FollowUpBy"]] == "SCC"),
        "Secondary care",
      "Other",
        rlang::expr(.data[["S8FollowUpBy"]] == "O"),
        "Other"),
    csv_columns = c("S8FollowUpBy",
                    "S8FollowUp"),
    measure_type = "discrete"),

  FollowUpMood = audit_measure(
    stem_name = "FollowUpMood",
    description = "Mood, behaviour, cognition screening",
    exclusions = rlang::expr(.data[["S8FollowUp"]] != "Y"),
    numerators = rlang::exprs(
      "Yes"   = .data[["S8MoodBehaviourCognitiveScreened"]] == TRUE,
      "No"    = .data[["S8MoodBehaviourCognitiveScreened"]] == FALSE,
      "NoBut" = is.na(.data[["S8MoodBehaviourCognitiveScreened"]])),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "Yes",
        rlang::expr(.data[["S8MoodBehaviourCognitiveScreened"]] == TRUE),
        "Yes",
      "No",
        rlang::expr(.data[["S8MoodBehaviourCognitiveScreened"]] == FALSE),
        "No",
      "NoBut",
        rlang::expr(is.na(.data[["S8MoodBehaviourCognitiveScreened"]])),
        "No but"),
    csv_columns = c("S8MoodBehaviourCognitiveScreened",
                    "S8FollowUp"),
    measure_type = "discrete"),


  FollowUpMoodSupportNeeded = audit_measure(
    stem_name = "FollowUpMood",
    description = "Mood, behaviour, cognition screening",
    exclusions = rlang::expr(
      .data[["S8MoodBehaviourCognitiveScreened"]] != "Y"),
    numerators = rlang::expr(
      .data[["S8MoodBehaviourCognitiveSupportNeeded"]] == TRUE),
    new_numerators = rlang::expr(
      .data[["S8MoodBehaviourCognitiveSupportNeeded"]] == TRUE),
    csv_columns = c("S8MoodBehaviourCognitiveSupportNeeded",
                    "S8MoodBehaviourCognitiveScreened"),
    measure_type = "discrete"),

  FollowUpPsychologicalSupport = audit_measure(
    stem_name = "FollowUpPsychologicalSupport",
    description = glue::glue(
      "If support needed, psychological support received since
      discharge"),
    exclusions = rlang::expr(
      .data[["S8MoodBehaviourCognitiveSupportNeeded"]] != TRUE),
    numerators = rlang::exprs(
      "Yes"   = .data[[
        "S8MoodBehaviourCognitivePsychologicalSupport"]] == TRUE,
      "No"    = .data[[
        "S8MoodBehaviourCognitivePsychologicalSupport"]] == FALSE,
      "NoBut" = is.na(.data[[
        "S8MoodBehaviourCognitivePsychologicalSupport"]])),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "Yes",
        rlang::expr(.data[["S8MoodBehaviourCognitivePsychologicalSupport"]] == TRUE),
        "Yes",
      "No",
        rlang::expr(.data[["S8MoodBehaviourCognitivePsychologicalSupport"]] == FALSE),
        "No",
      "NoBut",
        rlang::expr(is.na(.data[["S8MoodBehaviourCognitivePsychologicalSupport"]])),
        "No but"),
    csv_columns = c("S8MoodBehaviourCognitivePsychologicalSupport",
                    "S8MoodBehaviourCognitiveSupportNeeded"),
    measure_type = "discrete"),

  FollowUpLiving = audit_measure(
    stem_name = "FollowUpLiving",
    description = "Where the patient is living",
    exclusions = rlang::expr(.data[["S8FollowUp"]] != "Y"),
    numerators = rlang::exprs(
      "Home"     = .data[["S8Living"]] == "H",
      "CareHome" = .data[["S8Living"]] == "CH",
      "Other"    = .data[["S8Living"]] == "O"),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "Home",
        rlang::expr(.data[["S8Living"]] == "H"),
        "Home",
      "CareHome",
        rlang::expr(.data[["S8Living"]] == "CH"),
        "Care home",
      "Other",
        rlang::expr(.data[["S8Living"]] == "O"),
        "Other"),
    csv_columns = c("S8Living",
                    "S8FollowUp"),
    measure_type = "discrete"),


  FollowUpRankin6Month = audit_measure(
    stem_name = "FollowUpRankinNotKnown",
    description = "Modified Rankin Scale is not known",
    exclusions = rlang::expr(.data[["S8FollowUp"]] != "Y"),
    numerators = rlang::exprs(
      "0" = .data[["S8Rankin6Month"]] == 0,
      "1" = .data[["S8Rankin6Month"]] == 1,
      "2" = .data[["S8Rankin6Month"]] == 2,
      "3" = .data[["S8Rankin6Month"]] == 3,
      "4" = .data[["S8Rankin6Month"]] == 4,
      "5" = .data[["S8Rankin6Month"]] == 5,
      "6" = (.data[["S7DischargeType"]] == "D") |
              (.data[["S8FollowUp"]] == "ND"),
      "NotKnown" = is.na(.data[["S8Rankin6Month"]])),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "0",
        rlang::expr(.data[["S8Rankin6Month"]] == 0),
        "0",
      "1",
        rlang::expr(.data[["S8Rankin6Month"]] == 1),
        "1",
      "2",
        rlang::expr(.data[["S8Rankin6Month"]] == 2),
        "2",
      "3",
        rlang::expr(.data[["S8Rankin6Month"]] == 3),
        "3",
      "4",
        rlang::expr(.data[["S8Rankin6Month"]] == 4),
        "4",
      "5",
        rlang::expr(.data[["S8Rankin6Month"]] == 5),
        "5",
      "6",
        rlang::expr((.data[["S7DischargeType"]] == "D") |
        (.data[["S8FollowUp"]] == "ND")),
        "6",
      "NotKnown",
        rlang::expr(is.na(.data[["S8Rankin6Month"]])),
        "Not known"),
    csv_columns = c("S8Rankin6Month",
                    "S7DischargeType",
                    "S8FollowUp",
                    "S8FollowUp"),
    measure_type = "discrete"),


  FollowUpAtrialFibrillation = audit_measure(
    stem_name = "FollowUpAtrialFibrillation",
    description = glue::glue(
      "Persistent, permanent or paroxysmal Atrial Fibrillation (AF)
      at the time of six month follow-up assessment"),
    exclusions = rlang::expr(.data[["S8FollowUp"]] != "Y"),
    numerators = rlang::exprs(
      "Yes"   = .data[["S8PersistentAtrialFibrillation"]] == TRUE,
      "No"    = .data[["S8PersistentAtrialFibrillation"]] == FALSE,
      "NoBut" = is.na(.data[["S8PersistentAtrialFibrillation"]])),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "Yes",
        rlang::expr(.data[["S8PersistentAtrialFibrillation"]] == TRUE),
        "Yes",
      "No",
        rlang::expr(.data[["S8PersistentAtrialFibrillation"]] == FALSE),
        "No",
      "NoBut",
        rlang::expr(is.na(.data[["S8PersistentAtrialFibrillation"]])),
        "No but"),
    csv_columns = c("S8PersistentAtrialFibrillation",
                    "S8FollowUp"),
    measure_type = "discrete"),


  FollowUpAFFromAdmission = audit_measure(
    stem_name = "FollowUpAFFromAdmission",
    description = glue::glue(
      "If patient is in AF at six month follow-up assessment, was
      also in AF when first admitted to hospital"),
    exclusions = rlang::expr(
      .data[["S8PersistentAtrialFibrillation"]] != TRUE),
    numerators = rlang::expr(
      .data[["S2CoMAtrialFibrillation"]] == TRUE),
    new_numerators = rlang::expr(
      .data[["S2CoMAtrialFibrillation"]] == TRUE),
    csv_columns = c("S2CoMAtrialFibrillation",
                    "S8PersistentAtrialFibrillation"),
    measure_type = "discrete"),


  FollowUpAFFromDischarge = audit_measure(
    stem_name = "FollowUpAFFromDischarge",
    description = glue::glue(
      "If patient is in AF at six month follow-assessment, was also
      in AF when discharged from inpatient care"),
    exclusions = rlang::expr(
      .data[["S8PersistentAtrialFibrillation"]] != TRUE),
    numerators = rlang::expr(
      .data[["S7DischargeAtrialFibrillation"]] == TRUE),
    new_numerators = rlang::expr(
      .data[["S7DischargeAtrialFibrillation"]] == TRUE),
    csv_columns = c("S7DischargeAtrialFibrillation",
                    "S8PersistentAtrialFibrillation"),
    measure_type = "discrete"),


  FollowUpAFOnAnticoagulant = audit_measure(
    stem_name = "FollowUpAFOnAnticoagulant",
    description = glue::glue(
      "If patient is in AF at six month follow-up assessment, then
      taking anti-coagulant"),
    exclusions = rlang::expr(
      .data[["S8PersistentAtrialFibrillation"]] != TRUE),
    numerators = rlang::expr(
      .data[["S8TakingAnticoagulent"]] == TRUE),
    new_numerators = rlang::expr(
      .data[["S8TakingAnticoagulent"]] == TRUE),
    csv_columns = c("S8TakingAnticoagulent",
                    "S8PersistentAtrialFibrillation"),
    measure_type = "discrete"),


  FollowUpTakingAntiplatelet = audit_measure(
    stem_name = "FollowUpTakingAntiplatelet",
    description = "Taking antiplatelet",
    exclusions = rlang::expr(.data[["S8FollowUp"]] != "Y"),
    numerators = rlang::exprs(
      "Yes"   = .data[["S8TakingAntiplateletDrug"]] == TRUE,
      "No"    = .data[["S8TakingAntiplateletDrug"]] == FALSE,
      "NoBut" = is.na(.data[["S8TakingAntiplateletDrug"]])),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "Yes",
        rlang::expr(.data[["S8TakingAntiplateletDrug"]] == TRUE),
        "Yes",
      "No",
        rlang::expr(.data[["S8TakingAntiplateletDrug"]] == FALSE),
        "No",
      "NoBut",
        rlang::expr(is.na(.data[["S8TakingAntiplateletDrug"]])),
        "No but"),
    csv_columns = c("S8TakingAntiplateletDrug",
                    "S8FollowUp"),
    measure_type = "discrete"),


  FollowUpTakingAnticoagulant = audit_measure(
    stem_name = "FollowUpTakingAnticoagulant",
    description = "Taking anticoagulant",
    exclusions = rlang::expr(.data[["S8FollowUp"]] != "Y"),
    numerators = rlang::exprs(
      "Yes"   = .data[["S8TakingAnticoagulent"]] == TRUE,
      "No"    = .data[["S8TakingAnticoagulent"]] == FALSE,
      "NoBut" = is.na(.data[["S8TakingAnticoagulent"]])),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "Yes",
        rlang::expr(.data[["S8TakingAnticoagulent"]] == TRUE),
        "Yes",
      "No",
        rlang::expr(.data[["S8TakingAnticoagulent"]] == FALSE),
        "No",
      "NoBut",
        rlang::expr(is.na(.data[["S8TakingAnticoagulent"]])),
        "No but"),
    csv_columns = c("S8TakingAnticoagulent",
                    "S8FollowUp"),
    measure_type = "discrete"),


  FollowUpAnticoagulantSinceDischarge = audit_measure(
    stem_name = "FollowUpAnticoagulantSinceDischarge",
    description = glue::glue(
      "If patient was discharged on anti-coagulant, still taking at
      six month follow-up assessment"),
    exclusions = rlang::expr(
      .data[["S8TakingAnticoagulent"]] != TRUE),
    numerators = rlang::expr(.data[[
      "S7DischargeAtrialFibrillationAnticoagulation"]] == TRUE),
    new_numerators = rlang::expr(.data[[
      "S7DischargeAtrialFibrillationAnticoagulation"]] == TRUE),
    csv_columns = c("S7DischargeAtrialFibrillationAnticoagulation",
                    "S8TakingAnticoagulent"),
    measure_type = "discrete"),


  FollowUpTakingLipidLowering = audit_measure(
    stem_name = "FollowUpTakingLipidLowering",
    description = "Taking lipid lowering drug",
    exclusions = rlang::expr(.data[["S8FollowUp"]] != "Y"),
    numerators = rlang::exprs(
      "Yes"   = .data[["S8TakingLipidLowering"]] == TRUE,
      "No"    = .data[["S8TakingLipidLowering"]] == FALSE,
      "NoBut" = is.na(.data[["S8TakingLipidLowering"]])),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "Yes",
        rlang::expr(.data[["S8TakingLipidLowering"]] == TRUE),
        "Yes",
      "No",
        rlang::expr(.data[["S8TakingLipidLowering"]] == FALSE),
        "No",
      "NoBut",
        rlang::expr(is.na(.data[["S8TakingLipidLowering"]])),
        "No but"),
    csv_columns = c("S8TakingLipidLowering",
                    "S8FollowUp"),
    measure_type = "discrete"),


  FollowUpTakingAntihypertensive = audit_measure(
    stem_name = "FollowUpTakingAntihypertensive",
    description = "Taking antihypertensive",
    exclusions = rlang::expr(.data[["S8FollowUp"]] != "Y"),
    numerators = rlang::exprs(
      "Yes"   = .data[["S8TakingAntihypertensive"]] == TRUE,
      "No"    = .data[["S8TakingAntihypertensive"]] == FALSE,
      "NoBut" = is.na(.data[["S8TakingAntihypertensive"]])),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "Yes",
        rlang::expr(.data[["S8TakingAntihypertensive"]] == TRUE),
        "Yes",
      "No",
        rlang::expr(.data[["S8TakingAntihypertensive"]] == FALSE),
        "No",
      "NoBut",
        rlang::expr(is.na(.data[["S8TakingAntihypertensive"]])),
        "No but"),
    csv_columns = c("S8TakingAntihypertensive",
                    "S8FollowUp"),
    measure_type = "discrete"),


  SinceStrokeAnotherStroke = audit_measure(
    stem_name = "SinceStrokeAnotherStroke",
    description = "Since stroke, another stroke",
    exclusions = rlang::expr(.data[["S8FollowUp"]] != "Y"),
    numerators = rlang::exprs(
      "Yes"   = .data[["S8SinceStrokeAnotherStroke"]] == TRUE,
      "No"    = .data[["S8SinceStrokeAnotherStroke"]] == FALSE,
      "NoBut" = is.na(.data[["S8SinceStrokeAnotherStroke"]])),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "Yes",
        rlang::expr(.data[["S8SinceStrokeAnotherStroke"]] == TRUE),
        "Yes",
      "No",
        rlang::expr(.data[["S8SinceStrokeAnotherStroke"]] == FALSE),
        "No",
      "NoBut",
        rlang::expr(is.na(.data[["S8SinceStrokeAnotherStroke"]])),
        "No but"),
    csv_columns = c("S8SinceStrokeAnotherStroke",
                    "S8FollowUp"),
    measure_type = "discrete"),


  SinceStrokeMyocardialInfarction = audit_measure(
    stem_name = "SinceStrokeMyocardialInfarction",
    description = "Since stroke, myocardial infarction",
    exclusions = rlang::expr(.data[["S8FollowUp"]] != "Y"),
    numerators = rlang::exprs(
      "Yes"   = .data[["S8SinceStrokeMyocardialInfarction"]] == TRUE,
      "No"    = .data[["S8SinceStrokeMyocardialInfarction"]] == FALSE,
      "NoBut" = is.na(.data[["S8SinceStrokeMyocardialInfarction"]])),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "Yes",
        rlang::expr(.data[["S8SinceStrokeMyocardialInfarction"]] == TRUE),
        "Yes",
      "No",
        rlang::expr(.data[["S8SinceStrokeMyocardialInfarction"]] == FALSE),
        "No",
      "NoBut",
        rlang::expr(is.na(.data[["S8SinceStrokeMyocardialInfarction"]])),
        "No but"),
    csv_columns = c("S8SinceStrokeMyocardialInfarction",
                    "S8FollowUp"),
    measure_type = "discrete"),


  SinceStrokeHospitalisation = audit_measure(
    stem_name = "SinceStrokeHospitalisation",
    description =
      "Since stroke, other illness requiring hospitalisation",
    exclusions = rlang::expr(.data[["S8FollowUp"]] != "Y"),
    numerators = rlang::exprs(
      "Yes"   =
        .data[["S8SinceStrokeOtherHospitalisationIllness"]] == TRUE,
      "No"    =
        .data[["S8SinceStrokeOtherHospitalisationIllness"]] == FALSE,
      "NoBut" =
        is.na(.data[["S8SinceStrokeOtherHospitalisationIllness"]])),
    new_numerators = tibble::tribble(
      ~numerator, ~fun, ~descriptor,
      "Yes",
        rlang::expr(.data[["S8SinceStrokeOtherHospitalisationIllness"]] == TRUE),
        "Yes",
      "No",
        rlang::expr(.data[["S8SinceStrokeOtherHospitalisationIllness"]] == FALSE),
        "No",
      "NoBut",
        rlang::expr(is.na(.data[["S8SinceStrokeOtherHospitalisationIllness"]])),
        "No but"),
    csv_columns = c("S8SinceStrokeOtherHospitalisationIllness",
                    "S8FollowUp"),
    measure_type = "discrete"),


# ClinicianAssessedWithin1hr ===================================

ClinicianAssessedWithin1hr = audit_measure(
  stem_name = "ClinicianAssessedWithin1hr",
  description = "Assessed by stroke consultant or nurse within 1 hr",
  exclusions = NULL,
  numerators = rlang::expr(
    (!!ssnap_field[["ClockStartToConsultantMins"]] <= 60) |
      (!!ssnap_field[["ClockStartToStrokeNurseMins"]] <= 60)),
  new_numerators = rlang::expr(
    (!!ssnap_field[["ClockStartToConsultantMins"]] <= 60) |
      (!!ssnap_field[["ClockStartToStrokeNurseMins"]] <= 60)),
  csv_columns = c("S1PatientClockStartDateTime",
                  "S3StrokeConsultantAssessedDateTime",
                  "S3StrokeNurseAssessedDateTime"),
  measure_type = "discrete"),

# tPACriticalHourStandardWithin1hr =============================

tPACriticalHourStandardWithin1hr = audit_measure(
  stem_name = "tPACriticalHourStandardWithin1hr",
  description = glue::glue("
    If an ischaemic stroke arriving within 4hrs of onset,
    was thrombolysis administered within 1 hour"),
  exclusions = rlang::expr(
    (tidyr::replace_na(!! ssnap_field[["OnsetToArrivalTimeMins"]], 999)
       > (4 * 60)) | 
      (!.data[["S2StrokeTypeIsInfarct"]]) | 
    ((.data[["S2Thrombolysis"]] == "NB") &
    bitwAnd(.data[["S2ThrombolysisNoBut"]],
            ssnapinterface::tpa_no_but["Refusal"])) |
    ((.data[["S2Thrombolysis"]] == "NB") &
                    bitwAnd(.data[["S2ThrombolysisNoBut"]],
                            ssnapinterface::tpa_no_but["Medication"])) |
      .data[["S2IAI"]]
    ),
  numerators = rlang::expr(
    !! ssnap_field[["ClockStartToThrombolysisMins"]] <= 60),
  new_numerators = rlang::expr(
    !! ssnap_field[["ClockStartToThrombolysisMins"]] <= 60),
  csv_columns = c("S1PatientClockStartDateTime",
                  "S2ThrombolysisDateTime",
                  "S1OnsetInHospital",
                  "S1OnsetDateIsPrecise",
                  "S1OnsetTimeIsPrecise",
                  "S1OnsetDateTime",
                  "S1PatientClockStartDateTime",
                  "S2StrokeTypeIsInfarct",
                  "S2Thrombolysis",
                  "S2ThrombolysisNoBut",
                  "S2IAI"),
  measure_type = "discrete")
)
