#' Team and Patient 72hr Cohort Key Indicators
#' 
#' This is a vector of audit measures describing the outputs for the
#' team and patient 72hr cohort key indicators.
#' 
#' Internally it is used to create the team72hrKIs and patient72hrKIs
#' cohorts. It is exposed externally to allow customisation of the
#' output spec.

# * -----------------------------------------------------------------
# * aggregated_output_kis_72hrs (single create output) --------------
# * -----------------------------------------------------------------

aggregated_output_kis_72hrs <- tibble::tribble(
  ~x, ~numerator, ~category, ~output_type,
  # Domain 1 ========================================================

  ssnap_measures[["ClockStartToBrainImagingMins"]], NULL, "Scanning", "median",
  ssnap_measures[["BrainImagingGrouped"]], "Within1hr", "Scanning", "pct",
  ssnap_measures[["BrainImagingGrouped"]], "Within12hrs", "Scanning", "pct",

  # Domain 2 ========================================================

  ssnap_measures[["ClockStartToFirstStrokeUnitMins"]], NULL, "Stroke Unit", "median",
  ssnap_measures[["FirstSUWithin4hrs"]], NULL, "Stroke Unit", "pct",

  # Domain 3 ========================================================

  ssnap_measures[["Thrombolysis"]], "Yes", "Thrombolysis", "pct",
  ssnap_measures[["tPAIfMeetsRCPCriteria"]], NULL, "Thrombolysis", "pct",
  ssnap_measures[["ClockStartToThrombolysisMins"]], NULL, "Thrombolysis", "median",
  ssnap_measures[["tPAAndSUIn4hrs"]], NULL, "Thrombolysis", "pct",
  ssnap_measures[["tPAWithin1hr"]], NULL, "Thrombolysis", "pct",

  # Domain 4 ========================================================

  ssnap_measures[["StrokeConsultantReview"]], "Within24hrs", "Specialist Assessment", "pct",
  ssnap_measures[["ClockStartToConsultantMins"]], NULL, "Specialist Assessment", "median",
  ssnap_measures[["StrokeNurseReview"]], "Within24hrs", "Specialist Assessment", "pct",
  ssnap_measures[["ClockStartToStrokeNurseMins"]], NULL, "Specialist Assessment", "median",
  ssnap_measures[["SwallowScreenWithin4hrs"]], NULL, "Specialist Assessment", "pct",
  ssnap_measures[["SpLangTherapistSwallowWithin72hrs"]], NULL, "Specialist Assessment", "pct",

  # Domain 8 ========================================================

  ssnap_measures[["OccTherApplicableWithin72hrs"]], NULL, "Multidisciplinary team", "pct",
  ssnap_measures[["ClockStartToOTMins"]], NULL, "Multidisciplinary team", "median",
  ssnap_measures[["PhysioApplicableWithin72hrs"]], NULL, "Multidisciplinary team", "pct",
  ssnap_measures[["ClockStartToPTMins"]], NULL, "Multidisciplinary team", "median",
  ssnap_measures[["SLTCommApplicableWithin72hrs"]], NULL, "Multidisciplinary team", "pct",
  ssnap_measures[["ClockStartToSLTCommMins"]], NULL, "Multidisciplinary team", "median"
)

#' 72hr Audit Compliance Measures
#' 
#' This is a vector of audit measures describing the outputs for
#' establishing SSNAP audit compliance. Audit compliance is applied
#' to both 72hr and post-72hr measures.
#' 
#' Internally it is used to create the team72hrKIs and patient72hrKIs
#' cohorts. It is exposed externally to allow customisation of the
#' output spec.

# * -----------------------------------------------------------------
# * aggregated_output_ac_72hrs (single create output) ---------------
# * -----------------------------------------------------------------

aggregated_output_ac_72hrs <- tibble::tribble(
  ~x, ~numerator, ~category, ~output_type,
  ssnap_measures[["NihssCompletion"]], "Complete", "NIHSS at arrival", "d_n_pct",
  ssnap_measures[["Nihss24HrsKnown"]], NULL, "NIHSS 24h", "d_n_pct",
  ssnap_measures[["ClockStartToRecordCreationHours"]], "Admission data entry", NULL, "median",
  ssnap_measures[["EthnicityKnown"]], NULL, "72h measures", "pct",
  ssnap_measures[["SwallowScreen4hrsComplete"]], "72h measures", NULL, "pct",
  ssnap_measures[["SwallowScreen72hrsComplete"]], "72h measures", NULL, "pct",
  ssnap_measures[["OTAssessment72HrsComplete"]], "72h measures", NULL, "pct",
  ssnap_measures[["PTAssessment72HrsComplete"]], "72h measures", NULL, "pct",
  ssnap_measures[["SLTCommAssessment72HrsComplete"]], "72h measures", NULL, "pct",
  ssnap_measures[["SwallowAssessment72hrsComplete"]], "72h measures", NULL, "pct"
)

#' Casemix 72hr Cohort Aggregation of Measures
#' 
#' This is a vector of audit measures describing the measures used to
#' look at casemix from the 72hrs Cohorts.
#' 
#' Internally it is used to create the Casemix tab in the portfolio.

# * -----------------------------------------------------------------
# * aggregated_output_casemix_72hrs (single create output) ----------
# * -----------------------------------------------------------------

aggregated_output_casemix_72hrs <- tibble::tribble(
  ~x, ~numerator, ~category, ~output_type,

  # Gender ==========================================================

  ssnap_measures[["Gender"]], NULL, "Gender", "d_n_pct",

  # Age =============================================================

  ssnap_measures[["AgeOver80"]], NULL, "Age", "d_n_pct",
# TODO the below give errors:
#   Error in summarise_impl(.data, dots) :
#   Evaluation error: `false` must be type integer, not logical.
#  ssnap_measures[["FemaleAgeOver80"]], NULL, "quartiles",
#  ssnap_measures[["MaleAgeOver80"]], NULL, "quartiles",
  ssnap_measures[["AgeOnArrivalGrouped"]], NULL, "Age", "d_n_pct",

  # Comorbidities ===================================================

  ssnap_measures[["ComorbidityCount"]], NULL, "Comorbidities", "d_n_pct",
  ssnap_measures[["CongestiveHeartFailure"]], NULL, "Comorbidities", "d_n_pct",
  ssnap_measures[["Hypertension"]], NULL, "Comorbidities", "d_n_pct",
  ssnap_measures[["Diabetes"]], NULL, "Comorbidities", "d_n_pct",
  ssnap_measures[["PreviousStrokeTIA"]], NULL, "Comorbidities", "d_n_pct",

  # Atrial fibrillation  ============================================

  ssnap_measures[["AtrialFibrillation"]], NULL, "Atrial fibrillation", "d_n_pct",
  ssnap_measures[["AFOnAntiplatelet"]], NULL, "Atrial fibrillation", "d_n_pct",
  ssnap_measures[["AFOnAnticoagulant"]], NULL, "Atrial fibrillation", "d_n_pct",
  ssnap_measures[["AFMeds"]], NULL, "Atrial fibrillation", "d_n_pct",

  # Stroke type  ====================================================

  ssnap_measures[["StrokeType"]], NULL, "Stroke type", "d_n_pct",

  # mRS before stroke ===============================================

  ssnap_measures[["RankinBeforeStroke"]], NULL, "Modified Rankin score", "d_n_pct",

  # NIHSS ===========================================================

  ssnap_measures[["LOCArrival"]], NULL, "NIHSS", "d_n_pct",
  ssnap_measures[["NihssCompletion"]], NULL, "NIHSS", "d_n_pct",
  ssnap_measures[["NihssOnArrivalGrouped"]], NULL, "NIHSS", "d_n_pct",
  ssnap_measures[["NihssOnArrival"]], NULL, "NIHSS", "quartiles",

  # Palliative care =================================================

  ssnap_measures[["EndOfLifeWithin72hrs"]], NULL, "Palliative care", "d_n_pct",
  ssnap_measures[["EndOfLifePathway"]], NULL, "Palliative care", "d_n_pct",
  ssnap_measures[["ClockStartToPalliativeCare72Hours"]], NULL, "Palliative care",
    "quartiles",
  ssnap_measures[["OnsetInHospital"]], NULL, "In hospital stroke",
    "d_n_pct"
)

#' Portfolio 72hr Cohort Aggregation of Measures
#' 
#' This is a vector of audit measures describing the measures used to
#' create the 72hr portfolio. This is a large dataset and so should
#' only be used for this purpose: it is quicker to create a custom
#' audit spec for specific purposes.

# * -----------------------------------------------------------------
# * aggregated_output_portfolio_72hrs (single create output) --------
# * -----------------------------------------------------------------

aggregated_output_portfolio_72hrs <- tibble::tribble(
  ~x, ~numerator, ~category, ~output_type,

  # Case mix ========================================================

  ssnap_measures[["AgeOnArrival"]], NULL, "Casemix", "median",
  ssnap_measures[["StrokeType"]], NULL, "Casemix", "pct",
  ssnap_measures[["RankinBeforeStrokeGrouped"]], NULL, "Casemix", "pct",
  ssnap_measures[["LOCArrivalGrouped"]], NULL, "Casemix", "pct",
  ssnap_measures[["NihssOnArrival"]], NULL, "Casemix", "quartiles",
  ssnap_measures[["EndOfLifeWithin72hrs"]], NULL, "Casemix", "pct",

  # Onset of symptoms ===============================================

  ssnap_measures[["OnsetDateAccuracy"]], NULL, "Onset of symptoms", "d_n_pct",
  ssnap_measures[["OnsetTimeAccuracy"]], NULL, "Onset of symptoms", "d_n_pct",
  ssnap_measures[["OnsetTimeKnown"]], NULL, "Onset of symptoms", "d_n_pct",
  ssnap_measures[["OnsetToArrivalGrouped"]], NULL, "Onset of symptoms", "d_n_pct",

  # Timings from onset ==============================================

  ssnap_measures[["OnsetToArrivalTimeMins"]], NULL, "Timings from onset", "quartiles",
  ssnap_measures[["OnsetToFirstStrokeUnitMins"]], NULL, "Timings from onset", "quartiles",
  ssnap_measures[["OnsetToBrainImagingMins"]], NULL, "Timings from onset", "quartiles",
  ssnap_measures[["OnsetToThrombolysisMins"]], NULL, "Timings from onset", "quartiles",

  # Arrival by ambulance ============================================

  ssnap_measures[["ArrivalByAmbulance"]], NULL, "Ambulance", "d_n_pct",

  # Day and time of arrival (TODO) ==================================

  # Scanning ========================================================

  ssnap_measures[["BrainImagingGrouped"]], NULL, "Scanning", "d_n_pct",
  ssnap_measures[["ClockStartToBrainImagingMins"]], NULL, "Scanning", "quartiles",

  # Admission to stroke unit ========================================

  ssnap_measures[["WentToAStrokeUnit"]], NULL, "Admission to stroke unit", "d_n_pct",
  ssnap_measures[["ClockStartToFirstStrokeUnitMins"]], NULL, "Admission to stroke unit", "quartiles",
  ssnap_measures[["FirstWard"]], NULL, "Admission to stroke unit", "d_n_pct",
  ssnap_measures[["FirstSUWithin4hrs"]], NULL, "Admission to stroke unit", "d_n_pct",
  # TODO Out of hours and SU within 4 hrs.

  # Stroke nurse ====================================================

  ssnap_measures[["StrokeNurseReview"]], NULL, "Stroke nurse", "d_n_pct",
  ssnap_measures[["ClockStartToStrokeNurseMins"]], NULL, "Stroke nurse", "quartiles",

  # Stroke consultant ===============================================

  ssnap_measures[["StrokeConsultantReview"]], NULL, "Stroke consultant", "d_n_pct",
  ssnap_measures[["ClockStartToConsultantMins"]], NULL, "Stroke consultant", "quartiles",

  # Occupational Therapy ============================================

  ssnap_measures[["OccTherReviewGrouped"]], NULL, "Occupational therapy", "d_n_pct",
  ssnap_measures[["ClockStartToOTMins"]], NULL, "Occupational therapy", "quartiles",
  ssnap_measures[["OccTherApplicability"]], NULL, "Occupational therapy", "d_n_pct",
  ssnap_measures[["OccTherApplicableWithin72hrs"]], NULL, "Occupational therapy", "d_n_pct",
  ssnap_measures[["OccTherApplicableWithin24hrs"]], NULL, "Occupational therapy", "d_n_pct",

  # Physiotherapy ===================================================

  ssnap_measures[["PhysioReviewGrouped"]], NULL, "Physiotherapy", "d_n_pct",
  ssnap_measures[["ClockStartToPTMins"]], NULL, "Physiotherapy", "quartiles",
  ssnap_measures[["PhysioApplicability"]], NULL, "Physiotherapy", "d_n_pct",
  ssnap_measures[["PhysioApplicableWithin72hrs"]], NULL, "Physiotherapy", "d_n_pct",
  ssnap_measures[["PhysioApplicableWithin24hrs"]], NULL, "Physiotherapy", "d_n_pct",

  # Speech and language therapy (communication) =====================

  ssnap_measures[["SLTCommReviewGrouped"]], NULL, "Speech and language therapy", "d_n_pct",
  ssnap_measures[["ClockStartToSLTCommMins"]], NULL, "Speech and language therapy", "quartiles",
  ssnap_measures[["SLTCommApplicability"]], NULL, "Speech and language therapy", "d_n_pct",
  ssnap_measures[["SLTCommApplicableWithin72hrs"]], NULL, "Speech and language therapy", "d_n_pct",
  ssnap_measures[["SLTCommApplicableWithin24hrs"]], NULL, "Speech and language therapy", "d_n_pct",

  # First 72hr bundle (TODO) ========================================

  # Swallow screening ===============================================

  ssnap_measures[["SwallowScreen4hrsGrouped"]], NULL, "Swallow screening", "d_n_pct",
  ssnap_measures[["ClockStartToSwallowScreen4hrsMins"]], NULL, "Swallow screening", "quartiles",
  ssnap_measures[["SwallowScreen4hrsApplicability"]], NULL, "Swallow screening", "d_n_pct",
  ssnap_measures[["SwallowScreenWithin4hrs"]], NULL, "Swallow screening", "d_n_pct",
#  create_output( # TODO this has an error on it
#    ssnap_measures[["SwallowScreen72hrsGrouped"]],
#    output_type = "d_n_pct")
#  create_output(
#    ssnap_measures[["ClockStartToSwallowScreen72hrsMins"]],
#    output_type = "quartiles"),
#  create_output(
#    ssnap_measures[["SwallowScreen72hrsApplicability"]],
#    output_type = "d_n_pct"),
#  create_output(
#    ssnap_measures[["SwallowScreenWithin72hrs"]],
#    output_type = "d_n_pct"),

  # Swallow assessment ==============================================

  ssnap_measures[["ClockStartToSpLangTherapistSwallowMins"]], NULL, "Swallow assessment", "quartiles",
  ssnap_measures[["SpLangTherapistSwallow72hrsGrouped"]], NULL, "Swallow assessment", "d_n_pct",
  ssnap_measures[["SpLangTherapistSwallowApplicability"]], NULL, "Swallow assessment", "d_n_pct",
  ssnap_measures[["SpLangTherapistSwallowWithin72hrs"]], NULL, "Swallow assessment", "d_n_pct",

  # Thrombolysis ====================================================

  ssnap_measures[["Thrombolysis"]], NULL, "Thrombolysis", "d_n_pct",
  ssnap_measures[["ClockStartToThrombolysisMins"]], NULL, "Thrombolysis", "quartiles",
  ssnap_measures[["ThrombolysedOnsetToClockStartMins"]], NULL, "Thrombolysis", "median",
  ssnap_measures[["ThrombolysedClockStartToBrainImagingMins"]], NULL, "Thrombolysis", "median",
  ssnap_measures[["ThrombolysedBrainImagingToNeedleMins"]], NULL, "Thrombolysis", "median",
  ssnap_measures[["EligibleFortPAByRCPCriteria"]], NULL, "Thrombolysis", "d_n_pct",
  ssnap_measures[["tPAIfMeetsRCPCriteria"]], NULL, "Thrombolysis", "d_n_pct",
  ssnap_measures[["tPAIfDoesntMeetRCPCriteria"]], NULL, "Thrombolysis", "d_n_pct",
  ssnap_measures[["EligibleOrThrombolysed"]], NULL, "Thrombolysis", "d_n_pct",
  ssnap_measures[["EligibleExcludingNoButs"]], NULL, "Thrombolysis", "d_n_pct",
  ssnap_measures[["tPAWithin1hr"]], NULL, "Thrombolysis", "d_n_pct",
  ssnap_measures[["tPAAndSUIn4hrs"]], NULL, "Thrombolysis", "d_n_pct",

  # Thrombolysis complications ======================================

  ssnap_measures[["ThrombolysisComplications"]], NULL, "Thrombolysis complications", "d_n_pct",
  ssnap_measures[["ThrombolysisComplicationsGrouped"]], NULL, "Thrombolysis complications", "d_n_pct",

  # NIHSS following thrombolysis ====================================

  ssnap_measures[["Nihss24HrsKnown"]], NULL, "NIHSS after thrombolysis", "d_n_pct",
  ssnap_measures[["Nihss24hrsSeverity"]], NULL, "NIHSS after thrombolysis", "d_n_pct",
  ssnap_measures[["ChangeInNIHSSAt24hrs"]], NULL, "NIHSS after thrombolysis", "d_n_pct",
  ssnap_measures[["Nihss24hrsScoreChange"]], NULL, "NIHSS after thrombolysis", "d_n_pct",

  # Onset to arrival ================================================

  ssnap_measures[["DaysOnsetToArrival"]], NULL, "Days onset to arrival", "d_n_pct",

  # Thrombectomy ====================================================

  ssnap_measures[["Thrombectomy"]], NULL, "Thrombectomy", "d_n_pct",
  ssnap_measures[["OnsetToIAIPunctureMins"]], NULL, "Thrombectomy", "quartiles",
  ssnap_measures[["OnsetToIAICompletionMins"]], NULL, "Thrombectomy", "quartiles",
  ssnap_measures[["ClockStartToIAIPunctureMins"]], NULL, "Thrombectomy", "quartiles",
  ssnap_measures[["PunctureToDeploymentMins"]], NULL, "Thrombectomy", "quartiles",
  ssnap_measures[["PunctureToIAICompletionMins"]], NULL, "Thrombectomy", "quartiles",
  ssnap_measures[["Nihss24hrsAfterIAIKnown"]], NULL, "Thrombectomy", "d_n_pct"
)

#' Portfolio post-72hr Cohort 7Day Aggregation of Measures
#' 
#' This is a vector of audit measures describing the measures used to
#' create the post-72hr portfolio, 7-day cohort. This is a large
#' dataset and so should only be used for this purpose: it is quicker
#' to create a custom audit spec for specific purposes.

# * -----------------------------------------------------------------
# * aggregated_output_portfolio_post72hrs_7day (single create output)
# * -----------------------------------------------------------------

aggregated_output_portfolio_post72hrs_7day <- tibble::tribble(
  ~x, ~numerator, ~category, ~output_type,

  # Continence plan =================================================

  ssnap_measures[["ContinencePlanGrouped"]], NULL, "Continence plan", "d_n_pct",
  ssnap_measures[["ClockStartToContinencePlanHours"]], NULL, "Continence plan", "quartiles",
  ssnap_measures[["ContinencePlanApplicability"]], NULL, "Continence plan", "d_n_pct",
  ssnap_measures[["ContinencePlanIfApplicable"]], NULL, "Continence plan", "d_n_pct",

  # Nutrition =======================================================

  ssnap_measures[["NutritionalScreeningByDischarge"]], NULL, "Nutrition", "d_n_pct",
  ssnap_measures[["NutritionalScreeningHighRisk"]], NULL, "Nutrition", "d_n_pct",
  ssnap_measures[["NutritionSeenDietitian"]], NULL, "Nutrition", "d_n_pct",
  ssnap_measures[["NutritionalApplicability"]], NULL, "Nutrition", "d_n_pct",
  ssnap_measures[["SeenDietitianIfApplicable"]], NULL, "Nutrition", "d_n_pct",

  # Level of consciousness ==========================================

  ssnap_measures[["WorstLOCFirst7Days"]], NULL, "Level of consciousness", "d_n_pct",

  # Urinary tract infection =========================================

  ssnap_measures[["UrinaryTractInfection"]], NULL, "Urinary tract infection", "d_n_pct",

  # PneumoniaAntibiotics ============================================

  ssnap_measures[["PneumoniaAntibiotics"]], NULL, "Urinary tract infection", "d_n_pct"

  # K7.1 and K7.2 are omitted because a median of a categorical variable
  # makes no sense.
  # K7.3 onwards also makes no sense given LOC can't worsen from the
  # 'Worst in first 7 days'.

)

#' Portfolio post-72hr Cohort Discharge Aggregation of Measures
#' 
#' This is a vector of audit measures describing the measures used to
#' create the post-72hr portfolio, discharge cohort. This is a large
#' dataset and so should only be used for this purpose: it is quicker
#' to create a custom audit spec for specific purposes.

# * -----------------------------------------------------------------
# * aggregated_output_portfolio_post72hrs_7day (single create output)
# * -----------------------------------------------------------------

aggregated_output_portfolio_post72hrs_discharge <- tibble::tribble(
  ~x, ~numerator, ~category, ~output_type,

  # Continence plan =================================================

  ssnap_measures[["ContinencePlanIfApplicableIn21Days"]], NULL, "Continence plan", "d_n_pct",

  # Mood screen =====================================================

  ssnap_measures[["MoodScreenGrouped"]], NULL, "Mood screen", "d_n_pct",
  ssnap_measures[["MoodScreenApplicability"]], NULL, "Mood screen", "d_n_pct",

  # TODO Lots missing here

  # Cognition screen ================================================

  ssnap_measures[["CognitiveScreenApplicability"]], NULL, "Mood screen", "d_n_pct",

  # TODO Lots missing here

  # Mood and cognition ==============================================

  # TODO Lots missing here

  # Occupational therapy ============================================

#  ssnap_measures[["OccTherApplicable"]], NULL, "Occupational therapy", "d_n_pct",

)


#' Portfolio Discharge Cohort Key Indicators
#' 
#' This is a vector of audit measures describing the measures used to
#' create the 72hr portfolio. This is a large dataset and so should
#' only be used for this purpose: it is quicker to create a custom
#' audit spec for specific purposes.

# * -----------------------------------------------------------------
# * aggregated_output_kis_discharge (single create output) ----------
# * -----------------------------------------------------------------

aggregated_output_kis_discharge <- tibble::tribble(
  ~x, ~numerator, ~output_type,

  # Domain 2 ========================================================

  #TODO: 90% stay is returning NAs
#  ssnap_measures[["StayOver90PCInStrokeUnit"]], NULL, "pct",

  # Domain 9 ========================================================

  ssnap_measures[["ContinencePlanIfApplicableIn21Days"]], NULL, "pct",
  ssnap_measures[["CognitionAndMoodAssessed"]], NULL, "pct",

  # Domain 10 =======================================================

  ssnap_measures[["JointCarePlanning"]], NULL, "pct",
  ssnap_measures[["DischargedWithESD"]], NULL, "pct",
  ssnap_measures[["AFWithAnticoagulation"]], NULL, "pct",
  ssnap_measures[["DischargeNamedContact"]], NULL, "pct"
)

#' Team post-72hr (all teams) Cohort Key Indicators
#' 
#' This is a vector of audit measures describing the measures used to
#' create the all teams post-72hr key indicators. This is used in
#' performance reporting.

# * -----------------------------------------------------------------
# * aggregated_output_kis_team_post72hrs ----------------------------
# * -----------------------------------------------------------------

aggregated_output_kis_team_post72hrs <- tibble::tribble(
  ~x, ~numerator, ~output_type,

  # Domain 5 ========================================================

  ssnap_measures[["OccTherApplicable"]], NULL, "pct",
  ssnap_measures[["PCOccTherDays"]], NULL, "median",
  ssnap_measures[["OccTherMinutes"]], NULL, "median",

  # Domain 6 ========================================================

  ssnap_measures[["PhysioApplicable"]], NULL, "pct",
  ssnap_measures[["PCPhysioDays"]], NULL, "median",
  ssnap_measures[["PhysioMinutes"]], NULL, "median",

  # Domain 7 ========================================================

  ssnap_measures[["SpeechLangApplicable"]], NULL, "pct",
  ssnap_measures[["PCSpeechLangDays"]], NULL, "median",
  ssnap_measures[["SpeechLangMinutes"]], NULL, "median"
)


#' Patient post-72hr (all teams) Cohort Key Indicators
#' 
#' This is a vector of audit measures describing the measures used to
#' create the all patient post-72hr key indicators. This is used in
#' performance reporting.

# * -----------------------------------------------------------------
# * aggregated_output_kis_patient_post72hrs ----------------------------
# * -----------------------------------------------------------------

aggregated_output_kis_patient_post72hrs <- tibble::tribble(
  ~x, ~numerator, ~output_type,
  
  # Domain 5 ========================================================
  
  ssnap_measures[["OccTherApplicable"]], NULL, "pct",
  ssnap_measures[["PCOccTherDays"]], NULL, "median",
  ssnap_measures[["OccTherMinutes"]], NULL, "median",
  
  # Domain 6 ========================================================
  
  ssnap_measures[["PhysioApplicable"]], NULL, "pct",
  ssnap_measures[["PCPhysioDays"]], NULL, "median",
  ssnap_measures[["PhysioMinutes"]], NULL, "median",
  
  # Domain 7 ========================================================
  
  ssnap_measures[["SpeechLangApplicable"]], NULL, "pct",
  ssnap_measures[["PCSpeechLangDays"]], NULL, "median",
  ssnap_measures[["SpeechLangMinutes"]], NULL, "median",

  # Domain 10 =======================================================

  ssnap_measures[["JointCarePlanning"]], NULL, "pct",
  ssnap_measures[["DischargedWithESD"]], NULL, "pct",
  ssnap_measures[["AFWithAnticoagulation"]], NULL, "pct",
  ssnap_measures[["DischargeNamedContact"]], NULL, "pct"
)

aggregated_output_kis_critical_hour <- tibble::tribble(
  ~x, ~numerator, ~category, ~output_type,
  
  ssnap_measures[["BrainImagingGrouped"]], "Within1hr", "Critical1Hr", "d_n_pct",
  ssnap_measures[["ClinicianAssessedWithin1hr"]], NULL, "Critical1Hr", "d_n_pct",
  ssnap_measures[["tPACriticalHourStandardWithin1hr"]], NULL, "Critical1Hr", "d_n_pct"
)

ssnap_measures_tests <- tibble::tribble(
  ~x, ~numerator, ~category, ~output_type,
  
  ssnap_measures[["Gender"]], "Male", "Gender", "d_n_pct",
  ssnap_measures[["AgeOver80"]], NULL, "Age", "d_n_pct",
  ssnap_measures[["AgeOnArrivalGrouped"]], NULL, "Age", "d_n_pct"
)

#' @export
ssnap_output_specs <- list(
  "Tests" = list(
    "cohort" = "Team72HrCohort",
    "outputs_table" = ssnap_measures_tests),
  
  "Team72HrCohortKIs" = list(
    "cohort" = "Team72HrCohort",
    "outputs_table" = aggregated_output_kis_72hrs),
  "Patient72HrCohortKIs" = list(
    "cohort" = "Patient72HrCohort",
    "outputs_table" = aggregated_output_kis_72hrs),
  "Team72HrCohortPortfolio" = list(
    "cohort" = "Team72HrCohort",
    "outputs_table" = aggregated_output_portfolio_72hrs),
  "Patient72HrCohortPortfolio" = list(
    "cohort" = "Patient72HrCohort",
    "outputs_table" = aggregated_output_portfolio_72hrs),
  "TeamCasemix" = list(
    "cohort" = "Team72HrCohort",
    "outputs_table" = aggregated_output_casemix_72hrs),
  "TeamDischargeCohortKIs" = list(
    "cohort" = "TeamDischargeCohort",
    "outputs_table" = aggregated_output_kis_discharge),
# TODO the 7-day cohort is bugged so we have to fudge by using the
# Discharge cohort to test with for now and fix this later.
  "TeamPost72hrsCohortPortfolio7Day" = list(
    "cohort" = "Team7DayCohort",
    "outputs_table" = aggregated_output_portfolio_post72hrs_7day),
  "TeamPost72HrsAllTeamsCohortKIs" = list(
    "cohort" = "TeamPost72HrsAllTeamsCohort",
    "outputs_table" = aggregated_output_kis_team_post72hrs),
  #TODO Pt post-7day cohort not yet defined
  "PatientPost72HrsAllTeamsCohortKIs" = list(
    "cohort" = "PatientPost72HrCohort",
    "outputs_table" = aggregated_output_kis_patient_post72hrs),
  "CriticalHourCohortKIs" = list(
    "cohort" = "CriticalHourCohort",
    "outputs_table" = aggregated_output_kis_critical_hour)
)
