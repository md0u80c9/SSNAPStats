#' @importFrom rlang "!!"

# Any fields that have to be referenced by other fields need to be
# declared first as their own names

# * ssnap_fields (defined before main list) -------------------------
# * -----------------------------------------------------------------

# OnsetToClockStartMins =============================================

onset_to_clock_start <- rlang::expr(
  dplyr::if_else(
    is.na(.data[["S1OnsetTimeIsPrecise"]]) |
      .data[["S1OnsetInHospital"]],
    NA_integer_,
    internal_time_difference(
      .data[["S1OnsetDateTime"]],
      .data[["S1PatientClockStartDateTime"]])
    )
)

# LengthOfStayAtTeamMins ============================================

length_of_stay_at_team_mins <- rlang::expr(
  internal_time_difference(
    .data[["S1TeamClockStartDateTime"]],
    .data[["S7TeamClockStopDateTime"]])
)

# LengthOfStayOnStrokeUnitTeamMins ==================================

length_of_stay_on_stroke_unit <- rlang::expr(
  internal_time_difference(
    .data[["S4StrokeUnitArrivalDateTime"]],
    .data[["S7StrokeUnitDischargeDateTime"]])
)

# TeamPhysioNeedMins ================================================

team_physio_need_mins <- rlang::expr(
  internal_time_difference(
    .data[["S1TeamClockStartDateTime"]],
    .data[["S4PhysioEndDate"]])
)

# TeamOccTherNeedMins ===============================================

team_occther_need_mins <- rlang::expr(
  internal_time_difference(
    .data[["S1TeamClockStartDateTime"]],
    .data[["S4OccTherEndDate"]])
)

# TeamSpeechLangNeedMins ============================================

team_speechlang_need_mins <- rlang::expr(
  internal_time_difference(
    .data[["S1TeamClockStartDateTime"]],
    .data[["S4SpeechLangEndDate"]])
)

# TeamPsychologyNeedMins ============================================

team_psychology_need_mins <- rlang::expr(
  internal_time_difference(
    .data[["S1TeamClockStartDateTime"]],
    .data[["S4PsychologyEndDate"]])
)

# TeamClockStart ====================================================

team_clockstart <- rlang::expr(
  dplyr::if_else(
    is.na(.data[["S4ArrivalDateTime"]]),
    .data[["S1FirstArrivalDateTime"]],
    .data[["S4ArrivalDateTime"]])
)

#' Record-level data fields built from other data fields
#'
#' \code{ssnap_field} is a set of named expressions (code snippets) 
#' used to describe calculations to perform on individual rows to
#' produce new patient-level information.
#'
#' Example uses of these fields are to calculate time differences,
#' or to perform complex row calculations such as eligibility for
#' thrombolysis.
#' 
#' We aren't actually creating these extra data rows - instead we
#' insert the code inline into the formulae for performing the
#' summarising: the expressions in the ssnap_measures lists insert
#' these snippets to perform the calculation.
#' 
#' Writing the code in this way allows us to test these code segments
#' individually in the unit tests, and execute them in the same pass
#' as aggregation. If the goal is for us to send a query to the SSNAP
#' database, we don't want to need to receive patient-level detail.
#' 
#' Although the primary intended user is SSNAPStats, they are
#' exported because we may want to apply these in custom
#' calculations outside of SSNAPStats.
#' 
#' When examining the internals of this function, we can't reference
#' another ssnap_field from within the same list as they are all
#' created in parallel, so if you want to use a ssnap_field from
#' within ssnap_field, you must put the first one outside it and
#' then splice it in - see inset_to_clock_start as an example of
#' that.
#' @export

# * -----------------------------------------------------------------
# * ssnap_field -----------------------------------------------------
# * -----------------------------------------------------------------

ssnap_field <- rlang::exprs(
  "n72hrs" = length(.data[["TeamCode"]]),

  "OnsetToClockStartMins" = onset_to_clock_start,

  # Comorbidities ===================================================

  "Comorbidities" =
    (.data[["S2CoMCongestiveHeartFailure"]] * 1) +
    (.data[["S2CoMHypertension"]] * 1) +
    (.data[["S2CoMAtrialFibrillation"]] * 1) +
    (.data[["S2CoMDiabetes"]] * 1) +
    (.data[["S2CoMStrokeTIA"]] * 1),

  # NihssComponentsComplete =========================================

  "NihssComponentsComplete" =
    ( (.data[["S2NihssArrivalLoc"]] != -1) * 1) +
    ( (.data[["S2NihssArrivalLocQuestions"]] != -1) * 1) +
    ( (.data[["S2NihssArrivalLocCommands"]] != -1) * 1) +
    ( (.data[["S2NihssArrivalBestGaze"]] != -1) * 1) +
    ( (.data[["S2NihssArrivalVisual"]] != -1) * 1) +
    ( (.data[["S2NihssArrivalFacialPalsy"]] != -1) * 1) +
    ( (.data[["S2NihssArrivalMotorArmLeft"]] != -1) * 1) +
    ( (.data[["S2NihssArrivalMotorArmRight"]] != -1) * 1) +
    ( (.data[["S2NihssArrivalMotorLegLeft"]] != -1) * 1) +
    ( (.data[["S2NihssArrivalMotorLegRight"]] != -1) * 1) +
    ( (.data[["S2NihssArrivalLimbAtaxia"]] != -1) * 1) +
    ( (.data[["S2NihssArrivalSensory"]] != -1) * 1) +
    ( (.data[["S2NihssArrivalBestLanguage"]] != -1) * 1) +
    ( (.data[["S2NihssArrivalDysarthria"]] != -1) * 1) +
    ( (.data[["S2NihssArrivalExtinctionInattention"]] != -1) * 1),

  # ClockStartToPalliativeCare72Days ================================

  "ClockStartToPalliativeCare72Days" = internal_time_difference(
    .data[["S1PatientClockStartDateTime"]],
    .data[["S3PalliativeCareDecisionDate"]], "days"),

  # OnsetToArrivalTimeMins ==========================================

  "OnsetToArrivalTimeMins" =
    dplyr::if_else(
      .data[["S1OnsetInHospital"]] |
        is.na(.data[["S1OnsetDateIsPrecise"]]) |
        is.na(.data[["S1OnsetTimeIsPrecise"]]),
      NA_integer_,
      internal_time_difference(
        .data[["S1OnsetDateTime"]],
        .data[["S1PatientClockStartDateTime"]])
    ),

  # OnsetToFirstStrokeUnitMins ======================================

  "OnsetToFirstStrokeUnitMins" = internal_time_difference(
    .data[["S1OnsetDateTime"]],
    .data[["S1FirstStrokeUnitArrivalDateTime"]]),

  # OnsetToBrainImagingMins =========================================

  "OnsetToBrainImagingMins" = internal_time_difference(
    .data[["S1OnsetDateTime"]],
    .data[["S2BrainImagingDateTime"]]),

  # OnsetToThrombolysisMins =========================================

  "OnsetToThrombolysisMins" = internal_time_difference(
    .data[["S1OnsetDateTime"]],
    .data[["S2ThrombolysisDateTime"]]),

  # ClockStartToBrainImagingMins ====================================

  "ClockStartToBrainImagingMins" = internal_time_difference(
    .data[["S1PatientClockStartDateTime"]],
    .data[["S2BrainImagingDateTime"]]),

  # ClockStartToFirstStrokeUnitMins =================================

  "ClockStartToFirstStrokeUnitMins" = internal_time_difference(
    .data[["S1PatientClockStartDateTime"]],
    .data[["S1FirstStrokeUnitArrivalDateTime"]]),

  # ClockStartToThrombolysisMins ====================================

  "ClockStartToThrombolysisMins" = internal_time_difference(
    .data[["S1PatientClockStartDateTime"]],
    .data[["S2ThrombolysisDateTime"]]),

  # ThrombolysisNoButConsistent =====================================

  "ThrombolysisNoButConsistent" =
    bitwAnd(.data[["S2ThrombolysisNoBut"]],
      (ssnapinterface::tpa_no_but["Haemorrhagic"] +
        ssnapinterface::tpa_no_but["Improving"] +
        ssnapinterface::tpa_no_but["Comorbidity"] +
        ssnapinterface::tpa_no_but["Medication"] +
        ssnapinterface::tpa_no_but["Refusal"] +
        ssnapinterface::tpa_no_but["OtherMedical"])) +

  # Check No But Age
  # If age is inconsistent, reset Bit 7 to 0.
    dplyr::case_when( (bitwAnd(.data[["S2ThrombolysisNoBut"]],
      ssnapinterface::tpa_no_but["Age"]) == 0) ~ 0,
      .data[["S1AgeOnArrival"]] < 80 ~ 0,
      (.data[["S1AgeOnArrival"]] >= 80) &
        .data[["S1OnsetTimeIsPrecise"]] &
        (!! onset_to_clock_start < 120) ~ 0,
      TRUE ~ ssnapinterface::tpa_no_but["Age"]) +

  # Check No But Too Mild
  # If Severity is inconsistent, reset Bit 8 to 0.
    dplyr::case_when( (bitwAnd(.data[["S2ThrombolysisNoBut"]],
      ssnapinterface::tpa_no_but["TooMildSevere"]) == 0) ~ 0,
      .data[["S2NihssArrival"]] >= 4 ~ 0,
      TRUE ~ ssnapinterface::tpa_no_but["TooMildSevere"]) +

  # Check the No But for Time Unknown / wake up
  # If Time Unknown / Wake Up is inconsistent, reset Bit 9 to 0.
    dplyr::case_when( (bitwAnd(.data[["S2ThrombolysisNoBut"]],
      ssnapinterface::tpa_no_but["TimeUnknownWakeUp"]) == 0) ~ 0,
      .data[["S1OnsetTimeIsPrecise"]] ~ 0,
      TRUE ~ ssnapinterface::tpa_no_but["TimeUnknownWakeUp"]) +

      # Check the No But for Time window
      # If Time window is inconsistent, reset Bit 10 to 0.
    dplyr::case_when( (bitwAnd(.data[["S2ThrombolysisNoBut"]],
      ssnapinterface::tpa_no_but["TimeWindow"]) == 0) ~ 0,
      (.data[["S1AgeOnArrival"]] < 80) &
        .data[["S1OnsetTimeIsPrecise"]] &
        (!! onset_to_clock_start < 210) ~ 0,
      (.data[["S1AgeOnArrival"]] >= 80) &
        .data[["S1OnsetTimeIsPrecise"]] &
        (!! onset_to_clock_start < 120) ~ 0,
      .data[["S1OnsetInHospital"]] &
        !is.na(.data[["S1OnsetDateIsPrecise"]]) ~ 0,
      TRUE ~ ssnapinterface::tpa_no_but["TimeWindow"]),

  # EligibleFortPAByRCPCriteria =====================================

  "EligibleFortPAByRCPCriteria" =
    # All hospital inpatients are eligible
    .data[["S1OnsetInHospital"]] |
      # Or outpatients with a clock start under 2hrs (3.5 if <80)
      # And clock start time is either precise or a best estimate.
      (!is.na(!! onset_to_clock_start) &
        ( (!! onset_to_clock_start < 120) |
          ( (.data[["S1AgeOnArrival"]] < 80) &
            (!! onset_to_clock_start < 210))
          )
       ),

  # ClockStartToConsultantMins ======================================

  "ClockStartToConsultantMins" = internal_time_difference(
    .data[["S1PatientClockStartDateTime"]],
    .data[["S3StrokeConsultantAssessedDateTime"]]),

  # ClockStartToStrokeNurseMins =====================================

  "ClockStartToStrokeNurseMins" = internal_time_difference(
    .data[["S1PatientClockStartDateTime"]],
    .data[["S3StrokeNurseAssessedDateTime"]]),

  # ClockStartToSwallowScreen4hrsMins ===============================

  "ClockStartToSwallowScreen4hrsMins" = internal_time_difference(
    .data[["S1PatientClockStartDateTime"]],
    .data[["S2SwallowScreening4HrsDateTime"]]),

  # ClockStartToSwallowScreen4hrsMins ===============================

  "ClockStartToSwallowScreen72hrsMins" =
    dplyr::if_else(
      (.data[["S2SwallowScreening4HrsDateTime"]] > 0),
      internal_time_difference(
        .data[["S1PatientClockStartDateTime"]],
        .data[["S2SwallowScreening4HrsDateTime"]]),
      internal_time_difference(
        .data[["S1PatientClockStartDateTime"]],
        .data[["S3SwallowScreening72HrsDateTime"]])),

  # ClockStartToSpLangTherapistSwallowMins ==========================

  "ClockStartToSpLangTherapistSwallowMins" =
    internal_time_difference(
      .data[["S1PatientClockStartDateTime"]],
      .data[["S3SpLangTherapistSwallow72HrsDateTime"]]),

  # ClockStartToOTMins ==============================================

  "ClockStartToOTMins" = internal_time_difference(
    .data[["S1PatientClockStartDateTime"]],
    .data[["S3OccTherapist72HrsDateTime"]]),

  # ClockStartToPTMins ==============================================

  "ClockStartToPTMins" = internal_time_difference(
    .data[["S1PatientClockStartDateTime"]],
    .data[["S3Physio72HrsDateTime"]]),

  # ClockStartToSLTCommMins =========================================

  "ClockStartToSLTCommMins" = internal_time_difference(
    .data[["S1PatientClockStartDateTime"]],
    .data[["S3SpLangTherapistComm72HrsDateTime"]]),

  # ClockStartToRecordCreationHours =================================

  "ClockStartToRecordCreationHours" = internal_time_difference(
    .data[["S1PatientClockStartDateTime"]],
    .data[["CreatedDateTime"]], "hours"),

  # ClockStartToRehabGoalsHours =====================================

  "ClockStartToRehabGoalsDays" = internal_time_difference(
    .data[["S1PatientClockStartDateTime"]],
    .data[["S4RehabGoalsDate"]], "days"),

  # BrainImagingToThrombolysisMins ==================================

  "BrainImagingToThrombolysisMins" = internal_time_difference(
    .data[["S2BrainImagingDateTime"]],
    .data[["S2ThrombolysisDateTime"]], "mins"),

  # DaysOnsetToArrival ==============================================

  "DaysOnsetToArrival" = as.integer(difftime(
    lubridate::ceiling_date(.data[["S1FirstArrivalDateTime"]], "day"),
    lubridate::ceiling_date(.data[["S1OnsetDateTime"]], "day"),
    units = "days")),

  # OnsetToIAIPunctureMins ==========================================

  "OnsetToIAIPunctureMins" = internal_time_difference(
    .data[["S1OnsetDateTime"]],
    .data[["S2IAIArterialPunctureDateTime"]], "mins"),

  # OnsetToIAICompletionMins ========================================

  "OnsetToIAICompletionMins" = internal_time_difference(
    .data[["S1OnsetDateTime"]],
    .data[["S2IAIEndOfProcedureDateTime"]], "mins"),

  # ClockStartToIAIPunctureMins =====================================

  "ClockStartToIAIPunctureMins" = internal_time_difference(
    .data[["S1PatientClockStartDateTime"]],
    .data[["S2IAIArterialPunctureDateTime"]], "mins"),

  # PunctureToDeploymentMins ========================================

  "PunctureToDeploymentMins" = internal_time_difference(
    .data[["S2IAIArterialPunctureDateTime"]],
    .data[["S2IAIThrombectomyAspirationDeviceDateTime"]], "mins"),

  # PunctureToIAICompletionMins =====================================

  "PunctureToIAICompletionMins" = internal_time_difference(
    .data[["S2IAIArterialPunctureDateTime"]],
    .data[["S2IAIEndOfProcedureDateTime"]], "mins"),

  # TeamClockStart ==================================================

  "TeamClockStart" = team_clockstart,

  # Calculate length of stay at teamfor KI 2.3 (90% stay)
  "LengthOfStayAtTeamMins" = length_of_stay_at_team_mins,

  # LengthOfFirstInptStayHrs ========================================

  "LengthOfFirstInptStayHrs" = internal_time_difference(
    .data[["S1PatientClockStartDateTime"]],
    .data[["S7FirstInptEpisodeClockStopDateTime"]], "hours"),

  "S7LengthOfStayOnStrokeUnitTeamMins" =
    length_of_stay_on_stroke_unit,

  # StayPercent =====================================================

  "StayPercent" = dplyr::case_when(
      # Pass anyone with LOS less than 4hrs as length of stay will be
      # negative with the calculation
      !! length_of_stay_at_team_mins <= 240 ~ 100,

      # Exclude anyone who dies on the day of arrival
#      as.Date(.data[["S7DeathDate"]]) ==
#        as.Date(.data[["S4ArrivalDateTime"]]) ~ NA_real_,

      # Exclude any CCU / HDU / ITU stays
#      .data[["S4FirstWard"]] == "ICH" ~ NA_real_,

      # If you didn't stay on a stroke unit, and have not been
      # excluded above, then the stroke unit stay is 0 percent
      is.na(.data[["S4StrokeUnitArrivalDateTime"]]) ~ 0,

      # For the rest use length of stay on stroke team divided by the
      # Total length of stay excluding 4 hours (spent in ED).
      TRUE ~ pmin(!! length_of_stay_on_stroke_unit /
        (!! length_of_stay_at_team_mins - 240) * 100, 100)
    ),

  "TeamPhysioNeedMins" = team_physio_need_mins,

  "TeamOccTherNeedMins" = team_occther_need_mins,

  "TeamSpeechLangNeedMins" = team_speechlang_need_mins,

  # PCPhysioDays ====================================================

  "PCPhysioDays" = dplyr::case_when(
    !.data[["S4Physio"]] ~ NA_real_,
    (.data[["S4PhysioDays"]] == 0) ~ 0,
    (!! team_physio_need_mins < 1440) ~ 100,
    TRUE ~ (.data[["S4PhysioDays"]] * 24 * 60) /
      !! team_physio_need_mins * 100),

  # PCOccTherDays ===================================================

  "PCOccTherDays" = dplyr::case_when(
    !.data[["S4OccTher"]] ~ NA_real_,
    (.data[["S4OccTherDays"]] == 0) ~ 0,
    (!! team_occther_need_mins < 1440) ~ 100,
    TRUE ~ (.data[["S4OccTherDays"]] * 24 * 60) /
      !! team_occther_need_mins * 100),

  # PCSpeechLangDays ================================================

  "PCSpeechLangDays" = dplyr::case_when(
    !.data[["S4SpeechLang"]] ~ NA_real_,
    (.data[["S4SpeechLangDays"]] == 0) ~ 0,
    (!! team_speechlang_need_mins < 1440) ~ 100,
    TRUE ~ (.data[["S4SpeechLangDays"]] * 24 * 60) /
      !! team_speechlang_need_mins * 100),

  # PCPsychologyDays ================================================

  "PCPsychologyDays" = dplyr::case_when(
    !.data[["S4Psychology"]] ~ NA_real_,
    (.data[["S4PsychologyDays"]] == 0) ~ 0,
    (!! team_psychology_need_mins < 1440) ~ 100,
    TRUE ~ (.data[["S4PsychologyDays"]] * 24 * 60) /
      !! team_psychology_need_mins * 100),

  # PhysioMinutesPerDay =============================================

  "PhysioMinutesPerDay" = dplyr::if_else(
    .data[["S4PhysioDays"]] > 0,
    .data[["S4PhysioMinutes"]] / .data[["S4PhysioDays"]],
    0),

  # OccTherMinutesPerDay ============================================

  "OccTherMinutesPerDay" = dplyr::if_else(
    .data[["S4OccTherDays"]] > 0,
    .data[["S4OccTherMinutes"]] / .data[["S4OccTherDays"]],
    0),

  # SpeechLangMinutesPerDay =========================================

  "SpeechLangMinutesPerDay" = dplyr::if_else(
    .data[["S4SpeechLangDays"]] > 0,
    .data[["S4SpeechLangMinutes"]] / .data[["S4SpeechLangDays"]],
    0),

  # PhysioMinutesPerDay =============================================

  "PsychologyMinutesPerDay" = dplyr::if_else(
    .data[["S4PsychologyDays"]] > 0,
    .data[["S4PsychologyMinutes"]] / .data[["S4PsychologyDays"]],
    0),

  # ClockStartToContinencePlanHours =================================

  "ClockStartToContinencePlanHours" = internal_time_difference(
    .data[["S1PatientClockStartDateTime"]],
    .data[["S6UrinaryContinencePlanDate"]], "hours"),

  # ClockStartToMoodScreenHours =====================================

  "ClockStartToMoodScreenHours" = internal_time_difference(
    .data[["S1PatientClockStartDateTime"]],
    .data[["S6MoodScreeningDate"]], "hours"),

  # ClockStartToCognitionScreenHours ================================

  "ClockStartToCognitionScreenHours" = internal_time_difference(
    .data[["S1PatientClockStartDateTime"]],
    .data[["S6CognitionScreeningDate"]], "hours"),

  # ClockStartTo6MonthAssessmentMonths ==============================

  "ClockStartTo6MonthAssessmentMonths" = internal_time_difference(
    .data[["S1PatientClockStartDateTime"]],
    .data[["S8FollowUpDate"]], "months"),


  # DischargeTo6MonthAssessmentMonths ===============================

  "DischargeTo6MonthAssessmentMonths" = internal_time_difference(
    .data[["S7HospitalDischargeDateTime"]],
    .data[["S8FollowUpDate"]], "months")

)
