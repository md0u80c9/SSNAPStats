#' @importFrom rlang !!!
#' @importFrom rlang .data
#' @title internal_calculated_field_functions_discharge
#'
#' @description
#' \code{internal_d1ki_calculated_field_functions} is a list of
#' quosures used to create extra data columns to imported SSNAP data
#' to support domain 1 (scanning) aggregation calculations.
#'
#' @details
#' Calculated fields are returned as a list of functions so that a
#' single dplyr mutate operation can undertake all the calculated
#' fields in a single go.
#' 
#' It adds boolean fields for whether the scan was performed within 1
#' hour and 12 hours, and duplicates the scan time as a Key Indicator
#' value.
#'
#' @author Andrew Hill, \email{andrew.hill@@doctors.org.uk}

# duplications in various calculated_field_functions

deprecated_time_difference <- function(start_field,
                                       end_field,
                                       new_field_name,
                                       units) {
  
  start_field <- rlang::enquo(start_field)
  end_field <- rlang::enquo(end_field)
  
  rlang::quos(!!new_field_name := dplyr::if_else(
    !is.na(!!start_field) &
      !is.na(!!end_field) &
      ( (!!start_field) > 0) &
      ( (!!end_field) > 0),
    as.integer(difftime(!!end_field,
                        !!start_field,
                        units = units)),
    NA_integer_)
  )
}

apply_measure_proportion <- function(measure,
                                     item = "",
                                     pct_only = FALSE) {
  d_name <- glue::glue("{measure}D")
  n_name <- glue::glue("{measure}{item}N")
  
  dn_outputs <- NULL
  # By default we want to return the denominator and numerator
  if (!pct_only) {
    dn_outputs <- rlang::quos(
      !!d_name := !! ssnap_measure[[d_name]],
      !!n_name := !! ssnap_measure[[n_name]]
    )
  }
  outputs_pct <- glue::glue("{measure}{item}Pct")
  c(dn_outputs,
    rlang::quos(
      !!outputs_pct := round(
        sum(!! ssnap_measures[[n_name]], na.rm = TRUE) /
          !! ssnap_measures[[d_name]] * 100, 1)
    )
  )
}

apply_pct <- function(measure, item = "") {
  apply_measure_proportion(measure = measure,
                           item = item,
                           pct_only = TRUE)
}

internal_acdischarge_calculated_field_functions <- rlang::quos(
  # Record locked to discharge uses LockedS7DateTime as the proxy for
  # complete record locking.
  !!! deprecated_time_difference(
    .data[["S7TeamClockStopDateTime"]],
    .data[["LockedS7DateTime"]],
    "ACClockStopToRecordLockHours",
    "hours"),
  !!! deprecated_time_difference(
    .data[["S7TeamClockStopDateTime"]],
    .data[["TransferToActionDateTime"]],
    "ACClockStopToRecordTransferHours",
    "hours"),
  
  ACESDCRTTransfer = dplyr::if_else(
    (!is.na(.data[["S7DischargedSpecialistEsdmt"]]) &
       !is.na(.data[["S7DischargedSpecialistMcrt"]])),
    stringr::str_detect(.data[["S7TransferTeamCode"]], "C"),
    NA)
)

internal_d2discharge_calculated_field_functions <- rlang::quos(
  # Calculate length of stay at teamfor KI 2.3 (90% stay)
  !!! deprecated_time_difference(
    .data[["S1TeamClockStartDateTime"]],
    .data[["S7TeamClockStopDateTime"]],
    "S7LengthOfStayAtTeamMins",
    "mins"),
  
  !!! deprecated_time_difference(
    .data[["S4StrokeUnitArrivalDateTime"]],
    .data[["S7StrokeUnitDischargeDateTime"]],
    "S7LengthOfStayOnStrokeUnitTeamMins",
    "mins"),
  
  S7StayPercent = dplyr::case_when(
    # Pass anyone with LOS less than 4hrs as length of stay will be
    # negative with the calculation
    .data[["S7LengthOfStayAtTeamMins"]] <= 240 ~ 100,
    
    # Exclude anyone who dies on the day of arrival
    as.Date(.data[["S7DeathDate"]]) ==
      as.Date(.data[["S4ArrivalDateTime"]]) ~ NA_real_,
    
    # Exclude any CCU / HDU / ITU stays
    .data[["S4FirstWard"]] == "ICH" ~ NA_real_,
    
    # If you didn't stay on a stroke unit, and have not been excluded
    # above, then the stroke unit stay is 0 percent
    is.na(.data[["S4StrokeUnitArrivalDateTime"]]) ~ 0,
    
    # For the rest use length of stay on stroke team divided by the
    # Total length of stay excluding 4 hours (spent in ED).
    TRUE ~ pmin(.data[["S7LengthOfStayOnStrokeUnitTeamMins"]] /
                  (.data[["S7LengthOfStayAtTeamMins"]] - 240) * 100, 100)
  ),
  
  TCKIStayOver90PCInStrokeUnit = (.data[["S7StayPercent"]] >= 90)
)

internal_d9discharge_calculated_field_functions <- rlang::quos(
  TCKIContinenceAssessed = dplyr::case_when(
    # Exclude anyone who is continent or refuses
    .data[["S6UrinaryContinencePlanNoPlanReason"]] %in%
      c("PC", "PR") ~ NA,
    TRUE ~ (as.Date(.data[["S6UrinaryContinencePlanDate"]]) <
              (as.Date(.data[["S1PatientClockStartDateTime"]]) + 21))
  ),
  TCKICognitionAndMoodAssessed = dplyr::case_when(
    # Exclude anyone who refuses or is unwell (mood)
    .data[["S6MoodScreeningNoScreeningReason"]] %in%
      c("MU", "PR") ~ NA,
    
    # Exclude anyone who refuses or is unwell (cognition)
    .data[["S6CognitionScreeningNoScreeningReason"]] %in%
      c("MU", "PR") ~ NA,
    
    # Exclude anyone whose total length of stay is < 7 days AND
    # had one or both screens omitted
    ( (.data[["S7TeamClockStopDateTime"]] <
         (as.Date(.data[["S1PatientClockStartDateTime"]]) + 7)) &
        is.na(.data[["S6MoodScreeningDate"]]) &
        is.na(.data[["S6CognitionScreeningDate"]])) ~ NA,
    
    # If we're not excluded then check mood and cognition done within
    # six weeks of initial clock start
    TRUE ~ ( (.data[["S6MoodScreeningDate"]] <
                (as.Date(.data[["S1PatientClockStartDateTime"]]) + 42)) &
               (.data[["S6CognitionScreeningDate"]] <
                  (as.Date(.data[["S1PatientClockStartDateTime"]]) + 42)))
  )
)

internal_d10discharge_calculated_field_functions <- rlang::quos(
  TCKIJointCarePlanning = dplyr::case_when(
    # Exclude patients who die or whose last known records indicates
    # transfer to another inpatient team.
    .data[["S7DischargeType"]] %in% c("D", "T") ~ NA,
    
    # Exclude any where joint care planning deemed not applicable.
    is.na(.data[["S7DischargeJointCarePlanning"]]) ~ NA,
    
    TRUE ~ .data[["S7DischargeJointCarePlanning"]]),
  
  TCKIDischargedWithESD = dplyr::case_when(
    # Exclude patients who die or whose last known records indicates
    # transfer to another inpatient team.
    .data[["S7DischargeType"]] %in% c("D", "T") ~ NA,
    
    # If both ESD and MCR are NA then we know no ESD discharge
    # Occurred.
    (is.na(.data[["S7DischargedSpecialistEsdmt"]]) &
       is.na(.data[["S7DischargedSpecialistMcrt"]])) ~ FALSE,
    
    TRUE ~ (.data[["S7DischargedSpecialistEsdmt"]] |
              .data[["S7DischargedSpecialistMcrt"]])),
  
  TCKIAFWithAnticoagulation = dplyr::case_when(
    # Exclude patients who die or whose last known records indicates
    # transfer to another inpatient team.
    .data[["S7DischargeType"]] %in% c("D", "T") ~ NA,
    
    # Exclude patients who are not in atrial fibrillation
    !.data[["S7DischargeAtrialFibrillation"]] ~ NA,
    
    # Patients with "no but" for anticoagulation are excluded
    is.na(.data[["S7DischargeAtrialFibrillationAnticoagulation"]])
    ~ NA,
    
    TRUE ~ .data[["S7DischargeAtrialFibrillationAnticoagulation"]]),
  
  TCKINamedContact = dplyr::case_when(
    # Exclude patients who die or whose last known records indicates
    # transfer to another inpatient team.
    .data[["S7DischargeType"]] %in% c("D", "T") ~ NA,
    
    TRUE ~ .data[["S7DischargeNamedContact"]])
)

internal_teamDischargeCohortKIs_ki_fields <- c(
  "S7TeamClockStopDateTime",
  "LockedS7DateTime",
  "TransferToActionDateTime",
  "S7DischargedSpecialistEsdmt",
  "S7DischargedSpecialistMcrt",
  "S7TransferTeamCode",
  "S1TeamClockStartDateTime",
  "S4StrokeUnitArrivalDateTime",
  "S7StrokeUnitDischargeDateTime",
  "S4ArrivalDateTime",
  "S4FirstWard",
  "S6UrinaryContinencePlanNoPlanReason",
  "S6UrinaryContinencePlanDate",
  "S6MoodScreeningNoScreeningReason",
  "S6MoodScreeningDate",
  "S6CognitionScreeningNoScreeningReason",
  "S6CognitionScreeningDate",
  "S7DischargeType",
  "S7DischargeJointCarePlanning",
  "S7DischargeAtrialFibrillation",
  "S7DischargeAtrialFibrillationAnticoagulation",
  "S7DischargeNamedContact",
  "S7DeathDate"
)

internal_calculated_field_functions_discharge <- c(
  internal_d2discharge_calculated_field_functions,
  internal_d9discharge_calculated_field_functions,
  internal_d10discharge_calculated_field_functions,
  internal_acdischarge_calculated_field_functions
)

internal_aggr_value_functions_discharge <- function(cohort_type) {
  rlang::quos(
    !!! internal_d2_aggr_value_functions_discharge(cohort_type),
    !!! internal_d9_aggr_value_functions_discharge(cohort_type),
    !!! internal_d10_aggr_value_functions_discharge(cohort_type)
  )
}

internal_teamDischargeAggrKIs_ac_fields <- c(
  "S6OccTherapistByDischargeNotAssessedReason",
  "S6PhysioByDischargeNotAssessedReason",
  "S6SpLangTherapistCommByDischargeNotAssessedReason",
  "S6SpLangTherapistSwallowByDischargeNotAssessedReason",
  "S6CognitionScreeningNoScreeningReason",
  "S6MoodScreeningNoScreeningReason",
  "S7HomeDischargeType",
  "S7DischargeVisitsPerWeekNK"
)

internal_calculate_ac_aggr_values_discharge <- function() {
  rlang::quos(
    ACMedianRecordLockTimeHrs =
      stats::median(.data[["ACClockStopToRecordLockHours"]],
                    na.rm = TRUE),
    ACMedianRecordTransferTimeHrs =
      stats::median(.data[["ACClockStopToRecordTransferHours"]],
                    na.rm = TRUE),
    ACPCOccTherapistByDischargeComplete = 100 - (sum(
      .data[["S6OccTherapistByDischargeNotAssessedReason"]] == "NK",
      na.rm = TRUE) / length(.data[["TeamCode"]]) * 100),
    ACPCPhysioByDischargeComplete = 100 - (
      sum(.data[["S6PhysioByDischargeNotAssessedReason"]] == "NK",
          na.rm = TRUE) / length(.data[["TeamCode"]]) * 100),
    ACPCSpLangTherapistCommByDischargeComplete = 100 - (sum(
      .data[["S6SpLangTherapistCommByDischargeNotAssessedReason"]] ==
        "NK", na.rm = TRUE) / length(.data[["TeamCode"]]) * 100),
    ACPCSpLangTherapistSwallowByDischargeComplete = 100 - (sum(
      .data[["S6SpLangTherapistSwallowByDischargeNotAssessedReason"]]
      == "NK", na.rm = TRUE) / length(.data[["TeamCode"]]) * 100),
    ACPCMoodScreeningComplete = 100 - (
      sum(.data[["S6MoodScreeningNoScreeningReason"]]
          == "NK", na.rm = TRUE) / length(.data[["TeamCode"]]) * 100),
    ACPCCognitionScreeningComplete = 100 - (
      sum(.data[["S6CognitionScreeningNoScreeningReason"]]
          == "NK", na.rm = TRUE) / length(.data[["TeamCode"]]) * 100),
    ACPCHomeDischargeLivingAloneComplete = 100 - (
      sum(.data[["S7HomeDischargeType"]]
          == "NK", na.rm = TRUE) / length(.data[["TeamCode"]]) * 100),
    ACPCDischargeVisitsComplete = 100 - (
      sum(.data[["S7DischargeVisitsPerWeekNK"]], na.rm = TRUE) /
        length(.data[["TeamCode"]]) * 100)
  )
}

internal_d2_aggr_value_functions_discharge <- function(cohort_type) {
  rlang::quos(
    !!kisym(cohort_type, "KIPCStayOver90PCInStrokeUnit") :=
      deprecated_na_percentage(!!kisym(cohort_type,
                                       "KIStayOver90PCInStrokeUnit"))
  )
}

internal_d9_aggr_value_functions_discharge <- function(cohort_type) {
  rlang::quos(
    !!kisym(cohort_type, "KIPCContinenceAssessed") :=
      deprecated_na_percentage(!!kisym(cohort_type,
                                       "KIContinenceAssessed")),
    !!kisym(cohort_type, "KIPCCognitionAndMoodAssessed") :=
      deprecated_na_percentage(!!kisym(cohort_type,
                                       "KICognitionAndMoodAssessed"))
  )
}

internal_d10_aggr_value_functions_discharge <- function(cohort_type) {
  rlang::quos(
    !!kisym(cohort_type, "KIPCJointCarePlanning") :=
      deprecated_na_percentage(!!kisym(cohort_type,
                                       "KIJointCarePlanning")),
    !!kisym(cohort_type, "KIPCDischargedWithESD") :=
      deprecated_na_percentage(!!kisym(cohort_type,
                                       "KIDischargedWithESD")),
    !!kisym(cohort_type, "KIPCAFWithAnticoagulation") :=
      deprecated_na_percentage(!!kisym(cohort_type,
                                       "KIAFWithAnticoagulation")),
    !!kisym(cohort_type, "KIPCNamedContact") :=
      deprecated_na_percentage(!!kisym(cohort_type,
                                       "KINamedContact"))
  )
}

internal_d567ki_calculated_field_functions <- rlang::quos(
  !!! deprecated_time_difference(
    .data[["S1TeamClockStartDateTime"]],
    .data[["S4PhysioEndDate"]],
    "S4TeamPhysioNeedMins",
    "mins"),
  #  S4TeamPhysioNeedHours = max(24, S4TeamPhysioNeedHours),
  
  !!! deprecated_time_difference(
    .data[["S1TeamClockStartDateTime"]],
    .data[["S4OccTherEndDate"]],
    "S4TeamOccTherNeedMins",
    "mins"),
  #  S4TeamOccTherNeedHours = max(24, S4TeamOccTherNeedHours),
  
  !!! deprecated_time_difference(
    .data[["S1TeamClockStartDateTime"]],
    .data[["S4SpeechLangEndDate"]],
    "S4TeamSpeechLangNeedMins",
    "mins"),
  #  S4TeamSpeechLangNeedHours = max(24, S4TeamSpeechLangNeedHours),
  
  TCKIPCPhysioDays = dplyr::case_when(
    !.data[["S4Physio"]] ~ NA_real_,
    (.data[["S4PhysioDays"]] == 0) ~ 0,
    (.data[["S4TeamPhysioNeedMins"]] < 1440) ~ 100,
    TRUE ~ (.data[["S4PhysioDays"]] * 24 * 60) /
      .data[["S4TeamPhysioNeedMins"]] * 100),
  TCKIPCOccTherDays = dplyr::case_when(
    !.data[["S4OccTher"]] ~ NA_real_,
    (.data[["S4OccTherDays"]] == 0) ~ 0,
    (.data[["S4TeamOccTherNeedMins"]] < 1440) ~ 100,
    TRUE ~ (.data[["S4OccTherDays"]] * 24 * 60) /
      .data[["S4TeamOccTherNeedMins"]] * 100),
  TCKIPCSpeechLangDays = dplyr::case_when(
    !.data[["S4SpeechLang"]] ~ NA_real_,
    (.data[["S4SpeechLangDays"]] == 0) ~ 0,
    (.data[["S4TeamSpeechLangNeedMins"]] < 1440) ~ 100,
    TRUE ~ (.data[["S4SpeechLangDays"]] * 24 * 60) /
      .data[["S4TeamSpeechLangNeedMins"]] * 100),
  TCKIPhysio = .data[["S4Physio"]],
  TCKIOccTher = .data[["S4OccTher"]],
  TCKISpeechLang = .data[["S4SpeechLang"]],
  TCKIPhysioMinutesPerDay = dplyr::if_else(
    .data[["S4PhysioDays"]] > 0,
    .data[["S4PhysioMinutes"]] / .data[["S4PhysioDays"]],
    0),
  TCKIOccTherMinutesPerDay = dplyr::if_else(
    .data[["S4OccTherDays"]] > 0,
    .data[["S4OccTherMinutes"]] / .data[["S4OccTherDays"]],
    0),
  TCKISpeechLangMinutesPerDay = dplyr::if_else(
    .data[["S4SpeechLangDays"]] > 0,
    .data[["S4SpeechLangMinutes"]] / .data[["S4SpeechLangDays"]],
    0)
)

internal_teamPost72hrAllTeamsCohortKIs_ki_fields <- c(
  "S7TeamClockStopDateTime",
  "S1TeamClockStartDateTime",
  "S4PhysioEndDate",
  "S4OccTherEndDate",
  "S4SpeechLangEndDate",
  "S4PhysioDays",
  "S4OccTherDays",
  "S4SpeechLangDays",
  "S4Physio",
  "S4OccTher",
  "S4SpeechLang",
  "S4PhysioMinutes",
  "S4OccTherMinutes",
  "S4SpeechLangMinutes"
)

internal_calculated_field_functions_post72hrs <- c(
  internal_d567ki_calculated_field_functions
)

internal_cohorts_kis_discharge <- function(cohort_type) {
  n <- kisym(cohort_type, "nDischarge")
  
  rlang::quos(
#    !!n := !! ssnap_field[["nDischarge"]],
    
#    !!! apply_pct("StayOver90PCInStrokeUnit"),
    
#    !!! apply_pct("ContinenceAssessed"),
#    !!! apply_pct("CognitionAndMoodAssessed"),
    
#    !!! apply_pct("JointCarePlanning"),
#    !!! apply_pct("DischargedWithESD"),
#    !!! apply_pct("AFWithAnticoagulation"),
#    !!! apply_pct("DischargeNamedContact")
  )
}

internal_cohorts_kis_post72hrs <- function(cohort_type) {
  n <- kisym(cohort_type, "nPost72hrs")
  
  rlang::quos(
    !!n := !! ssnap_field[["nPost72hrs"]],
    
    !!! apply_pct("Physio"),
    
    !!! apply_pct("OccTher"),
    !!! apply_pct("SpeechLang")
  )
}
