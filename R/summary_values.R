#' Summary Values For Therapy Compliance
#' 
#' The team post-72hr cohort (Key Indicators in Domains 5, 6 and 7)
#' use a set of derived summary statistics to calculate the
#' percentage of compliance with therapy.
#' 

# * summary_values_therapy_compliance --------------------------------
#' @export

summary_values_therapy_compliance <- rlang::exprs(
  PhysioCompliance = (.data[["PhysioPct"]] / 100) *
    .data[["PhysioMinutesQ2"]] *
    (.data[["PCPhysioDaysQ2"]] / 100) /
    (0.85 * 45 * 5 / 7) * 100,
  OccTherCompliance = (.data[["OccTherPct"]] / 100) *
    .data[["OccTherMinutesQ2"]] *
    (.data[["PCOccTherDaysQ2"]] / 100) /
    (0.80 * 45 * 5 / 7) * 100,
  SpeechLangCompliance = (.data[["SpeechLangPct"]] / 100) *
    .data[["SpeechLangMinutesQ2"]] *
    (.data[["PCSpeechLangDaysQ2"]] / 100) /
    (0.5 * 45 * 5 / 7) * 100
)

#' Summary Values For SSNAP Scores
#' 
#' These calculations are used to produce scores for individual key
#' indicators and overall domains on SSNAP.
#' 

# * summary_values_ssnap_scores ------------------------------
#' @export

summary_values_ssnap_scores <- function(cohort_type) {
  rlang::exprs(
    !!kisym(cohort_type, "KI1_1") := {
      pmin(100,
          round(!!kisym(cohort_type,
                        "BrainImagingWithin1hrPct") * 2, 1))
    },

    !!kisym(cohort_type, "KI1_2") := {
        pmax(0,
            round( ( (!!kisym(cohort_type,
            "BrainImagingWithin12hrsPct") / 5) - 10) * 10, 1))
    },

    !!kisym(cohort_type, "KI1_3") := {
      10 * (11 - cut(
        !!kisym(cohort_type, "ClockStartToBrainImagingMinsQ2"),
        breaks = c(  0,  45,  60,  75,  90, 120,
                   180, 240, 300, 360, 480, Inf),
        labels = FALSE))
    },

    !!kisym(cohort_type, "D1Score") := {
      round( (!!kisym(cohort_type, "KI1_1") +
          !!kisym(cohort_type, "KI1_2") +
          !!kisym(cohort_type, "KI1_3")) / 3, 1)
    },

    !!kisym(cohort_type, "D1Level") := {
      cut(!!kisym(cohort_type, "D1Score"),
          breaks = c(0, 65, 70, 85, 95, Inf),
          labels = c("E", "D", "C", "B", "A"))
    },

    !!kisym(cohort_type, "KI2_1") :=
      !!kisym(cohort_type, "FirstSUWithin4hrsPct"),

    !!kisym(cohort_type, "KI2_2") := {
      10 * (11 - cut(
        !!kisym(cohort_type, "ClockStartToFirstStrokeUnitMinsQ2"),
        breaks = c(  0,  60, 120, 180, 240, 270,
                   300, 330, 360, 420, 480, Inf),
        labels = FALSE))
    },

# TODO Domain 2.3

    !!kisym(cohort_type, "D2Score") := {
      round( (!!kisym(cohort_type, "KI2_1") +
            !!kisym(cohort_type, "KI2_2")) / 2, 1)
#                !!kisym(cohort_type, "KI2_2"),
#                !!kisym(cohort_type, "KI2_3")) / 3, 1)
    },

    !!kisym(cohort_type, "D2Level") := {
      cut(!!kisym(cohort_type, "D2Score"),
          breaks = c(0, 60, 70, 80, 90, Inf),
          labels = c("E", "D", "C", "B", "A"))
    },

    !!kisym(cohort_type, "KI3_1") := {
      pmin(100,
          round(!!kisym(cohort_type, "ThrombolysisYesPct") * 5, 1))
    },

    !!kisym(cohort_type, "KI3_2") :=
      !!kisym(cohort_type, "tPAIfMeetsRCPCriteriaYesPct"),

    !!kisym(cohort_type, "KI3_3") :=
      !!kisym(cohort_type, "tPAWithin1hrPct"),

    !!kisym(cohort_type, "KI3_4") :=
      !!kisym(cohort_type, "tPAAndSUIn4hrsPct"),

    !!kisym(cohort_type, "KI3_5") := {
      10 * (11 - cut(
        !!kisym(cohort_type, "ClockStartToThrombolysisMinsQ2"),
        breaks = c( 0, 30,  40,  50,  60,  70,
                   80, 90, 100, 110, 120, Inf),
        labels = FALSE))
    },

    !!kisym(cohort_type, "D3Score") := {
      round( (!!kisym(cohort_type, "KI3_1") +
             !!kisym(cohort_type, "KI3_2") +
             !!kisym(cohort_type, "KI3_3") +
             !!kisym(cohort_type, "KI3_4") +
             !!kisym(cohort_type, "KI3_5")) / 5, 1)
    },

    !!kisym(cohort_type, "D3Level") := {
      cut(!!kisym(cohort_type, "D3Score"),
          breaks = c(0, 45, 60, 70, 80, Inf),
          labels = c("E", "D", "C", "B", "A"))
    },

    !!kisym(cohort_type, "KI4_1") :=
      !!kisym(cohort_type, "StrokeConsultantReviewWithin24hrsPct"),

    !!kisym(cohort_type, "KI4_2") := {
      10 * (11 - cut(
        !!kisym(cohort_type, "ClockStartToConsultantMinsQ2"),
        breaks = c(   0,  180,  360,  540,  720, 900,
                   1080, 1260, 1440, 2160, 2880, Inf),
        labels = FALSE))
    },

    !!kisym(cohort_type, "KI4_3") :=
      !!kisym(cohort_type, "StrokeNurseReviewWithin24hrsPct"),

    !!kisym(cohort_type, "KI4_4") := {
      10 * (11 - cut(
        !!kisym(cohort_type, "ClockStartToStrokeNurseMinsQ2"),
        breaks = c(  0,  30,  60,  120,  180, 360,
                   540, 720, 900, 1080, 1260, Inf),
        labels = FALSE))
    },

    !!kisym(cohort_type, "KI4_5") :=
      !!kisym(cohort_type, "SwallowScreenWithin4hrsPct"),

    !!kisym(cohort_type, "KI4_6") :=
      !!kisym(cohort_type, "SpLangTherapistSwallowWithin72hrsPct"),

    !!kisym(cohort_type, "D4Score") := {
      round( (!!kisym(cohort_type, "KI4_1") +
             !!kisym(cohort_type, "KI4_2") +
             !!kisym(cohort_type, "KI4_3") +
             !!kisym(cohort_type, "KI4_4") +
             !!kisym(cohort_type, "KI4_5") +
             !!kisym(cohort_type, "KI4_6")) / 6, 1)
    },

    !!kisym(cohort_type, "D4Level") := {
      cut(!!kisym(cohort_type, "D4Score"),
          breaks = c(0, 65, 75, 80, 90, Inf),
          labels = c("E", "D", "C", "B", "A"))
    },

    !!kisym(cohort_type, "KI5_1") :=
      !!kisym(cohort_type, "OccTherApplicablePct"),

    !!kisym(cohort_type, "KI5_2") := {
      (cut(!!kisym(cohort_type, "OccTherMinutesQ2"),
         breaks = c(0,  4, 8,  12, 16,
                  20, 24, 28,  32, 40, Inf),
         labels = FALSE) * 10)
    },

    !!kisym(cohort_type, "KI5_3") :=
      !!kisym(cohort_type, "PCOccTherDaysQ2"),

    !!kisym(cohort_type, "KI5_4") :=
      (!!kisym(cohort_type, "OccTherApplicablePct") / 100) *
      !!kisym(cohort_type, "OccTherMinutesQ2") *
      (!!kisym(cohort_type, "PCOccTherDaysQ2") / 100) /
      (0.80 * 45 * 5 / 7) * 100,

    !!kisym(cohort_type, "D5Score") := {
      round( (!!kisym(cohort_type, "KI5_1") +
              !!kisym(cohort_type, "KI5_2") +
              !!kisym(cohort_type, "KI5_3") +
              !!kisym(cohort_type, "KI5_4")) / 4, 1)
    },

    !!kisym(cohort_type, "D5Level") := {
      cut(!!kisym(cohort_type, "D5Score"),
          breaks = c(0, 60, 65, 75, 80, Inf),
          labels = c("E", "D", "C", "B", "A"))
    },

    !!kisym(cohort_type, "KI6_1") :=
      !!kisym(cohort_type, "PhysioApplicablePct"),

    !!kisym(cohort_type, "KI6_2") := {
      (cut(!!kisym(cohort_type, "PhysioMinutesQ2"),
          breaks = c(0,  4, 8,  12, 16,
                    20, 24, 28,  32, 40, Inf),
          labels = FALSE) * 10)
    },

    !!kisym(cohort_type, "KI6_3") :=
      !!kisym(cohort_type, "PCPhysioDaysQ2"),

    !!kisym(cohort_type, "KI6_4") :=
      (!!kisym(cohort_type, "PhysioApplicablePct") / 100) *
      !!kisym(cohort_type, "PhysioMinutesQ2") *
      (!!kisym(cohort_type, "PCPhysioDaysQ2") / 100) /
      (0.85 * 45 * 5 / 7) * 100,

    !!kisym(cohort_type, "D6Score") := {
      round( (!!kisym(cohort_type, "KI6_1") +
            !!kisym(cohort_type, "KI6_2") +
            !!kisym(cohort_type, "KI6_3") +
            !!kisym(cohort_type, "KI6_4")) / 4, 1)
    },

    !!kisym(cohort_type, "D6Level") := {
      cut(!!kisym(cohort_type, "D6Score"),
        breaks = c(0, 60, 70, 75, 85, Inf),
        labels = c("E", "D", "C", "B", "A"))
    },

    !!kisym(cohort_type, "KI7_1") :=
      !!kisym(cohort_type, "SpeechLangApplicablePct"),


    !!kisym(cohort_type, "KI7_2") := {
      (cut(!!kisym(cohort_type, "SpeechLangMinutesQ2"),
        breaks = c(0,  4, 8,  12, 16,
                   20, 24, 28,  32, 40, Inf),
        labels = FALSE) * 10)
    },

    !!kisym(cohort_type, "KI7_3") :=
      !!kisym(cohort_type, "PCSpeechLangDaysQ2"),


    !!kisym(cohort_type, "KI7_4") :=
      (!!kisym(cohort_type, "SpeechLangApplicablePct") / 100) *
      !!kisym(cohort_type, "SpeechLangMinutesQ2") *
      (!!kisym(cohort_type, "PCSpeechLangDaysQ2") / 100) /
      (0.5 * 45 * 5 / 7) * 100,

    !!kisym(cohort_type, "D7Score") := {
      round( (!!kisym(cohort_type, "KI7_1") +
            !!kisym(cohort_type, "KI7_2") +
            !!kisym(cohort_type, "KI7_3") +
            !!kisym(cohort_type, "KI7_4")) / 4, 1)
    },

    !!kisym(cohort_type, "D7Level") := {
      cut(!!kisym(cohort_type, "D7Score"),
        breaks = c(0, 50, 55, 65, 75, Inf),
        labels = c("E", "D", "C", "B", "A"))
    },

    !!kisym(cohort_type, "KI8_1") :=
      !!kisym(cohort_type, "OccTherApplicableWithin72hrsPct"),

    !!kisym(cohort_type, "KI8_2") := {
      110 - (cut(!!kisym(cohort_type, "ClockStartToOTMinsQ2"),
                 breaks = c(0,  360,  720, 1080,  1440, 1800,
                            2160, 2520, 2880,  3240, 3600, Inf),
                 labels = FALSE) * 10)
    },

    !!kisym(cohort_type, "KI8_3") :=
      !!kisym(cohort_type, "PhysioApplicableWithin72hrsPct"),

    !!kisym(cohort_type, "KI8_4") := {
      110 - (cut(!!kisym(cohort_type, "ClockStartToPTMinsQ2"),
                 breaks = c(0,  360,  720, 1080,  1440, 1800,
                            2160, 2520, 2880,  3240, 3600, Inf),
                 labels = FALSE) * 10)
    },

    !!kisym(cohort_type, "KI8_5") :=
      !!kisym(cohort_type, "SLTCommApplicableWithin72hrsPct"),

    !!kisym(cohort_type, "KI8_6") := {
      110 - (cut(!!kisym(cohort_type, "ClockStartToSLTCommMinsQ2"),
                 breaks = c(0, 360,  720, 1080,  1440, 1800,
                            2160, 2520, 2880,  3240, 3600, Inf),
                 labels = FALSE) * 10)
    },

# TODO 8.7 and 8.8 need to go here

    !!kisym(cohort_type, "D8Score") := {
      round( (!!kisym(cohort_type, "KI8_1") +
             !!kisym(cohort_type, "KI8_2") +
             !!kisym(cohort_type, "KI8_3") +
             !!kisym(cohort_type, "KI8_4") +
             !!kisym(cohort_type, "KI8_5") +
             !!kisym(cohort_type, "KI8_6")) / 6, 1)
#      !!kisym(cohort_type, "KI8_7"),
#      !!kisym(cohort_type, "KI8_8")) / 8, 1)
    },

    !!kisym(cohort_type, "D8Level") := {
      cut(!!kisym(cohort_type, "D8Score"),
          breaks = c(0, 65, 75, 80, 85, Inf),
          labels = c("E", "D", "C", "B", "A"))
    },

    !!kisym(cohort_type, "KI10_1") := {
      !!kisym(cohort_type, "JointCarePlanningPct")
    },

    !!kisym(cohort_type, "KI10_2") := {
      pmin(c(!!kisym(cohort_type, "DischargedWithESDPct") * 2.5), 100)
    },

    !!kisym(cohort_type, "KI10_3") := {
      !!kisym(cohort_type, "AFWithAnticoagulationPct")
    },

    !!kisym(cohort_type, "KI10_4") := {
      !!kisym(cohort_type, "DischargeNamedContactPct")
    }#,

#    !!kisym(cohort_type, "D10Score") := {
#      round( (!!kisym(cohort_type, "KI10_1") +
#             !!kisym(cohort_type, "KI10_2") +
#             !!kisym(cohort_type, "KI10_3") +
#             !!kisym(cohort_type, "KI10_4")) / 4, 1)
#    },

#    !!kisym(cohort_type, "D10Level") := {
#      cut(!!kisym(cohort_type, "D10Score"),
#          breaks = c(0, 60, 75, 85, 90, Inf),
#          labels = c("E", "D", "C", "B", "A"))
#    }
  )
}
