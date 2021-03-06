% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ssnap_cohorts.R
\name{ssnap_cohorts}
\alias{ssnap_cohorts}
\title{Create a cohort of SSNAP data}
\usage{
ssnap_cohorts(ssnap_data, cohort, period_type = c("months", "periods",
  "financialyears"), aggregate_by = c("national", "team"),
  convert_team_codes = TRUE, filters = NULL)
}
\description{
This function is a higher level interface to the generic
create_cohorts functions.
}
\details{
Using the list of named cohort outputs in \code{ssnap_cohort_names}
it matches the correct cohort selection to the correct type of
operation and list of outputs, simplifying the eventual code in
reports and outputs.

You can still roll your own outputs by using the generic
\code{create_outputs} functions and supplying inputs from
\code{ssnap_cohort_definitions}, outputs from
\code{ssnap_audit_output_specs} or even building your own custom
output spec from \code{ssnap measures} depending upon how much
control and customisation you need.
}
