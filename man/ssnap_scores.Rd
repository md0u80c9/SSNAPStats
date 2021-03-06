% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ssnap_cohorts.R
\name{ssnap_scores}
\alias{ssnap_scores}
\title{Create the SSNAP Scores summary from multiple cohorts}
\usage{
ssnap_scores(ssnap_data, period_type = c("months", "periods",
  "financialyears"), aggregate_by = c("national", "team"),
  filters = NULL)
}
\arguments{
\item{ssnap_data}{Either a tibble containing raw SSNAP data, or
a string containing the name of a CSV file.}

\item{period_type}{A string determining what the gap between
time periods should be. The factor should either be 'months',
'periods' or financialyears'. Months and financialyears are
self-explanatory. Periods represent reporting periods - these were
initially quarterly, then were trimesterly, then quarterly again.
Periods are referred to by their numeric number, rather than a
defined time period to avoid confusion in the calcalations.}

\item{aggregate_by}{Set whether we are looking to aggregate by
'team' (default) or 'national'.}

\item{filters}{Filters to apply to ssnap_data before creating the
cohort. Common examples would be to select only a subset of teams
or a certain date range to reduce the need to summarise results
which are not going to be used subsequently.}
}
\value{
The cohort is returned in a \code{tibble}.
}
\description{
Unlike other cohorts, SSNAP Scores is built from the composite of
multiple cohorts, with a series of additional columns then created
on top. This can't be done easily within our generic audit
framework, so we create a special function for doing this.
}
