% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/audit_measure.R
\name{create_data_type_label}
\alias{create_data_type_label}
\alias{create_data_type_label.audit_measure_continuous}
\alias{create_data_type_label.audit_measure_discrete}
\title{Create data type label for an output from an audit_measure}
\usage{
create_data_type_label(x, numerator = NULL, output_type)

\method{create_data_type_label}{audit_measure_continuous}(x,
  numerator = NULL, output_type)

\method{create_data_type_label}{audit_measure_discrete}(x,
  numerator = NULL, output_type)
}
\arguments{
\item{x}{An \code{audit_measure} object.}

\item{numerator}{If the measure has more than one numerator and
only one of the numerators is required, this should be set to the
name of the numerator. If only one numerator is specified in the
measure, or all numerators are to be used, then this should be
omitted (or set to NULL).}

\item{output_type}{Determines which sort of aggregated results
are being requested from the data.}
}
\description{
This function is a companion to create_output and is called by
create_output_tbl. Where create_output creates a set of expressions
for an audit_measure; create_data_type_label creates a label which
describes what sort of data the row represents (ie. denominator,
numerator, median, percentage etc).
}
