#' @export
#' @title report_period_start_date_in_period
#'
#' @description
#' \code{report_period_start_date_in_period} takes a SSNAP reporting
#' period number and returns the start date of that period.
#'
#' @details
#' SSNAP has gone from a period of quarterly to trimesterly reports
#' (and back again). So we can't use conventional quarterly reporting
#' as we have trimesterly reporting for two financial years.
#' 
#' The report_period functions act as conversions between dates and
#' the reporting periods.
#'
#' @param period_number The period you want to know the first date of.
#' @return The date, expressed as a Date object.
#' @author Andrew Hill, \email{andrew.hill@@doctors.org.uk}
#' @examples
#' report_period_start_date_in_period(16)

report_period_start_date_in_period <- function(period_number) {
  if (!(is.numeric(period_number)) | (period_number < 1)) {
    stop("period_number is not an integer")
  }
  report_period_date <- lubridate::ymd("2013-01-01")

  if (period_number < 14) {
    lubridate::year(report_period_date) <- 2013 +
      ( (period_number - 1) %/% 4)
    lubridate::month(report_period_date) <- ( ( (period_number - 1)
                                              %% 4) * 3) + 1
  }

  if ( (period_number >= 14) && (period_number < 20)) {
    lubridate::year(report_period_date) <- 2016 +
      ( (period_number - 14) %/% 3)
    month <- ( (period_number - 13) %% 3) * 4
    if (month == 0) {
      month <- 12
    }
    lubridate::month(report_period_date) <- month
  }

  if (period_number >= 20) {
    lubridate::year(report_period_date) <- 2018 +
      ( (period_number - 19) %/% 4)
    lubridate::month(report_period_date) <- ( ( (period_number - 19)
                                              %% 4) * 3) + 1
  }

  return(report_period_date)
}

#' @export
#' @title report_period_end_date_in_period
#'
#' @description
#' \code{report_period_end_date_in_period} takes a SSNAP reporting
#' period number and returns the end date of that period.
#'
#' @details
#' SSNAP has gone from a period of quarterly to trimesterly reports
#' (and back again). So we can't use conventional quarterly reporting
#' as we have trimesterly reporting for two financial years.
#' 
#' The report_period functions act as conversions between dates and
#' the reporting periods.
#'
#' @param period_number The period you want to know the last date of.
#' @return The date, expressed as a Date object.
#' @author Andrew Hill, \email{andrew.hill@@doctors.org.uk}

report_period_end_date_in_period <- function(period_number) {
  report_period_start_date_in_period(period_number + 1) -
    lubridate::days(1)
}

#' Convert a date to the report number active at that time.
#'
#' SSNAP has gone from a period of quarterly to trimesterly reports
#' (and back again). So we can't use conventional quarterly reporting
#' as we have trimesterly reporting for two financial years.
#' This function calculates the current report number for a given
#' date. 
#'
#' @param date The date you want to know the reporting period for as
#' a Date object.
#' @return The reporting period, expressed as an integer value.
#' @examples
#' ssnap_report_period(as.Date('2014-02-01'))

#' @export

ssnap_report_period <- function(date) {
  if (!(lubridate::is.Date(date))) {
    stop("date is not a date")
  }
  dplyr::case_when(
    date >= as.Date("2013-01-01") & date < as.Date("2016-04-01") ~
      ( (lubridate::year(date) - 2013) * 4) +
      floor( (lubridate::month(date) - 1) / 3) + 1,

    date >= as.Date("2016-04-01") & date < as.Date("2018-04-01") ~
      ( (lubridate::year(date) - 2016) * 3) +
      floor(lubridate::month(date) / 4) + 13,

    date >= as.Date("2016-04-01") ~
      ( (lubridate::year(date) - 2018) * 4) +
      floor( (lubridate::month(date) - 1) / 3) + 19,
    TRUE ~ 0)
}

#' Tibble Time time series for SSNAP report periods
#'
#' SSNAP has gone from a period of quarterly to trimesterly reports
#' (and back again). So we can't use conventional quarterly reporting
#' as we have trimesterly reporting for two financial years.
#' This sets up a series of time series using
#' \code{tibbletime::create_series}. Please note that the series
#' currently ends in 2030 - the end date will need updating if the
#' code is to be used beyond that date.
#'
#' @examples
#' ssnap_report_period_series

#' @export

ssnap_report_period_series <- c(
  tibbletime::create_series(
    time_formula = "2012-04-01" ~ "2016-01-01",
    period = "quarterly",
    class = "Date",
    as_vector = TRUE),
  tibbletime::create_series(
    time_formula = "2016-04-01" ~ "2017-12-01",
    period = "4 months",
    class = "Date",
    as_vector = TRUE),
  tibbletime::create_series(
    time_formula = "2018-04-01" ~ "2030-03-31",
    period = "quarterly",
    class = "Date",
    as_vector = TRUE)
)

#' Tibble Time time series for SSNAP report years
#'
#' SSNAP follows the UK financial year calendar of April 1st year
#' start. \code{ssnap_report_year_series} returns a time series using
#' \code{tibbletime::create_series} following the SSNAP yearly
#' reports. Please note that the series currently ends in 2030 - the
#' end date will need updating if the code is to be used beyond that
#' date.
#'
#' @examples
#' ssnap_report_year_series

#' @export

ssnap_report_year_series <- tibbletime::create_series(
  time_formula = "2012-04-01" ~ "2030-04-01",
  period = "1 Y",
  class = "Date",
  as_vector = TRUE)
