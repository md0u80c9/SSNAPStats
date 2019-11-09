#' A vector of the bank holidays in England and Wales
#'
#' A list of bank holidays in England and Wales stored as a vector
#' of Dates, taken from https://www.gov.uk/bank-holidays. These are
#' needed to analyse differences between normal working hours,
#' weekends and public holidays. They are accurate up to the end of
#' 2019; they will need periodical maintenance to add the dates for
#' subsequent bank holidays.
#' @export

bank_holidays_engwales <- c(
  as.Date("2012-01-02"), as.Date("2012-04-06"), as.Date("2012-04-09"),
  as.Date("2012-05-07"), as.Date("2012-06-04"), as.Date("2012-06-05"),
  as.Date("2012-08-27"), as.Date("2012-12-25"), as.Date("2012-12-26"),

  as.Date("2013-01-01"), as.Date("2013-03-29"), as.Date("2013-04-01"),
  as.Date("2013-05-06"), as.Date("2013-05-27"), as.Date("2013-08-26"),
  as.Date("2013-12-25"), as.Date("2013-12-26"),

  as.Date("2014-01-01"), as.Date("2014-04-18"), as.Date("2014-04-21"),
  as.Date("2014-05-05"), as.Date("2014-05-26"), as.Date("2014-08-25"),
  as.Date("2014-12-25"), as.Date("2014-12-26"),

  as.Date("2015-01-01"), as.Date("2015-04-03"), as.Date("2015-04-06"),
  as.Date("2015-05-04"), as.Date("2015-05-25"), as.Date("2015-08-31"),
  as.Date("2015-12-25"), as.Date("2015-12-28"),

  as.Date("2016-01-01"), as.Date("2016-03-25"), as.Date("2016-03-28"),
  as.Date("2016-05-02"), as.Date("2016-05-30"), as.Date("2016-08-29"),
  as.Date("2016-12-26"), as.Date("2016-12-27"),

  as.Date("2017-01-02"), as.Date("2017-04-14"), as.Date("2017-04-17"),
  as.Date("2017-05-01"), as.Date("2017-05-29"), as.Date("2017-08-28"),
  as.Date("2017-12-25"), as.Date("2017-12-26"),

  as.Date("2018-01-01"), as.Date("2018-03-30"), as.Date("2018-04-02"),
  as.Date("2018-05-07"), as.Date("2018-05-28"), as.Date("2018-08-27"),
  as.Date("2018-12-25"), as.Date("2018-12-26"),

  as.Date("2019-01-01"), as.Date("2019-04-19"), as.Date("2019-04-22"),
  as.Date("2019-05-06"), as.Date("2019-05-27"), as.Date("2019-08-26"),
  as.Date("2019-12-25"), as.Date("2019-12-26")
)

#' A vector of the bank holidays in Northern Ireland
#'
#' A list of bank holidays in Northern Ireland stored as a vector
#' of Dates, taken from https://www.gov.uk/bank-holidays. These are
#' needed to analyse differences between normal working hours,
#' weekends and public holidays. They are accurate up to the end of
#' 2019; they will need periodical maintenance to add the dates for
#' subsequent bank holidays.
#' 
#' NB these are England and Wales bank holidays plus St Patrick's Day
#' and Battle of the Boyne.
#' @export

bank_holidays_ni <- c(
  bank_holidays_engwales,
  as.Date("2012-03-19"), as.Date("2012-07-12"),
  as.Date("2013-03-18"), as.Date("2013-07-12"),
  as.Date("2014-03-17"), as.Date("2014-07-14"),
  as.Date("2015-03-17"), as.Date("2015-07-13"),
  as.Date("2016-03-17"), as.Date("2016-07-12"),
  as.Date("2017-03-17"), as.Date("2017-07-12"),
  as.Date("2018-03-19"), as.Date("2018-07-12"),
  as.Date("2019-03-18"), as.Date("2019-07-12")
)
