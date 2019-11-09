#' Calculate difference between two date or times inline
#'
#' Many measures need to work on the difference between two
#' date/times.
#' 
#' This function calculates the difference between those times, or
#' returns an NA. The returned value is an integer, which makes
#' calculations simple.
#' 
#' Always calculate to the lowest precision you want to report - so
#' if you want to report hours and minutes then ask for the time in
#' minutes and convert the value later at the printing stage to be
#' in hours and minutes.
#' 
#' Many measures needing this should ideally have this value stored
#' in the source data ultimately, so should be recorded for potential
#' review of database inputs at a later stage.
#' 
#' @param start_field The field at the beginning of the time interval.
#' @param end_field The field at the end of the time interval.
#' @param units This is a string which must match those in the
#' difftime function (ie. 'minutes', 'hours', 'days' etc)
#' @export

internal_time_difference <- function(start_field,
                                     end_field,
                                     units = "mins") {
  dplyr::if_else(
    ( start_field > 0) &
      ( end_field > 0),
    as.integer(difftime(end_field, start_field,
                        units = units)),
    NA_integer_)
}
