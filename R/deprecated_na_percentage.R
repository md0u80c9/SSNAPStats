#' @importFrom rlang !!!
#' @importFrom rlang .data
#' @title deprecated_na_percentage
#'
#' @description
#' \code{deprecated_na_percentage} Calculates the percentage of rows
#' which are not NA values.
#'
#' @details
#' Calculates the percentage of rows which
#' are not NA values.
#'
#' @param field_name The name of the field to calculate the proportion
#' of non-NA values for.
#' @author Andrew Hill, \email{andrew.hill@@doctors.org.uk}

deprecated_na_percentage <- function(field_name) {
  dplyr::if_else(
    sum(field_name, na.rm = TRUE) == 0,
    0,
    round(sum(field_name, na.rm = TRUE) /
            sum(!is.na(field_name)) * 100, 1))
}
