#' @title kisym
#'
#' @description
#' \code{kisym} is an internally-used function that allows us to
#' prefix a key indicator symbol name with 'TC' or 'PC' (for
#' patient-centred or team-centred). 
#'
#' @details
#' Although it is internal, we have elected not to prefix the
#' variable name with internal_ because it is used in situations
#' where having readable code is important, so the name reads
#' similarly to 'sym' for that reason.
#'
#' @param cohort_type the prefix - should be 'TC' or 'PC'
#' @param symbol_name the name of the symbol that we are going to use,
#' eg. MySym would produce a symbol name of TCMySym or PCMySym
#' respectively.
#' @author Andrew Hill, \email{andrew.hill@@doctors.org.uk}

kisym <- function(cohort_type, symbol_name) {
  return(rlang::sym(paste0(cohort_type, symbol_name)))
}
