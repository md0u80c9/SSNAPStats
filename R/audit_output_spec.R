#' Coerces a tibble to become an audit output spec.
#'
#' This function is used to create an audit output specification
#' from a tibble. It will check that we have two columns named name
#' and type. Both must contain only strings. The type column must
#' only contain type keywords. If successful, the tibble is returned
#' as an audit_output_spec object.
#'
#' @param x The audit output specification written as a tibble.
#'  
#' @return A validated audit_output_spec object.
#' @export

as_audit_output_spec <- function(x) {
  if (!tibble::is_tibble(x)) {
    stop("as_audit_output_spec only works on tibbles")
  }

  return(tibble::new_tibble(x, subclass = "audit_output_spec"))
}

#' Test if the object is an audit_output_spec
#'
#' This function returns `TRUE` for audit_output_specs
#'
#' @param x An object
#' @return `TRUE` if the object inherits from the `audit_output_spec`
#' class.
#' @export

is_audit_output_spec <- function(x) {
  "audit_output_spec" %in% class(x)
}
