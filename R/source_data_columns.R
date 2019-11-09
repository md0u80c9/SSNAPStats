#' Find source data columns from a set of expressions
#' 
#' This function analyses a list of expressions, and aims to extract
#' all the field names of anything enclosed with
#' \code{.data[["ColumnName"]]}. When functional, this will be a
#' vital function for working out which CSV columns we need for any
#' given set of functions - currently we have to state this by hand
#' which is error-prone.
#' 
#' The current code needs optimising in order to practically help
#' improve query speed.
#' 
#' @param exprs_list A list of expressions that you want to extract
#' the column names from
#' @return A string vector of column names needed to produce all the
#' outputs in the expression list.
#' 
#' @importFrom rlang ":="
#' @importFrom rlang "!!"
#' @export

source_data_columns <- function(exprs_list) {

  get_ast <- function( ee ) {
    purrr::map_if(as.list(ee), is.call, get_ast)
  }

  asts <- exprs_list
  asts <- purrr::map(asts, get_ast )

  # If the input matches .data, [[ and a name, returns "name".
  # Otherwise, NULL.
  get_name <- function(x) {
    if (is.list(x) && length(x) == 3 &&
      identical( x[[1]], quote(`[[`) ) &&
      identical( x[[2]], quote(.data) ) &&
      is.character(x[[3]]) ) return(x[[3]])
    NULL
  }

  get_names <- function(aa) {
    field_names <- purrr::keep(aa, is.list)
    # Recurse to any list descendants
    field_names <- purrr::map(field_names, get_names)
    # Append self to the result
    field_names <- c(field_names, get_name(aa) )
    # Return as character vector, not list
    field_names <- unlist(field_names)
  }

  csv_fields <- get_names(asts)

  return(unique(csv_fields))
}
