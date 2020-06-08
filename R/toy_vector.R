new_numerator <- function(
  name = character(),
  fun = rlang::exprs(),
  descriptor = character()) {
  
  vctrs::vec_assert(name,character())
  vctrs::vec_assert(fun, rlang::exprs())
  vctrs::vec_assert(descriptor, character())

  vctrs::new_rcrd(list(
    "name" = name,
    "fun" = fun,
    "descriptor" = descriptor
  ), class = "numerator")
}

numerator <- function(
  name = character(),
  fun = rlang::exprs(),
  descriptor = character()) {
  
  if (is.call(fun)) {
    fun = rlang::exprs(fun)
  }
  new_numerator(name, fun, descriptor)
}

format.numerator <- function(x, ...) {
  info <- sprintf("Placeholder")
  #  info <- sprintf("%s is %s",
  #                  vctrs::field(x, "x"),
  #                  vctrs::field(x, "descriptor"))
  paste(info, sep = "\n")
}

obj_print_data.numerator <- function(x) {
  cat(format(x), sep = "\n")
}

vec_ptype2.numerator.numerator <-
  function(x, y, ...) new_numerator()

vec_cast.numerator.numerator <- function(x, to, ...) x

setOldClass(c("numerator", "vctrs_vctr"))




new_aggregate_audit_measure <- function(
  x = audit_measure_vctrs(),
  numerator = character(),
  category_label = character(),
  aggregation = character()) {
    #TODO Assert x to be an audit measure
    # Needs audit_measure to be vctrs-compliant
    vctrs::vec_assert(x,audit_measure_vctrs())
    vctrs::vec_assert(numerator, character())
    vctrs::vec_assert(category_label, character())
    vctrs::vec_assert(aggregation, character())
     
    vctrs::new_rcrd(list(
      "x" = x,
      "descriptor" = numerator,
      "category_label" = category_label,
      "aggregation" = aggregation
    ), class = "aggregate_audit_measure")
}

format.aggregate_audit_measure <- function(x, ...) {
  info <- sprintf("Placeholder")
#  info <- sprintf("%s is %s",
#                  vctrs::field(x, "x"),
#                  vctrs::field(x, "descriptor"))
  paste(info, sep = "\n")
}

obj_print_data.aggregate_audit_measure <- function(x) {
  cat(format(x), sep = "\n")
}

vec_ptype2.aggregate_audit_measure.aggregate_audit_measure <-
  function(x, y, ...) new_aggregate_audit_measure()

vec_cast.aggregate_audit_measure.aggregate_audit_measure <- function(x, to, ...) x
 
setOldClass(c("aggregate_audit_measure", "vctrs_vctr"))