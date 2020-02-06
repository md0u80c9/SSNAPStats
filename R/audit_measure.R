#' Creates a new audit_measure object
#'
#' This family of functions are used to create a new audit_measure
#' object.
#' Audit measure objects have two subclasses: audit_measure_continuous
#' and audit_measure_discrete. You can create both using the
#' audit_measure function (to allow us to create multiple objects at
#' once, for example from a script).
#' 
#' Audit measure objects describe the rules which will be applied to
#' a cohort to derive a set of aggregated output measures.
#' The denominator for these measures will be the cohort that they
#' are applied over.
#' 
#' Any exclusions from the cohort are specified in exclusions.
#' Each measure can have one or more numerators. This allows measures
#' to be described in groups or a breakdown of results into
#' categories.
#'
#' @param stem_name The name of the new audit measure
#' @param description A short textual description of what the audit
#' measure is intended to measure.
#' @param exclusions Any exclusions from the population expressed as
#' an expression.
#' @param numerators One or more numerators for the measure. This is
#' either the variable if we are looking at a continuous measure, or
#' the pass / fail criteria expressed as a boolean expression.
#' Results can be broken down by using a series of numerators.
#' @param numerator_descriptors A descriptor for each numerator. These
#' are optional but if you provide one you must provide one for every
#' numerator.
#' @param is_key_indicator Is this measure a key indicator?
#' @param reference_id A unique reference number for this audit
#' measure. Different audits use different reference schemes. We
#' don't use this reference for calculations, but it is included in
#' metadata and may be used for some of the outputs.
#' @param csv_columns a character vector of all the columns from the
#' raw CSV file needed to make this audit measure.
#' @param measure_type Describes the type of variable - whether it is
#' discrete (usually TRUE/FALSE), or continuous (where medians and
#' quartiles may be used to describe the output).
#'  
#' @return The created audit_measure object.
#' @export

audit_measure <- function(stem_name,
                          description,
                          exclusions = NULL,
                          numerators,
                          numerator_descriptors = NULL,
                          new_numerators = NULL,
                          is_key_indicator = FALSE,
                          reference_id = NULL,
                          csv_columns = NULL,
                          measure_type = c("discrete",
                                           "continuous")) {
  if (!is.character(stem_name)) {
    stop("stem_name must be a string")
  }
  if (is.character(exclusions)) {
    exclusions <- rlang::parse_expr(exclusions)
  }
  if (!is.null(exclusions) & !rlang::is_expression(exclusions)) {
    stop("exclusions must be either NULL or an expression")
  }
  if (!rlang::is_expression(numerators) & !rlang::is_list(numerators)) {
    stop(glue::glue("numerators must be either an expression or a
                    list of expressions"))
  }
  if (!is.null(numerator_descriptors) &
    ( (rlang::is_expression(numerators) &
      length(numerator_descriptors) != 1) |
    (rlang::is_list(numerators) &
      (length(numerators) != length(numerator_descriptors))))) {
    stop(glue::glue("if numerator_descriptors are provided you must \\
                     provide one for each numerator: there are \\
                     {length(numerators)} numerators and \\
                     {length(numerator_descriptors)} numerator \\
                     descriptors."))
  }
  
  if (!tibble::is_tibble(new_numerators)) {
    if (rlang::is_expression(new_numerators)) {
      new_numerators = tibble::tribble(
        ~numerator, ~fun, ~descriptor,
        stem_name, new_numerators, description)
    } else {
      stop("numerators must be either a tibble or an expression")
    }
  } else {
    if (!all(c("numerator", "fun", "descriptor") %in%
             names(new_numerators))) {
      stop(glue::glue("numerators must contain the columns \\
                      numerator, fun and descriptor"))
    }
  }
  
  if (!is.null(csv_columns) & !is.character(csv_columns)) {
    stop("csv_columns must be either NULL or a character vector")
  }

  if (is.list(numerators)) {
    names(numerators) <- glue::glue("{stem_name}{names(numerators)}")
  } else {
    numerators <- list(numerators)
    names(numerators) <- stem_name
  }

  if (!is.character(description)) {
    stop("description must be a character string")
  }
  
  if (!is.logical(is_key_indicator)) {
    stop("is_key_indicator must be logical (TRUE or FALSE)")
  }

  if (length(measure_type) != 1) {
    stop("measure_type must set to either 'discrete' or 'continuous'")
  }

  audit_measure_output <- rlang::list2(
    "stem_name" = stem_name,
    "exclusions" = exclusions,
    "numerators" = numerators,
    "numerator_descriptors" = numerator_descriptors,
    "new_numerators" = new_numerators,
    "is_key_indicator" = is_key_indicator,
    "reference_id" = reference_id,
    "csv_columns" = csv_columns,
    "measure_type" = measure_type,
    "description" = description)

  if (measure_type == "discrete") {
    class(audit_measure_output) <-
      c("audit_measure_discrete", "audit_measure")
  }
  if (measure_type == "continuous") {
    class(audit_measure_output) <-
      c("audit_measure_continuous", "audit_measure")
  }

  return(audit_measure_output)
}

#' Test if the object is an audit_measure
#'
#' This function returns `TRUE` for audit_measures
#'
#' @param x An object
#' @return `TRUE` if the object inherits from the `audit_measure`
#' class.
#' @export

is_audit_measure <- function(x) {
  "audit_measure" %in% class(x)
}

#' @rdname is_audit_measure
#' @usage NULL
#' @export
is.audit_measure <- is_audit_measure

#' @rdname audit_measure
#' @export

audit_measure_continuous <- function(stem_name,
                                     description,
                                     exclusions = NULL,
                                     numerators,
                                     numerator_descriptors = NULL,
                                     csv_columns = NULL) {
  return(audit_measure(
    stem_name = stem_name,
    description = description,
    exclusions = exclusions,
    numerators = numerators,
    numerator_descriptors = numerator_descriptors,
    csv_columns = csv_columns,
    measure_type = c("continuous")))
}

#' @rdname audit_measure
#' @export

audit_measure_discrete <- function(stem_name,
                                   description,
                                   exclusions = NULL,
                                   numerators,
                                   numerator_descriptors = NULL,
                                   csv_columns = NULL) {
  return(audit_measure(
    stem_name = stem_name,
    description = description,
    exclusions = exclusions,
    numerators = numerators,
    numerator_descriptors = numerator_descriptors,
    csv_columns = csv_columns,
    measure_type = c("discrete")))
}

#' Create outputs from an audit_measure object
#'
#' \code{create_output} is a method to produce a set of instructions
#' to pass to \code{dplyr::summarise}, which is then applied over a
#' cohort to produce our cohort results. The instructions are passed
#' as \code{rlang::exprs}, which can be appended to the summary step
#' using the \code{rlang::!!!} operator.
#' Output columns have a consistent naming convention.
#' 
#' @param x An \code{audit_measure} object.
#' @param numerator If the measure has more than one numerator and
#' only one of the numerators is required, this should be set to the
#' name of the numerator. If only one numerator is specified in the
#' measure, or all numerators are to be used, then this should be
#' omitted (or set to NULL).
#' @param output_type Determines which sort of aggregated results
#' are being requested from the data:
#' \itemize{
#'   \item \strong{median} {Only applicable to
#' \code{audit_measure_continuous} types. It calculates the median
#' and returns it in a column named with the \code{audit_measure}'s
#' stem name followed by Q2, e.g. \strong{myMeasureQ2}.
#' Multiple numerators are not supported by the median output type
#' and will generate an error. This may be fixed if there is a need
#' for it in future. Exclusion criteria are also not currently
#' handled - this is likely to be addressed in future.}
#'   \item \strong{quartiles} {This option is similar to \code{median}
#' but in addition to generating Q2, it also creates the lower and
#' upper quartiles, suffixed by Q1 and Q3.}
#' }
#' @export
create_output <- function(x,
                          numerator = NULL,
                          output_type) {
  UseMethod("create_output")
}

#' @export
create_output.default <- function(x,
                                  numerator = NULL,
                                  output_type) {
  stop("Unknown class")
}

#' @rdname create_output
#' @export
create_output.audit_measure_continuous <- function(x,
    numerator = NULL,
    output_type) {

  available_output_types <- c("median", "quartiles")
  if (!output_type %in% available_output_types) {
    stop(glue::glue(
      "output type for a continuous variable must be either median or
      quartiles"))
  }

  measure <- x

  numerator_with_exclusions <- measure$numerators[[1]]
  if (!is.null(measure$exclusions)) {
    numerator_with_exclusions <- rlang::expr(dplyr::if_else(
      !! measure$exclusions,
      NA_integer_,
      !! measure$numerators[[1]]
    ))
  }

  probs <- list("median" = 0.5,
                "quartiles" = c(0.5, 0.25, 0.75))
  indicator_name <- list("median" = "Q2",
                         "quartiles" = c("Q2", "Q1", "Q3"))

  output <- purrr::map(probs[[output_type]], ~ rlang::expr(
    stats::quantile(!! numerator_with_exclusions,
                               probs = !!.x,
                               na.rm = TRUE)))
  names(output) <- glue::glue(
    "{measure$stem_name}{indicator_name[[output_type]]}")
  output
}

#' @rdname create_output
#' @export
create_output.audit_measure_discrete <- function(x,
                                                 numerator = NULL,
                                                 output_type) {

  available_output_types <- c("d_n_pct",
                              "d_n_percentage",
                              "pct",
                              "percentage")
  if (!output_type %in% available_output_types) {
    stop(glue::glue(
      "output type for a discrete variable must be either pct or
      d_n_pct"))
  }

  measure <- x

  # If we specified a numerator then select only that one;
  # otherwise provide all numerators (multiple numerators on one
  # go not yet supported)

  if (!is.null(numerator)) {
    numerator_output <- measure$numerators[
      glue::glue("{measure$stem_name}{numerator}")]
  } else {
    numerator_output <- measure$numerators
  }

  # If there are exclusion criteria add these to the denominator
  # and any numerators. The numerator sums all rows where the
  # measure is TRUE and not excluded.

  if (is.null(measure$exclusions)) {
    denom <- rlang::expr(dplyr::n())
    nums <- purrr::map(numerator_output, ~ rlang::expr(
      sum( (!!.x), na.rm = TRUE)))
  } else {
    denom <- rlang::expr(dplyr::n() - sum(!!measure$exclusions))
    nums <- purrr::map(numerator_output, ~ rlang::expr(
      sum( (!!.x) & !(!!measure$exclusions), na.rm = TRUE)))
  }

  # If we are outputting the denominator and numerator, set up
  # the names for the output, and put the outputs into a vector.
  # Set both these vectors as NULL initially so if we aren't
  # using them we can just 'add null' without checking.

  d_name <- NULL
  n_names <- NULL
  d_output <- NULL
  n_outputs <- NULL
  if (startsWith(output_type, "d_n_")) {
    d_name <- glue::glue("{measure$stem_name}D")
    n_names <-  glue::glue("{names(numerator_output)}N")
    d_output <- denom
    n_outputs <- nums
    d_sym <- rlang::sym(d_name)
    pct_outputs <- lapply(rlang::syms(n_names),
      function (x) rlang::expr(round(!!x / !!d_sym * 100, 1)))
  } else {
    pct_outputs <- lapply(nums,
      function (x) rlang::expr(round(!!x / !!denom * 100, 1)))
  }

  # Build the percentage functions from the denominator and any
  # number of numerator sections. Then name the list items.


  output <- c(d_output, rbind(n_outputs, pct_outputs))
  names(output) <- c(d_name,
                     rbind(n_names,
                     glue::glue("{names(numerator_output)}Pct")))
  output
}

#' Create descriptions of an output from an audit_measure
#'
#' This function is a companion to create_output and is called by
#' create_output_tbl. Where create_output creates a set of expressions
#' for an audit_measure; create_descriptors outputs a string vector of
#' descriptors for that measure. These are used in audit outputs. The
#' function inputs miror those of create_output.
#' @param x An \code{audit_measure} object.
#' @param numerator If the measure has more than one numerator and
#' only one of the numerators is required, this should be set to the
#' name of the numerator. If only one numerator is specified in the
#' measure, or all numerators are to be used, then this should be
#' omitted (or set to NULL).
#' @param output_type Determines which sort of aggregated results
#' are being requested from the data:
#' @export
create_descriptors <- function(x,
                          numerator = NULL,
                          output_type) {
  UseMethod("create_descriptors")
}

#' @export
create_descriptors.default <- function(x,
                                  numerator = NULL,
                                  output_type) {
  stop("Unknown class")
}


#' @rdname create_descriptors
#' @export
create_descriptors.audit_measure_continuous <- function(x,
                                                   numerator = NULL,
                                                   output_type) {

  measure <- x

  available_output_types <- c("median", "quartiles")
  if (!output_type %in% available_output_types) {
    stop(glue::glue(
      "output type for a continuous variable must be either median or
      quartiles"))
  }

  rows <- if (output_type == "quartiles") 3 else 1
  descriptors <- vector("character", rows)

  descriptors[[1]] <- if (!is.null(measure$numerator_descriptors[[1]])) {
    measure$description
  } else {
    measure$stem_name
  }
  descriptors
}


#' @rdname create_descriptors
#' @export
create_descriptors.audit_measure_discrete <- function(x,
                                                 numerator = NULL,
                                                 output_type) {

  measure <- x

  available_output_types <- c("d_n_pct",
                              "d_n_percentage",
                              "pct",
                              "percentage")
  if (!output_type %in% available_output_types) {
    stop(glue::glue(
      "output type for a discrete variable must be either pct or
      d_n_pct"))
  }

  d_n_rows <- if (startsWith(output_type, "d_n_")) 1 else 0
  num_rows <- if (is.null(numerator)) {
    length(measure$numerators)
  } else 1
  rows <- (1 + d_n_rows) * num_rows + d_n_rows

  if (is.null(measure$numerator_descriptors)) {
    return (rep(measure$stem_name, times = rows))
  }

  descriptors <- vector("character", rows)

  if (is.null(numerator)) {
    descriptors <- if (d_n_rows == 0) {
      measure$numerator_descriptors
    } else {
      c(measure$description,
        rbind(measure$numerator_descriptors, ""))
    }
  } else {
    if (d_n_rows == 1) {
      descriptors[1] <- measure$description
    }
    descriptors[d_n_rows + 1] <-
      measure$numerator_descriptors[[
        glue::glue("{measure$stem_name}{numerator}")]]
  }
  descriptors
}

#' Create categories of an output from an audit_measure
#'
#' This function is a companion to create_output and is called by
#' create_output_tbl. Where create_output creates a set of expressions
#' for an audit_measure; create_categories outputs a string vector of
#' the category type for that measure, allowing us to sort the
#' aggregated data once produced. Unlike the other class methods, an
#' extra parameter, category, must be supplied: this is the string
#' category name which will be assigned to all output rows.
#' @param x An \code{audit_measure} object.
#' @param numerator If the measure has more than one numerator and
#' only one of the numerators is required, this should be set to the
#' name of the numerator. If only one numerator is specified in the
#' measure, or all numerators are to be used, then this should be
#' omitted (or set to NULL).
#' @param output_type Determines which sort of aggregated results
#' are being requested from the data.
#' @param category Name of the category as a string. This will be set
#' as a blank string if unspecified.
#' @export
create_categories <- function(x,
                              numerator = NULL,
                              output_type,
                              category = "") {
  UseMethod("create_categories")
}

#' @export
create_categories.default <- function(x,
                              numerator = NULL,
                              output_type,
                              category = "") {
  stop("Unknown class")
}


#' @rdname create_categories
#' @export
create_categories.audit_measure_continuous <- function(x,
                                                      numerator = NULL,
                                                      output_type,
                                                      category = "") {
  available_output_types <- c("median", "quartiles")
  if (!output_type %in% available_output_types) {
    stop(glue::glue(
      "output type for a continuous variable must be either median or
      quartiles"))
  }

  rows <- if (output_type == "quartiles") 3 else 1
  rep(category, times = rows)
}


#' @rdname create_categories
#' @export
create_categories.audit_measure_discrete <- function(x,
                                                     numerator = NULL,
                                                     output_type,
                                                     category = "") {

  measure <- x

  available_output_types <- c("d_n_pct",
                              "d_n_percentage",
                              "pct",
                              "percentage")
  if (!output_type %in% available_output_types) {
    stop(glue::glue(
      "output type for a discrete variable must be either pct or
      d_n_pct"))
  }

  d_n_rows <- if (startsWith(output_type, "d_n_")) 1 else 0
  num_rows <- if (is.null(numerator)) {
    length(measure$numerators)
  } else 1
  rows <- (1 + d_n_rows) * num_rows + d_n_rows

  rep(category, times = rows)
}


#' Create data type label for an output from an audit_measure
#'
#' This function is a companion to create_output and is called by
#' create_output_tbl. Where create_output creates a set of expressions
#' for an audit_measure; create_data_type_label creates a label which
#' describes what sort of data the row represents (ie. denominator,
#' numerator, median, percentage etc).
#' @param x An \code{audit_measure} object.
#' @param numerator If the measure has more than one numerator and
#' only one of the numerators is required, this should be set to the
#' name of the numerator. If only one numerator is specified in the
#' measure, or all numerators are to be used, then this should be
#' omitted (or set to NULL).
#' @param output_type Determines which sort of aggregated results
#' are being requested from the data.
#' @export
create_data_type_label <- function(x,
                              numerator = NULL,
                              output_type) {
  UseMethod("create_data_type_label")
}

#' @export
create_data_type_label.default <- function(x,
  numerator = NULL,
  output_type) {
  stop("Unknown class")
}


#' @rdname create_data_type_label
#' @export
create_data_type_label.audit_measure_continuous <- function(x,
  numerator = NULL,
  output_type) {

  available_output_types <- c("median", "quartiles")
  if (!output_type %in% available_output_types) {
    stop(glue::glue(
      "output type for a continuous variable must be either median or
      quartiles"))
  }

  rows <- if (output_type == "quartiles") {
    return(c("Median", "Lower IQR", "Upper IQR"))
  }
  return("Median")
}


#' @rdname create_data_type_label
#' @export
create_data_type_label.audit_measure_discrete <- function(x,
  numerator = NULL,
  output_type) {

  measure <- x

  available_output_types <- c("d_n_pct",
                              "d_n_percentage",
                              "pct",
                              "percentage")
  if (!output_type %in% available_output_types) {
    stop(glue::glue(
      "output type for a discrete variable must be either pct or
      d_n_pct"))
  }

  d_n_rows <- if (startsWith(output_type, "d_n_")) 1 else 0
  field_labels <- if (d_n_rows == 1) c("n", "%") else "%"
  num_rows <- if (is.null(numerator)) {
    length(measure$numerators)
  } else 1

  descriptors <- rep(field_labels,
                     times = num_rows)
  if (d_n_rows == 1) {
    descriptors <- c("d", descriptors)
  }
  descriptors
}


#' Create a list of outputs from a table of audit_measure objects
#'
#' This function operates on a tibble containing the following
#' columns:
#' \itemize{
#'   \item \strong{x} {The output measure as a quosure.}
#'   \item \strong{numerator} {a numerator name if one is required.}
#'   \item \strong{output_type} {The type of output required. For
#'   continuous variables this should be \strong{median} or
#'   \strong{quartiles}, for discrete ones \strong{pct} or
#'   \strong{d_n_pct}.}
#' }
#' At present the tibble will need to be created in code because we
#' are referring to the quosure directly; in future though we will
#' use a list for all the measures and a name to refer to in the list
#' will be the column in the table.  
#' @export

create_output_tbl <- function(outputs_table) {
  tibble::tibble(
    categories = unlist(purrr::pmap(
      .f = create_categories,
      .l = outputs_table)),
    descriptors = unlist(purrr::pmap(
      .f = create_descriptors,
      .l = dplyr::select(outputs_table,
                         "x",
                         "numerator", "output_type"))),
    data_types = unlist(purrr::pmap(
      .f = create_data_type_label,
      .l = dplyr::select(outputs_table,
                         "x",
                         "numerator", "output_type"))),
    exprs = unlist(purrr::pmap(
      .f = create_output,
      .l = dplyr::select(outputs_table,
                         "x",
                         "numerator", "output_type")))
  )
}

# This is experimental work to replace creation of the outputs table
# With more efficient / easier to follow code.

# First we pick which numerators we want.

# Then we recode the output type (so far we just use that
# to produce the 'units' but we can use case to recode to a list of
# units with their functions).
# We then separate the vector (we will need to unnest the list if we
# opt for a list format.

# As part of the review we should look to merge numerators and
# numerator_descriptors in audit_measure as a tibble (ultimately as
# class objects; so a single operation extracts them and removes the
# danger of having mismatched labels.

#' Create a list of outputs from a table of audit_measure objects

#' @export
new_output_table <- function(outputs_table) {
  outputs_table$output_type <- 
    dplyr::recode(outputs_table$output_type,
                  d_n_pct = "d-n,%",
                  pct = "%",
                  quartiles = "Q1,Q2,Q3",
                  median = "Q2")
  outputs_table <-
    tidyr::separate_rows(outputs_table,
                         "output_type",
                         sep = "-")

  outputs_table <-
    tidyr::hoist(outputs_table, "x",
                 new_numerators = "new_numerators")
  outputs_table <-
    dplyr::mutate(outputs_table,
       new_numerators = dplyr::if_else(
         output_type != "d",
           new_numerators,
           NULL))
  
  outputs_table <- dplyr::select(outputs_table, -"x")
  outputs_table <- dplyr::rename(outputs_table,
                                 "desired_numerator" = "numerator")
  outputs_table <-
    tidyr::unnest(outputs_table,
                         col = "new_numerators",
                  keep_empty = TRUE)
  outputs_table <- dplyr::filter(outputs_table,
    (numerator == desired_numerator) | (desired_numerator == "NULL")
    | (fun == "NULL"))
  outputs_table <- dplyr::select(outputs_table, -"desired_numerator")

#  outputs_table$output_type <- 
#    dplyr::recode(outputs_table$output_type,
#                  d_n_pct = "d,n,%",
#                  pct = "%",
#                  quartiles = "Q1,Q2,Q3",
#                  median = "Q2")
  outputs_table <-
    tidyr::separate_rows(outputs_table,
                         "output_type",
                         sep = ",")

  outputs_table <- dplyr::select(outputs_table,
                                 "categories" = category,
                                 "descriptors" = descriptor,
                                 "output_type",
                                 "exprs"= fun)
}

#' Given an outputs_table containing a list of aggregated data
#' outputs, produce a list of all the data columns (raw and modified)
#' needed to create those outputs.
#' @param outputs_table A table of audit outputs (previously
#' created using create_output_tbl())
#' @return A character vector of names of the data columns needed
#' to create the
csv_columns_from_output_table <- function(outputs_table) {
  unique(
    unlist(
      purrr::map(.x = outputs_table[["x"]],
                 .f = "csv_columns")
  ))
}
