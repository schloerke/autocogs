#' @include auto_cog.R

#' Cognostic
#'
#' Add a description to a cognostic (subset metric)
#' @param x univariate scalar
#' @param desc description of \code{x}
#' @export
#' @examples
#' cog_desc(mean(1:10), "mean of 10 numbers")
cog_desc <- function(x, desc = NULL) {
  assert_scalar(x, na.ok = TRUE)
  assert_character(desc, len = 1, any.missing = FALSE)
  attr(x, "desc") <- desc
  x
}


# type CogGroupRequirements {
#   name: String!
#   description: String
#   fields: [FieldInfo!]!
#   cog_types: [CogMetric!]!
# }
#
# type CogMetric {
#   name: String!
#   description: String!
#   func: function!
# }
#

# type FieldInfo {
#   dimension: FieldDimension
#   typeof: FieldTypeOf
# }
# enum FieldDimension {
#   X, Y, Z
#   OTHER,
#   ANY
# }
# enum FieldTypeOf {
#   CONTINUOUS
#   DISCRETE, DISCRETE_ORDERED, DISCRETE_UNORDERED
#   DATE
#   ANY
# }

#' @import tibble dplyr checkmate
NULL

#' Field Type Information
#'
#' @param dimension field name. Use one of the listed options provided
#' @param type field type. Use one of the listed options provided
#' @export
#' @import dplyr
field_info <- function(
  dimension = c("x", "y", "z", "group", "any"),
  type = c("continuous", "discrete", "date", "any")
) {
  dimension <- match.arg(dimension)
  type <- match.arg(type)

  assert_choice(dimension, c("x", "y", "z", "group", "any"))
  assert_choice(type, c("continuous", "discrete", "date", "any"))

  data_frame(dimension, type)
}


known_cog_groups_ <- data_frame(
  # Name of autocog
  name = character(0),
  # Fields required
  fields = list(),
  # Description of auto cog group
  description = character(0),
  # function to calculate the auto cogs
  fn = list()
)

#' Cognostic Group information
#'
#' To add more cognostic groups, please see \code{\link{add_cog_group}()}
#'
#' @export
#' @examples
#' known_cog_groups()
known_cog_groups <- function() {
  known_cog_groups_
}
known_cog_groups_name <- function() {
  known_cog_groups_$name
}

autocog_env <- environment()

#' Add a cognostic group
#'
#' Add a new cognostic to be used when calculating automatic cognostics.
#' @param name Name of cognostic group
#' @param fields \code{data.frame} of \code{'dimension'} and \code{'type'} columns. \code{dplyr::\link[dplyr]{bind_rows}()} of \code{\link{field_info}} outputs for convenience
#' @param description Description of cognostic group
#' @param fn function to calculate a cognostic group.  May return a named list or a single row tibble.  Each value of the return data should be the output of \code{\link{cog_desc}}
#' @param ... ignored
#' @export
#' @examples
#' \dontrun{
#' add_cog_group(
#'   "univariate_continuous",
#'   field_info("x", "continuous"),
#'   "univariate metrics for continuous data",
#'   function(x, ...) {
#'     x_range <- range(x, na.rm = TRUE)
#'     list(
#'       min = cog_desc(x_range[1], "minimum of non NA data"),
#'       max = cog_desc(x_range[2], "maximum of non NA data"),
#'       mean = cog_desc(mean(x, na.rm = TRUE), "mean of non NA data"),
#'       median = cog_desc(median(x, na.rm = TRUE), "median of non NA data"),
#'       var = cog_desc(var(x, na.rm = TRUE), "variance of non NA data")
#'     )
#'   }
#' )
#' }
add_cog_group <- function(
  name,
  fields,
  description = NA,
  fn,
  ...
  # ,
  # verbose = TRUE
) {

  assert_character(name, len = 1, any.missing = FALSE)
  assert_data_frame(fields, c("character", "character"), min.rows = 1)
  assert_names(names(fields), identical.to = c("dimension", "type"))
  assert_character(description, any.missing = TRUE)
  assert_function(fn)
  # assert_logical(verbose, len = 1, any.missing = FALSE)

  cog_group <- data_frame(
    name,
    fields = list(fields),
    description = description,
    fn = list(fn)
  )

  known_cog_groups_ <<- bind_rows(known_cog_groups_, cog_group)

  assign(
    paste("autocog_", name, sep = ""),
    autocog(name, .fn_only = TRUE),
    envir = autocog_env
  )

  invisible(cog_group)
}
