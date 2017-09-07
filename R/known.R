

cog <- function(x, desc) {
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

#' Field Type info
#'
#' @param dimension field name. (use one of the listed options provided)
#' @param type field type. (use one of the listed options provided)
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


# field_infos <- function(...) {
#   args <- list(...)
#   if (length(args) %% 2 != 0 || length(args) == 0) {
#     stop("must supply a positive, even length number of args")
#   }
#
#   lapply(2 * seq_len(length(args) / 2), function(i) {
#     field_info(args[[i - 1]], args[[i]])
#   }) %>%
#     bind_rows()
# }


# cog <- function(name, fn, description = NULL, ..., fn_args = NULL) {
#   assert_character(name, len = 1, any.missing = FALSE)
#   assert_character(fn_args, null.ok = TRUE)
#   assert_function(fn, args = fn_args, ordered = TRUE)
#   assert_character(description, len = 1, null.ok = TRUE)
#
#   data_frame(name, fn = list(fn), description = list(description))
# }


known_cog_groups <- data_frame(
  # Name of autocog
  name = character(0),
  # Fields required
  fields = list(),
  # Description of auto cog group
  description = character(0),
  # function to calculate the auto cogs
  fn = list()
)

#' Add a cognostic group
#'
#' Add a new cognostic to be used when calculating automatic cognostics.
#' @param name Name of cognostic group
#' @param fields \code{data.frame} of \code{'dimension'} and \code{'type'} columns. \code{dplyr::\link[dplyr]{bind_rows}()} of \code{\link{field_info}} outputs for convenience
#' @param description Description of cognostic group
#' @param fn function to calculate a cognostic group.  May return a named list or a single row tibble.  Each value of the return data should be the output of \code{\link{cog}}
#' @param ... ignored
#' @export
#' @examples
# \dontrun{
# add_cog_group(
#   "univariate_continuous",
#   field_info("x", "continuous"),
#   "univariate metrics for continuous data",
#   function(x, ...) {
#     x_range <- range(x, na.rm = TRUE)
#     list(
#       min = cog(x_range[1], "minimum of non NA data"),
#       max = cog(x_range[2], "maximum of non NA data"),
#       mean = cog(mean(x, na.rm = TRUE), "mean of non NA data"),
#       median = cog(median(x, na.rm = TRUE), "median of non NA data"),
#       var = cog(var(x, na.rm = TRUE), "variance of non NA data")
#     )
#   }
# )
# }
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

  known_cog_groups <<- bind_rows(known_cog_groups, cog_group)

  invisible(cog_group)
}
