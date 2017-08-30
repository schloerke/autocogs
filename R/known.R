

cog <- trelliscopejs::cog
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


known_cogs <- data_frame(
  # Name of autocog
  name = character(0),
  # Fields required
  fields = list(),
  # Description of auto cog group
  description = character(0),
  # function to calculate the auto cogs
  fn = list()
)

add_auto_cog <- function(
  name,
  fields,
  description = NA,
  fn,
  ...,
  verbose = TRUE
) {

  assert_character(name, len = 1, any.missing = FALSE)
  assert_data_frame(fields, c("character", "character"), min.rows = 1)
  assert_character(description, any.missing = TRUE)
  # assert_data_frame(cogs, c("character", "list", "list"), min.rows = 1)
  assert_function(fn)
  assert_logical(verbose, len = 1, any.missing = FALSE)

  known_cogs <<- bind_rows(
    known_cogs,
    data_frame(
      name,
      fields = list(fields),
      description = description,
      fn = list(fn)
    )
  )
}
