


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

  tibble(dimension, type)
}
