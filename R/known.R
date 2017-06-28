

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

#' @export
#' @import dplyr
field_info <- function(
  dimension = c("x", "y", "z", "other", "any"),
  type = c("continuous", "discrete", "date", "any")
) {
  dimension = match.arg(dimension)
  type = match.arg(type)

  assert_choice(dimension, c("x", "y", "z", "other", "any"))
  assert_choice(type, c("continuous", "discrete","date", "any"))

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


cog <- function(name, fn, description = NULL, ..., fn_args = NULL) {
  assert_character(name, len = 1, any.missing = FALSE)
  assert_character(fn_args, null.ok = TRUE)
  assert_function(fn, args = fn_args, ordered = TRUE)
  assert_character(description, len = 1, null.ok = TRUE)

  data_frame(name, fn = list(fn), description = list(description))
}


known_cogs <- data_frame(
  name = character(0),
  fields = list(),
  description = character(0),
  cog_descriptions = list(),
  fn = list()
)

add_auto_cog <- function(
  name,
  fields,
  description = NA,
  cog_descriptions = NA,
  fn,
  ...,
  verbose = TRUE
) {
  if (missing(cog_descriptions)) cog_descriptions <- list()

  assert_character(name, len = 1, any.missing = FALSE)
  assert_data_frame(fields, c("character", "character"), min.rows = 1)
  assert_character(description, any.missing = TRUE)
  assert_list(cog_descriptions, "character", any.missing = FALSE, unique = TRUE)
  # assert_data_frame(cogs, c("character", "list", "list"), min.rows = 1)
  assert_function(fn)
  assert_logical(verbose, len = 1, any.missing = FALSE)

  known_cogs <<- bind_rows(
    known_cogs,
    data_frame(
      name,
      fields = list(fields),
      description = description,
      cog_descriptions = list(cog_descriptions),
      fn = list(fn)
    )
  )
}




add_auto_cog(
  "univariate_continuous",
  field_info("x", "continuous"),
  "univariate metrics for continuous data",
  list(
    "min" = "minimum of non NA data",
    "max" = "maximum of non NA data",
    "mean" = "mean of non NA data",
    "median" = "median of non NA data",
    # "mode" = "mode of non NA data",
    "var" = "variance of non NA data"
  ),
  function(x, ...) {
    x_range <- range(x, na.rm = TRUE)
    list(
      min = x_range[1],
      max = x_range[2],
      mean = mean(x, na.rm = TRUE),
      median = median(x, na.rm = TRUE),
      var = var(x, na.rm = TRUE)
    )
  }
)


add_auto_cog(
  "univariate_discrete",
  field_info("x", "discrete"),
  "univariate metrics for discrete data",
  list(
    "min_name", "minimum group count name",
    "count_min", "minimum group count",
    "count_mean", "average group count",
    "count_max", "maximum group count",
    "max_name", "maximum group count name"
  ),
  function(x, ...) {
    table_counts <- x %>% table() %>% sort()
    count_names <- names(table_counts)
    n_groups <- length(table_counts)
    list(
      min_name = count_names[1],
      count_min = table_counts[1],
      count_mean = sum(table_counts) / n_groups,
      count_max = table_counts[n_groups],
      max_name = count_names[n_groups]
    )
  }
)


add_auto_cog(
  "boxplot metrics",
  field_info("x", "continuous"),
  "univariate boxplot metrics for continuous data",
  list(
    "n_outlier_lower" = "count of lower outliers",
    "min_non_outlier" = "minimum point",
    "q1" = "first quartile value",
    "median" = "median value",
    "q3" = "third quartile value",
    "max_non_outlier" = "maximum non outlier point",
    "n_outlier_upper" = "count of upper outliers"
  ),
  function(x, ...) {
    boxplot_info <- StatBoxplot$compute_group(
      data.frame(x = 1, y = x), scales = NULL, width = 1, na.rm = FALSE, coef = 1.5
    )
    median <- boxplot_info$middle[[1]]
    outliers <- boxplot_info$outliers[[1]]
    list(
      "n_outlier_lower" = sum(outliers < median),
      "min_non_outlier" = boxplot_info$lower[[1]],
      "q1" = boxplot_info$notchlower[[1]],
      "median" = median,
      "q3" = boxplot_info$notchupper[[1]],
      "max_non_outlier" = boxplot_info$upper[[1]],
      "n_outlier_upper" = sum(outliers > median)
    )
  }
)





known_cogs %>% tibble::glimpse() %>% as.list() %>% print()
