#' @include known.R
NULL











# type PlotAutoCogs {
#   name: String!
#   description: String
#   example: Picture
#   cognostics: [String!]! # list of all cognostics names. ex: univariate_counts, univariate_continuous
# }

known_plot_cogs <- data_frame(
  # plot mechanism (ggplot2, rbokeh, plotly, etc.)
  kind = character(0),

  # Name of plot type (boxplot, histogram, density)
  name = character(0),


  # Automatic cognostics to calculate
  layer_cogs = list(),

  # Plot type description
  description = character(0)

  # Creates nested data.frames of every auto_cog provided
  # fn = list()
)

#' Add plot layer cognostics
#'
#' Add a new set of cognostic groups for a given plot layer. If the plot layer is found, the corresponding cognostic groups will be calculated.
#' @param name Name of plot layer. This should match the output of the \code{"name"} values of \code{\link{layer_info}}
#' @param description Description of cognostic group
#' @param layer_cogs A \code{data.frame} (or \code{tibble}) containing the columns: "cog_group", "cols", "name".  "cog_group" column should contain a string value of a known cognostic group.  "cols" should be a single value or vector of column names to use from the data supplied by \code{\link{layer_info}} during calculations. "name" should contain the final storage name of the cognostic group.
#' @param kind String value that will match the output of \code{\link{plot_class}} of the desired plot object
#' @param ... ignored
#' @export
#' @examples
#' \dontrun{
#' add_layer_cogs(
#'   "geom_point",
#'   kind = "ggplot",
#'   "scatter plot points",
#'   tibble::tribble(
#'     ~ cog_group, ~ cols, ~ name,
#'     "univariate_continuous", "x", "_x",
#'     "univariate_continuous", "y", "_y",
#'     "bivariate_continuous", c("x", "y"), "_bivar",
#'     "scagnostics", c("x", "y"), "_scagnostic",
#'     "bivariate_counts", c("x", "y"), "_n"
#'   )
#' )
#' }
add_layer_cogs <- function(
  name,
  description,
  layer_cogs,
  # setup_data = NULL,
  kind = "ggplot",
  ...
  # #,
  # verbose = TRUE
) {
  assert_character(name, len = 1, any.missing = FALSE)
  assert_character(description, any.missing = FALSE)

  assert_data_frame(
    layer_cogs,
    c("character", "list"), ncols = 3, min.rows = 1,
    any.missing = FALSE
  )
  assert_names(names(layer_cogs), identical.to = c("cog_group", "cols", "name"))
  # assert_list(layer_cogs, any.missing = FALSE, unique = TRUE)

  # if (!is.null(setup_data)) assert_function(setup_data)

  assert_character(kind, any.missing = FALSE, len = 1)
  # verbose <- isTRUE(verbose)
  # assert_logical(verbose, len = 1, any.missing = FALSE)


  # cog_info_list <- lapply(layer_cogs, function(auto_cog) {
  #   if (is.list(auto_cog)) {
  #
  #     assert_character(auto_cog$cog_name, len = 1, any.missing = FALSE)
  #     assert_character(auto_cog$cols, min.len = 1, any.missing = FALSE)
  #     assert_character(auto_cog$store_name, len = 1, any.missing = FALSE)
  #     return(auto_cog)

      #
      # cog_info <- known_cog_groups[known_cog_groups$name == auto_cog[[1]], ]
      # if (nrow(cog_info) == 0) {
      #   browser()
      #   stop("could not find auto_cog information for name: '", auto_cog, "'")
      # }
      #
      # cog_info <- as.list(cog_info)
      # inner_cog_fn <- cog_info$fn
      #
      # cog_fn <- function(data, ...) {
      #   inner_cog_fn(data[[]])
      # }
      #
      # return(cog_info$fn)
    # }

  #   stop("layer_cogs item needs to be a list of two characters (auto_cog name, columns used)")
  # })


  known_plot_cogs <<- bind_rows(
    known_plot_cogs,
    data_frame(
      kind,
      name,
      layer_cogs = list(layer_cogs),
      # fn = list(fn),
      description
    )
  )
}
