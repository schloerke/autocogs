#' @include known_layer_cogs.R
NULL

#' Add plot layer cognostics
#'
#' Add a new set of cognostic groups for a given plot layer. If the plot layer is found, the corresponding cognostic groups will be calculated.
#' @param name Name of plot layer. This should match the output of the \code{"name"} values of \code{\link{layer_info}}
#' @param description Description of cognostic group
#' @param cog_groups A \code{data.frame} (or \code{tibble}) containing the columns: "cog_group", "cols", "name".  "cog_group" column should contain a string value of a known cognostic group.  "cols" should be a single value or vector of column names to use from the data supplied by \code{\link{layer_info}} during calculations. "name" should contain the final storage name of the cognostic group.
#' @param kind String value that will match the output of \code{\link{plot_class}} of the desired plot object
#' @param ... ignored
#' @export
add_layer_cogs <- function(
  name,
  description,
  cog_groups,
  # setup_data = NULL,
  kind = "ggplot",
  ...
  # #,
  # verbose = TRUE
) {
  assert_character(name, len = 1, any.missing = FALSE)
  assert_character(description, any.missing = FALSE)

  assert_data_frame(
    cog_groups,
    c("character", "list"), ncols = 3, min.rows = 1,
    any.missing = FALSE
  )
  assert_names(names(cog_groups), identical.to = c("cog_group", "cols", "name"))
  # assert_list(cog_groups, any.missing = FALSE, unique = TRUE)

  # if (!is.null(setup_data)) assert_function(setup_data)

  assert_character(kind, any.missing = FALSE, len = 1)
  # verbose <- isTRUE(verbose)
  # assert_logical(verbose, len = 1, any.missing = FALSE)


  # cog_info_list <- lapply(cog_groups, function(auto_cog) {
  #   if (is.list(auto_cog)) {
  #
  #     assert_character(auto_cog$cog_name, len = 1, any.missing = FALSE)
  #     assert_character(auto_cog$cols, min.len = 1, any.missing = FALSE)
  #     assert_character(auto_cog$store_name, len = 1, any.missing = FALSE)
  #     return(auto_cog)

      #
      # cog_info <- cog_groups()[cog_groups_name() == auto_cog[[1]], ]
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

  #   stop("cog_groups item needs to be a list of two characters (auto_cog name, columns used)")
  # })


  known_layer_cogs_ <<- bind_rows(
    known_layer_cogs_,
    data_frame(
      kind,
      name,
      cog_groups = list(cog_groups),
      # fn = list(fn),
      description
    )
  )
}
