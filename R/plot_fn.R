#' @include known.R
NULL











# type PlotAutoCogs {
#   name: String!
#   description: String
#   example: Picture
#   cognostics: [String!]! # list of all cognostics names. ex: univariate_counts, univariate_continuous
# }

plot_cogs <- data_frame(
  # plot mechanism (ggplot2, rbokeh, plotly, etc.)
  kind = character(0),

  # Name of plot type (boxplot, histogram, density)
  name = character(0),


  # Automatic cognostics to calculate
  auto_cogs = list(),

  # Plot type description
  description = character(0)

  # Creates nested data.frames of every auto_cog provided
  # fn = list()
)

add_plot_cog <- function(
  name,
  description,
  auto_cogs,
  setup_data = NULL,
  ...,
  kind = "ggplot",
  verbose = TRUE
) {
  assert_character(name, len = 1, any.missing = FALSE)
  assert_character(description, any.missing = FALSE)

  assert_data_frame(
    auto_cogs,
    c("character", "list"), ncols = 3, min.rows = 1,
    any.missing = FALSE
  )
  assert_names(names(auto_cogs), identical.to = c("auto_cog", "cols", "store_name"))
  # assert_list(auto_cogs, any.missing = FALSE, unique = TRUE)

  if (!is.null(setup_data)) assert_function(setup_data)

  assert_character(kind, any.missing = FALSE, len = 1)
  verbose <- isTRUE(verbose)
  assert_logical(verbose, len = 1, any.missing = FALSE)


  # cog_info_list <- lapply(auto_cogs, function(auto_cog) {
  #   if (is.list(auto_cog)) {
  #
  #     assert_character(auto_cog$cog_name, len = 1, any.missing = FALSE)
  #     assert_character(auto_cog$cols, min.len = 1, any.missing = FALSE)
  #     assert_character(auto_cog$store_name, len = 1, any.missing = FALSE)
  #     return(auto_cog)

      #
      # cog_info <- known_cogs[known_cogs$name == auto_cog[[1]], ]
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

  #   stop("auto_cogs item needs to be a list of two characters (auto_cog name, columns used)")
  # })


  plot_cogs <<- bind_rows(
    plot_cogs,
    data_frame(
      kind,
      name,
      auto_cogs = list(auto_cogs),
      # fn = list(fn),
      description
    )
  )
}
