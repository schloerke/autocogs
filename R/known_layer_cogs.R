# type PlotAutoCogs {
#   name: String!
#   description: String
#   example: Picture
#   cognostics: [String!]! # list of all cognostics names. ex: univariate_counts, univariate_continuous
# }

# perform all `<<-` actions within a local environment to avoid
# a locked package environment if another package wants to add cognostic info
known_layer_info <- local({
  known_layer_cogs_ <- tibble::tibble(
    # plot mechanism (ggplot2, rbokeh, plotly, etc.)
    kind = character(0),

    # Name of plot type (boxplot, histogram, density)
    name = character(0),

    # Automatic cognostics to calculate
    cog_groups = list(),

    # Plot type description
    description = character(0)

    # Creates nested data.frames of every auto_cog provided
    # fn = list()
  )

  known_layer_cogs <- function() {
    known_layer_cogs_
  }
  known_layer_cogs_add <- function(new_layer_cogs) {
    known_layer_cogs_ <<- bind_rows(
      known_layer_cogs_,
      new_layer_cogs
    )
    invisible(known_layer_cogs())
  }

  list(
    known_layer_cogs = known_layer_cogs,
    known_layer_cogs_add = known_layer_cogs_add
  )
})

known_layer_cogs_fn <- known_layer_info$known_layer_cogs
known_layer_cogs_add <- known_layer_info$known_layer_cogs_add

#' Layer Cognostic groups
#'
#' Display all layer cognostic information to be paired with information from [known_cog_groups()].
#'
#' @export
#' @examples
#' known_layer_cogs()
known_layer_cogs <- function() {
  known_layer_cogs_fn()
}
