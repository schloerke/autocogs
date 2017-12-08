
# type PlotAutoCogs {
#   name: String!
#   description: String
#   example: Picture
#   cognostics: [String!]! # list of all cognostics names. ex: univariate_counts, univariate_continuous
# }

known_layer_cogs_ <- data_frame(
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

#' Layer Cognostic groups
#'
#' Display all layer cognostic information to be paired with information from \code{\link{known_cog_groups}()}.
#'
#' @export
#' @examples
#' known_layer_cogs()
known_layer_cogs <- function() {
  known_layer_cogs_
}
