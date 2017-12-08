


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
