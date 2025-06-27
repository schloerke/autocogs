# perform all `<<-` actions within a local environment to avoid
# a locked package environment if another package wants to add cognostic info
known_cog_info <- local({
  known_cog_groups_ <- tibble::tibble(
    # Name of autocog
    name = character(0),
    # Fields required
    fields = list(),
    # Description of auto cog group
    description = character(0),
    # function to calculate the auto cogs
    fn = list()
  )

  known_cog_groups <- function() {
    known_cog_groups_
  }
  known_cog_groups_name <- function() {
    known_cog_groups_$name
  }
  known_cog_groups_add <- function(new_cog_group) {
    known_cog_groups_ <<- bind_rows(
      known_cog_groups_,
      new_cog_group
    )
    invisible(known_cog_groups())
  }

  return(list(
    known_cog_groups = known_cog_groups,
    known_cog_groups_name = known_cog_groups_name,
    known_cog_groups_add = known_cog_groups_add
  ))
})
known_cog_groups_fn <- known_cog_info$known_cog_groups
known_cog_groups_name <- known_cog_info$known_cog_groups_name
known_cog_groups_add <- known_cog_info$known_cog_groups_add

#' Cognostic Group information
#'
#' To add more cognostic groups, please see [add_cog_group()]
#'
#' @export
#' @examples
#' known_cog_groups()
known_cog_groups <- function() {
  known_cog_groups_fn()
}
