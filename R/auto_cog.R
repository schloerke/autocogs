

simplify_cogs <- function(cog_list) {
  cog_list %>%
    unlist(recursive = FALSE) ->
  cogs
  not_duplicated <- !duplicated(cogs, fromLast = TRUE)
  cogs[not_duplicated] %>%
    lapply(list) %>%
    as_data_frame()
}


#' Panel cognostics
#'
#' Return or concatinate panel cognostics.  For each panel (plot) in the panel column, cognostics will be calculated for each panel. The result will be returned in a nested \code{\link[tibble]{tibble}}.
#' @param dt data to be used
#' @param panel_col panel column to be used in \code{dt}
#' @param ... parameters passed to \code{\link{get_data_list}}
#' @rdname panel_cogs
#' @export
panel_cogs <- function(dt, panel_col = "panel", ...) {
  panels <- dt[[panel_col]]

  pb <- progress::progress_bar$new(
    total = length(panels),
    format = "auto cogs [:bar] :current/:total eta::eta "
  )

  panels %>%
    lapply(function(x) {
      pb$tick()
      plot_cogs(x, ...)
    }) %>%
    lapply(simplify_cogs) %>%
    bind_rows() ->
  cog_dt

  cog_dt
}

#' @rdname panel_cogs
#' @export
add_panel_cogs <- function(dt, panel_col = "panel", ...) {
  bind_cols(dt, panel_cogs(dt, panel_col, ...))
}


#' Auto cognostic function
#'
#' Calculate an auto cognostic function given a name
#'
#' @param .name name of a known cognostic
#' @param ... arguments passed onto the found function
#' @param .fn_only boolean that determines if the function should be returned
#' @export
#' @examples
#' auto_cog("univariate_continuous", iris$Sepal.Length)
#' fn <- auto_cog("univariate_continuous", .fn_only = TRUE)
#' fn(iris$Sepal.Length)
auto_cog <- function(.name, ..., .fn_only = FALSE) {
  assert_character(.name, len = 1, any.missing = FALSE)
  assert_subset(.name, known_cogs$name, empty.ok = FALSE)

  this_cog <- known_cogs[known_cogs$name == .name, ]

  # fields <- this_cog$fields[[1]]
  fn <- this_cog$fn[[1]]
  attr(fn, "description") <- this_cog$description[[1]]

  if (isTRUE(.fn_only)) {
    ret <- function(...) {
      as_tibble(fn(...))
    }
    attr(ret, "fn") <- fn
    return(ret)
  }

  # TODO could check for field type here
  args <- list(...)
  as_tibble(do.call(fn, args))
}
