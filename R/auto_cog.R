

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
#' @rdname panel_cogs
#' @export
panel_cogs <- function(dt, panel_col = "panel", ...) {
  panels <- dt[[panel_col]]

  pb <- progress::progress_bar$new(total = length(panels), format = "auto cogs [:bar] :current/:total eta::eta ")

  panels %>%
    lapply(function(x) {
      pb$tick()
      calculate_auto_cogs(x, ...)
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
