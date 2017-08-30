

simplify_cogs <- function(cog_list) {
  cog_list %>%
    unlist(recursive = FALSE) ->
  cogs
  not_duplicated <- !duplicated(cogs, fromLast = TRUE)
  cogs[not_duplicated] %>%
    lapply(list) %>%
    as_data_frame()
}



#' @export
panel_cogs <- function(dt, panel_cogs = "panel") {
  panels <- dt[[panel_col]]

  pb <- progress::progress_bar$new(total = length(panels), format = "auto cogs [:bar] :current/:total eta::eta ")

  panels %>%
    lapply(function(x) {
      pb$tick()
      calculate_auto_cogs(x)
    }) %>%
    lapply(simplify_cogs) %>%
    bind_rows() ->
  cog_dt

  cog_dt
}

#' @export
add_panel_cogs <- function(dt, ...) {
  bind_cols(dt, panel_cogs(dt, ...))
}
