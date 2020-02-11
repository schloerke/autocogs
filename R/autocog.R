

simplify_cogs <- function(cog_list) {
  cog_list %>%
    unlist(recursive = FALSE) ->
  cogs
  not_duplicated <- !duplicated(cogs, fromLast = TRUE)
  cogs[not_duplicated] %>%
    lapply(list) %>%
    as_tibble()
}


#' Panel cognostics
#'
#' Return or concatenate panel cognostics.  For each panel (plot) in the panel column, cognostics will be calculated for each panel. The result will be returned in a nested \code{\link[tibble]{tibble}}.
#' @param dt data to be used
#' @param panel_col panel column to be used in \code{dt}
#' @param ... parameters passed to \code{\link{layer_info}}
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
#' autocog("univariate_continuous", iris$Sepal.Length)
#' fn <- autocog("univariate_continuous", .fn_only = TRUE)
#' fn(iris$Sepal.Length)
autocog <- function(.name, ..., .fn_only = FALSE) {
  assert_character(.name, len = 1, any.missing = FALSE)
  assert_subset(.name, known_cog_groups_name(), empty.ok = FALSE)

  this_cog <- known_cog_groups()[known_cog_groups_name() == .name, ]

  # fields <- this_cog$fields[[1]]
  fn <- this_cog$fn[[1]]
  .description <- this_cog$description[[1]]

  if (isTRUE(.fn_only)) {
    ret <- function(...) {
      .name
      .description
      as_tibble(fn(...))
    }
    class(ret) <- c("autocog", class(ret))
    return(ret)
  }

  # TODO could check for field type here
  args <- list(...)
  as_tibble(do.call(fn, args))
}

format.autocog <- function(x, ...) {
  env <- environment(x)
  fn <- get("fn", envir = env)
  desc <- get(".description", envir = env)
  name <- get(".name", envir = env)

  paste(
    "Automatic Cognostic Function:\n\t",
      name, "\n",
    "Description:\n\t",
      if (is.null(desc)) "(none)" else desc,
      "\n",
    "\n",
    "autocog_", name, " <- ",
    paste(format(fn), collapse = "\n"),
    sep = ""
  )
}
print.autocog <- function(x, ...) {
  cat(format(x, ...), "\n")
}


# autocog_univariate_continuous <- autocog("univariate_continuous", .fn_only = TRUE)
# autocog_univariate_discrete <- autocog("univariate_discrete", .fn_only = TRUE)
# autocog_boxplot <- autocog("boxplot", .fn_only = TRUE)
# autocog_bivariate_continuous <- autocog("bivariate_continuous", .fn_only = TRUE)
# autocog_scagnostics <- autocog("scagnostics", .fn_only = TRUE)
# autocog_univariate_counts <- autocog("univariate_counts", .fn_only = TRUE)
# autocog_bivariate_counts <- autocog("bivariate_counts", .fn_only = TRUE)
# autocog_hex_counts <- autocog("hex_counts", .fn_only = TRUE)
# autocog_square_counts <- autocog("square_counts", .fn_only = TRUE)
# autocog_density_continuous <- autocog("density_continuous", .fn_only = TRUE)
