#' Data List
#'
#' @param p plot object
#' @param keep boolean vector (size = 1 or length(plot$layers)). Determines if that layer should have cognostics calculated
#' @param ... parameters passed on to corresponding \code{layer_info}
#' @export
#' @rdname layer_info
#' @examples
#' require(ggplot2)
#' p <- ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
#'   geom_point(data = mpg, mapping = aes(cty, hwy))
#' layer_info(p)
layer_info <- function(p, keep = TRUE, ...) {
  UseMethod("layer_info", p)
}

#' @rdname layer_info
#' @export
layer_info.default <- function(p, keep = TRUE, ...) {
  stop("Please implement `layer_info.", class(p)[1], "(p, keep, ...)`")
}


# must return x, (y, ) group.
# if group is all equal, then there is only one grouping
#' @rdname layer_info
#' @export
layer_info.ggplot <- function(p, keep = TRUE, ...) {
  assert_logical(keep, any.missing = FALSE)
  if (length(keep) == 1) assert_true(keep)
  layer_list <- p$layers[keep]
  assert_list(layer_list, min.len = 1)

  layer_nums <- seq_len(layer_count(p))[keep]

  lapply(seq_along(layer_list), function(layer_i) {
    layer <- layer_list[[layer_i]]
    layer_data <-
      layer$layer_data(p$data) %>%
      mutate(PANEL = -2L) %>%
      layer$setup_layer(p)
    ret_data <- layer$compute_aesthetics(layer_data, p)

    if (
      ! tibble::has_name(ret_data, "x") &
      tibble::has_name(ret_data, "sample")
    ) {
      ret_data$x <- ret_data$sample
    }

    layer_name <- snake_class(layer$geom)

    val_or_empty <- function(x) {
      ret <- c(x, "")
      ret[1]
    }

    ret_name <- switch(layer_name,
      "geom_point" = switch(val_or_empty(snake_class(layer$position)),
        # "position_jitter" = "geom_jitter",
        switch(
          val_or_empty(snake_class(layer$stat)),
          "stat_qq" = "geom_qq",
          "stat_sum" = "geom_count",
          "geom_point"
        )
      ),
      "geom_smooth" = switch(val_or_empty(as.character(layer$stat_params$method)),
        "loess" = "geom_smooth_loess",
        "lm" = "geom_smooth_lm",
        "geom_smooth"
      ),
      "geom_tile" = if (inherits(layer$stat, "StatBin2d")) "geom_bin2d" else "geom_tile",
      "geom_bar" = if (inherits(layer$stat, "StatBin")) "geom_histogram" else "geom_bar",
      "geom_path" = if (inherits(layer$stat, "StatBin")) "geom_freqpoly" else "geom_path",
      "geom_rug" = {
        rug_sides <- strsplit(layer$geom_params$sides, "")[[1]]
        rug_axes <- c("t" = "x", "b" = "x", "r" = "y", "l" = "y")[rug_sides]
        paste("geom_rug_", paste(sort(unique(rug_axes)), collapse = ""), sep = "")
      },
      layer_name
    )

    params <- layer$stat_params
    if (has_name(params, "method.args")) {
      params$method_args <- params$method.args
      params$method.args <- NULL
    }

    list(
      name = ret_name,
      data = ret_data,
      params = params,
      layer_num = layer_nums[layer_i]
    )
  })
}
