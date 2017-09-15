#' @include plot_fn.R
NULL



snakeize <- function(x) {
  x <- gsub("([A-Za-z])([A-Z])([a-z])", "\\1_\\2\\3", x)
  x <- gsub(".", "_", x, fixed = TRUE)
  x <- gsub("([a-z])([A-Z])", "\\1_\\2", x)
  tolower(x)
}

snake_class <- function(x) {
  snakeize(class(x)[1])
}

#' Plot class
#'
#' First class of the plot object. Exception is ggplot2 as many objects are of class 'gg'
#' @param p plot object to retrieve class from
#' @rdname plot_class
#' @export
#' @examples
#' library(ggplot2)
#' p <- qplot(Sepal.Length, Sepal.Width, data = iris)
#' plot_class(p)
plot_class <- function(p) {
  UseMethod("plot_class", p)
}
#' @rdname plot_class
#' @export
plot_class.default <- function(p) {
  class(p)[1]
}
#' @rdname plot_class
#' @export
plot_class.gg <- function(p) {
  NextMethod()
}
#' @rdname plot_class
#' @export
plot_class.ggplot <- function(p) {
  return("ggplot")
}


#' Plot layer count
#'
#' Retrieves the number of layers in a given plot
#' @param p plot object
#' @return number
#' @export
#' @rdname layer_count
#' @examples
#' library(ggplot2)
#' p <- ggplot(iris, aes(Sepal.Length, Sepal.Width)) + geom_point()
#' layer_count(p) # 1
#' layer_count(p + geom_smooth(method = "lm") + geom_density_2d()) # 3
layer_count <- function(p) {
  UseMethod("layer_count")
}
#' @export
#' @rdname layer_count
layer_count.default <- function(p) {
  stop("Please implement `layer_count.", class(p)[1], "(p)`")
}
#' @export
#' @rdname layer_count
layer_count.ggplot <- function(p) {
  length(p$layers)
}

upgrade_cog_specs <- function(p, specs) {

  if (length(specs) < 1) {
    stop("`specs` must have a length of at least 1")
  }
  if (length(specs) == 1) {
    if (is.logical(specs[[1]])) {
      specs <- rep(specs[[1]], layer_count(p))
    }
  }
  specs <- as.list(specs)

  if (length(specs) != layer_count(p)) {
    stop("`specs` should have a length equal to the numer of layers in the plot (", layer_count(p), ") or 1, not ", length(specs))
  }

  specs <- lapply(specs, function(spec) {
    if (inherits(spec, "cog_spec")) {
      return(spec)
    }
    if (!test_logical(spec, any.missing = FALSE)) {
      stop("`spec` values should either by logical or created from `cog_spec()`. Found: ", paste(class(spec), collapse = ", "))
    }
    # only true of false values
    cog_spec(keep = spec)
  })

  specs
}


plot_cogs <- function(p, ..., spec = TRUE, verbose = FALSE) {

  plot_class_val <- plot_class(p)
  cog_specs <- upgrade_cog_specs(p, spec)
  keep_layers <- lapply(cog_specs, `[[`, "keep") %>% unlist()
  layer_info <- get_layer_data(p, keep = keep_layers, ...)

  # for every layer
  lapply(layer_info, function(layer_item) {
    # get the layer cog info
    layer_cog_group <- known_plot_cogs %>% filter_(~ kind == plot_class_val, ~ name == layer_item$name)

    # if the layer isnt registered, message and return early
    if (nrow(layer_cog_group) == 0) {
      if (verbose) {
        message("no cog group found for layer: ", plot_class_val, "::", layer_item$name)
      }
      return(NULL)
    }

    # get the layer info
    layer_cog_group <- as.list(layer_cog_group)

    # for every layer info row found, look at the auto_cog data frame
    lapply(layer_cog_group$layer_cogs, function(layer_cog_dt) {

      # produce a join of the request auto cogs and known auto cogs
      # (as the known cogs could have updated since last execution)
      item_cog_dt <- inner_join(
        layer_cog_dt, known_cog_groups,
        c("cog_group" = "name")
      )

      if (nrow(item_cog_dt) != nrow(layer_cog_dt)) {
        if (verbose) {
          message("missing cog groups found for auto cogs: ", paste(setdiff(layer_cog_dt$auto_cog, known_cog_groups$name), sep = ", "))
          print(layer_cog_dt)
        }
      }
      if (nrow(item_cog_dt) == 0) {
        return(NULL)
      }

      ret <- list()

      # for every auto cog
      cog_ans <- lapply(seq_len(nrow(item_cog_dt)), function(i) {

        # make a data list for fn execution
        dt_i_list <- list()

        if (nrow(item_cog_dt$fields[[i]]) != length(item_cog_dt$cols[[i]])) {
          utils::str(as.list(item_cog_dt[i, c("cols", "fields")]))
          stop("non matching lengths found for cog info and spec")
        }

        for (pos in seq_along(item_cog_dt$cols[[i]])) {
          dt_name <- item_cog_dt$fields[[i]]$dimension[[pos]]
          dt_i_list[[dt_name]] <- layer_item$data[[item_cog_dt$cols[[i]][pos]]]
        }

        # dt_i_list$plot <- p
        # dt_i_list$layer_data <- layer_item
# TODO listen to field type here
        fn <- item_cog_dt$fn[[i]]
        args <- append(dt_i_list, layer_item$params)
        ans <- do.call(fn, args)
        if (is.null(ans)) return(ans)
        as_data_frame(ans)
      })

      # store values by name store_name
      ret <- list()
      ret[item_cog_dt$name] <- cog_ans

      # remove any NULL values
      nulls <- lapply(ret, is.null) %>% unlist()
      if (any(nulls)) {
        ret[nulls] <- NULL
      }

      # return layer group info
      ret
    }) ->
    layer_info

    # return for each layer
    layer_info
  }) ->
  ret

  # return all extra columns to be produced
  unlist(ret, recursive = FALSE)
}





get_layer_data <- function(p, keep = TRUE, ...) {
  ret <- get_data_list(p, keep = keep, ...)
  assert_list(ret, min.len = 1)

  ans <- lapply(ret, function(item) {
    assert_list(item, max.len = 4, unique = TRUE)
    assert_names(names(item), subset.of = c("name", "data", "params", "layer_num"))
    assert_list(item$params, null.ok = TRUE)
    assert_character(item$name, len = 1, any.missing = FALSE)
    assert_data_frame(item$data)
    assert_numeric(item$layer_num, len = 1, any.missing = FALSE)
    item$data <- as_data_frame(item$data)
    item
  })

  ans
}

#' Data List
#'
#' @param p plot object
#' @param keep boolean vector (size = 1 or length(plot$layers)). Determines if that layer should have cognostics calculated
#' @param ... parameters passed on to corresponding \code{get_data_list}
#' @export
#' @rdname get_data_list
#' @examples
#' require(ggplot2)
#' p <- ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
#'   geom_point(data = mpg, mapping = aes(cty, hwy))
#' get_data_list(p)
get_data_list <- function(p, keep = TRUE, ...) {
  UseMethod("get_data_list", p)
}

#' @rdname get_data_list
#' @export
get_data_list.default <- function(p, keep = TRUE, ...) {
  stop("Please implement `get_data_list.", class(p)[1], "(p, keep, ...)`")
}


# must return x, (y, ) group.
# if group is all equal, then there is only one grouping
#' @rdname get_data_list
#' @export
get_data_list.ggplot <- function(p, keep = TRUE, ...) {
  assert_logical(keep, any.missing = FALSE)
  if (length(keep) == 1) assert_true(keep)
  layer_list <- p$layers[keep]
  assert_list(layer_list, min.len = 1)

  layer_nums <- seq_len(layer_count(p))[keep]

  lapply(seq_along(layer_list), function(layer_i) {
    layer <- layer_list[[layer_i]]
    layer_data <- layer$layer_data(p$data) %>% mutate(PANEL = -2L)

    ret_data <- layer$compute_aesthetics(layer_data, p)

    if (
      ! tibble::has_name(ret_data, "x") &
      tibble::has_name(ret_data, "sample")
    ) {
      ret_data$x <- ret_data$sample
    }

    layer_name <- snake_class(layer$geom)

    ret_name <- switch(layer_name,
      "geom_point" = switch(snake_class(layer$position),
        # "position_jitter" = "geom_jitter",
        switch(
          snake_class(layer$stat),
          "stat_qq" = "geom_qq",
          "stat_sum" = "geom_count",
          "geom_point"
        )
      ),
      "geom_smooth" = switch(as.character(layer$stat_params$method),
        "loess" = "geom_smooth_loess",
        "lm" = "geom_smooth_lm",
        "geom_smooth"
      ),
      "geom_tile" = if (inherits(layer$stat, "StatBin2d")) "geom_bin2d" else "geom_tile",
      "geom_bar" = if (inherits(layer$stat, "StatBin")) "geom_histogram" else "geom_bar",
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
