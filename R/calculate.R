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

plot_class <- function(p) {
  if (inherits(p, "ggplot")) {
    return("ggplot")
  }
  class(p)[1]
}


calculate_auto_cogs <- function(p, verbose = FALSE) {

  plot_class_val <- plot_class(p)
  layer_info <- get_layer_data(p)

  # for every layer
  lapply(layer_info, function(layer_item) {
    # get the layer cog info
    layer_cog_group <- plot_cogs %>% filter_(~ kind == plot_class_val, ~ name == layer_item$name)

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
    lapply(layer_cog_group$auto_cogs, function(auto_cog_dt) {

      # produce a join of the request auto cogs and known auto cogs
      item_cog_dt <- inner_join(auto_cog_dt, known_cogs, c("auto_cog" = "name"))

      if (nrow(item_cog_dt) != nrow(auto_cog_dt)) {
        if (verbose) {
          message("missing cog groups found for auto cogs: ", paste(setdiff(auto_cog_dt$auto_cog, known_cogs$name), sep = ", "))
          print(auto_cog_dt)
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

        if (has_name(layer_item$params, "method.args")) {
          layer_item$params$method_args <- layer_item$params$method.args
          layer_item$params$method.args <- NULL
        }

        fn <- item_cog_dt$fn[[i]]
        args <- append(dt_i_list, layer_item$params)
        ans <- do.call(fn, args)
        if (is.null(ans)) return(ans)
        as_data_frame(ans)
      })

      # store values with store_name
      ret <- list()
      ret[item_cog_dt$store_name] <- cog_ans

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





get_layer_data <- function(p) {
  ret <- get_data_list(p)
  ans <- lapply(ret, function(item) {
    assert_character(item$name, len = 1, any.missing = FALSE)
    assert_data_frame(item$data)
    item$data <- as_data_frame(item$data)
    # qq plot
    if (
      ! tibble::has_name(item$data, "x") & tibble::has_name(item$data, "sample")
    ) {
      item$data$x <- item$data$sample
    }
    item
  })
  for (i in seq_along(ans)) {
    ans[[i]]$layer_num <- i
  }
  ans
}

#' Data List
#'
#' @param p plot object
#' @export
#' @examples
#' require(ggplot2)
#' p <- ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
#'   geom_point(data = mpg, mapping = aes(cty, hwy))
#' get_data_list(p)
get_data_list <- function(p) {
  UseMethod("get_data_list", p)
}

#' @export
get_data_list.default <- function(p) {
  stop("Please implement `get_data_list.PLOT_TYPE(p)`")
}


# must return x, (y, ) group.
# if group is all equal, then there is only one grouping
#' @export
get_data_list.ggplot <- function(p) {
  lapply(p$layers, function(layer) {
    layer_data <- layer$layer_data(p$data) %>% mutate(PANEL = -2L)

    ret_data <- layer$compute_aesthetics(layer_data, p)

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
      layer_name
    )
    list(
      name = ret_name,
      data = ret_data,
      params = layer$stat_params
      # TODO
      # ,
      # geom_params = list(),
      # mapping = list(x, y, color, fill, ...)
    )
  })
}



# load_all(); p <- qplot(Sepal.Length, Sepal.Width, data = iris); calculate_auto_cogs(p)
