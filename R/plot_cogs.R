snakeize <- function(x) {
  x <- gsub("([A-Za-z])([A-Z])([a-z])", "\\1_\\2\\3", x)
  x <- gsub(".", "_", x, fixed = TRUE)
  x <- gsub("([a-z])([A-Z])", "\\1_\\2", x)
  tolower(x)
}

snake_class <- function(x) {
  snakeize(class(x)[1])
}


plot_cogs <- function(p, ..., spec = TRUE, verbose = FALSE) {
  plot_class_val <- plot_class(p)
  cog_specs <- as_cog_specs(p, spec)
  keep_layers <- cog_specs_keep_layer(cog_specs)
  layer_info <- get_layer_info(p, keep = keep_layers, ...)
  layer_cog_specs <- cog_specs[keep_layers]

  # for every layer
  lapply(layer_info, function(layer_item) {
    # get the layer cog info
    layer_cog_group <- known_layer_cogs() %>%
      filter(
        .data[["kind"]] == plot_class_val,
        .data[["name"]] == layer_item$name
      )

    # if the layer isnt registered, message and return early
    if (nrow(layer_cog_group) == 0) {
      if (verbose) {
        message(
          "no cog group found for layer: ",
          plot_class_val,
          ", ",
          layer_item$name
        )
      }
      return(NULL)
    }

    # get the layer info
    layer_cog_group <- as.list(layer_cog_group)

    # for every layer info row found, look at the auto_cog data frame
    lapply(seq_along(layer_cog_group$cog_groups), function(layer_cog_i) {
      layer_cog_spec <- layer_cog_specs[[layer_cog_i]]
      layer_cog_dt <- layer_cog_group$cog_groups[[layer_cog_i]]

      # remove unwanted cogs in layer
      layer_cog_dt <- layer_cog_dt[
        !(layer_cog_dt$cog_group %in% layer_cog_spec$remove),
      ]

      # produce a join of the request auto cogs and known auto cogs
      # (as the known cogs could have updated since last execution)
      item_cog_dt <- inner_join(
        layer_cog_dt,
        known_cog_groups(),
        c("cog_group" = "name")
      )

      if (nrow(item_cog_dt) != nrow(layer_cog_dt)) {
        if (verbose) {
          message(
            "missing cog groups found for auto cogs: ",
            paste(
              setdiff(layer_cog_dt$auto_cog, known_cog_groups_name()),
              sep = ", "
            )
          )
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
        if (is.null(ans)) {
          return(ans)
        }
        as_tibble(ans)
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
    }) -> layer_info

    # return for each layer
    layer_info
  }) -> ret

  # return all extra columns to be produced
  unlist(ret, recursive = FALSE)
}


get_layer_info <- function(p, keep = TRUE, ...) {
  ret <- layer_info(p, keep = keep, ...)
  assert_list(ret, min.len = 1)

  ans <- lapply(ret, function(item) {
    assert_list(item, max.len = 4, unique = TRUE)
    assert_names(
      names(item),
      subset.of = c("name", "data", "params", "layer_num")
    )
    assert_list(item$params, null.ok = TRUE)
    assert_character(item$name, len = 1, any.missing = FALSE)
    assert_data_frame(item$data)
    assert_numeric(item$layer_num, len = 1, any.missing = FALSE)
    item$data <- as_tibble(item$data)
    item
  })

  ans
}
