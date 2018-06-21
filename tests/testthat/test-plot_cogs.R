
context("plot_cogs")

library(ggplot2)


expect_auto_cogs <- function(
  p,
  names,
  lengths,
  debug = FALSE
) {
  cat("") # make the tests show up while running

  large_cog_list <- p %>% plot_cogs()

  # if (debug) browser()

  expect_list(large_cog_list, types = "list", len = 1)

  x <- large_cog_list[[1]]

  expect_list(x, types = c("data.frame"), len = length(lengths))
  expect_names(names(x), identical.to = names)

  lapply(seq_along(x), function(i) {
    item <- x[[i]]

    expect_tibble(item, nrows = 1, ncols = lengths[[i]])
  })

  TRUE
}



test_that("ggplot2::geom_bar", {
  qplot(Sepal.Length, data = iris, geom = "bar") %>%
    expect_auto_cogs(
      c("_x", "_n"),
      c(5, 2)
    )
})

test_that("ggplot2::geom_col", {
  # geom_col
  qplot(Sepal.Length, Sepal.Width, data = iris, geom = "col") %>%
    expect_auto_cogs(
      c("_x", "_y", "_n"),
      c(5, 5, 5)
    )
})

test_that("ggplot2::geom_hex", {
  qplot(carat, price, data = diamonds, geom = "hex") %>%
    expect_auto_cogs(
      c("_x", "_y", "_bivar", "_hex_bins", "_n"),
      c(5, 5, 2, 6, 5)
    )
})

test_that("ggplot2::geom_bin2d", {
  qplot(carat, price, data = diamonds, geom = "bin2d") %>%
    expect_auto_cogs(
      c("_square_bins", "_n"),
      c(6, 5)
    )
})

test_that("ggplot2::geom_point", {
  qplot(Sepal.Length, Sepal.Width, data = iris, geom = "point") %>%
    expect_auto_cogs(
      c("_x", "_y", "_bivar", "_scagnostic", "_n"),
      c(5, 5, 2, 1, 5)
    )
})

test_that("ggplot2::geom_boxplot", {
  qplot(x = 1, y = Sepal.Length, data = iris, geom = "boxplot") %>%
    expect_auto_cogs(
      c("_y", "_boxplot", "_n"),
      c(5, 7, 5)
    )
})

test_that("ggplot2::geom_count", {
  qplot(cty, hwy, data = mpg, geom = "count") %>%
    expect_auto_cogs(
      c("_x", "_y", "_x_counts", "_y_counts", "_bivar", "_xy_counts", "_scagnostic", "_n"),
      c(5, 5, 5, 5, 2, 6, 1, 5)
    )
})

test_that("ggplot2::geom_density", {
  qplot(cty, data = mpg, geom = "density") %>%
    expect_auto_cogs(
      c("_x", "_density", "_n"),
      c(5, 6, 2)
    )
})

test_that("ggplot2::geom_violin", {
  qplot(1, cty, data = mpg, geom = "violin") %>%
    expect_auto_cogs(
      c("_y", "_violin", "_n"),
      c(5, 6, 2)
    )
})

test_that("ggplot2::geom_density_2d", {
  qplot(carat, price, data = diamonds[1:100, ], geom = "density_2d") %>%
    expect_auto_cogs(
      c("_x", "_y", "_bivar", "_density_x", "_density_y", "_density2d", "_n"),
      c(5, 5, 2, 6, 6, 4, 5)
    )

  qplot(carat, price, data = diamonds[1:1001, ], geom = "density_2d") %>%
    expect_auto_cogs(
      c("_x", "_y", "_bivar", "_density_x", "_density_y", "_density2d", "_n"),
      c(5, 5, 2, 6, 6, 4, 5)
    )
})

test_that("ggplot2::geom_dotplot", {
  qplot(cty, data = mpg, geom = "dotplot") %>%
    expect_auto_cogs(
      c("_x", "_density_x", "_n"),
      c(5, 6, 2)
    )
})

test_that("ggplot2::geom_histogram", {
  qplot(Sepal.Length, data = iris, geom = "histogram") %>%
    expect_auto_cogs(
      c("_x", "_density_x", "_hist_x", "_n"),
      c(5, 6, 8, 2)
    )
})

test_that("ggplot2::geom_rug", {
  qplot(cty, hwy, data = mpg, geom = "rug") %>%
    expect_auto_cogs(
      c("_x", "_y", "_density_x", "_density_y", "_n_x", "_n_y"),
      c(5, 5, 6, 6, 2, 2),
      debug = TRUE
    )
  qplot(cty, hwy, data = mpg, geom = "rug", sides = "ltrb") %>%
    expect_auto_cogs(
      c("_x", "_y", "_density_x", "_density_y", "_n_x", "_n_y"),
      c(5, 5, 6, 6, 2, 2)
    )
  qplot(cty, hwy, data = mpg, geom = "rug", sides = "r") %>%
    expect_auto_cogs(
      c("_y", "_density_y", "_n"),
      c(5, 6, 2)
    )
  qplot(cty, hwy, data = mpg, geom = "rug", sides = "t") %>%
    expect_auto_cogs(
      c("_x", "_density_x", "_n"),
      c(5, 6, 2)
    )
})

test_that("ggplot2::geom_spoke", {
  qplot(
    Sepal.Length, Sepal.Width,
    radius = Petal.Length, angle = Petal.Width,
    data = iris,
    geom = "spoke"
  ) %>%
    expect_auto_cogs(
      c(
        "_x", "_density_x",
        "_y", "_density_y",
        "_angle", "_density_angle",
        "_radius", "_density_radius",
        "_n_xy", "_n_angle_radius"
      ),
      c(
        5, 6,
        5, 6,
        5, 6,
        5, 6,
        5, 5
      )
    )

  qplot(
    Sepal.Length, Sepal.Width,
    radius = 1, angle = Petal.Width,
    data = iris,
    geom = "spoke"
  ) %>%
    expect_auto_cogs(
      c(
        "_x", "_density_x",
        "_y", "_density_y",
        "_angle", "_density_angle",
        "_radius",
        "_n_xy", "_n_angle_radius"
      ),
      c(
        5, 6,
        5, 6,
        5, 6,
        5,
        5, 5
      )
    )

  qplot(
    Sepal.Length, Sepal.Width,
    radius = Petal.Length, angle = 1,
    data = iris,
    geom = "spoke"
  ) %>%
    expect_auto_cogs(
      c(
        "_x", "_density_x",
        "_y", "_density_y",
        "_angle",
        "_radius", "_density_radius",
        "_n_xy", "_n_angle_radius"
      ),
      c(
        5, 6,
        5, 6,
        5,
        5, 6,
        5, 5
      )
    )
})


test_that("ggplot2::geom_qq", {
  qplot(sample = Sepal.Length, data = iris, geom = "qq") %>%
    expect_auto_cogs(
      c("_x", "_qq"),
      c(5, 4)
    )
})


test_that("ggplot2::geom_smooth", {
  qplot(cty, hwy, data = mpg, geom = "smooth") %>%
    expect_auto_cogs(
      c("_x", "_y", "_bivar", "_smooth", "_n"),
      c(5, 5, 2, 3, 5)
    )
})



test_that("ggplot2::geom_smooth_loess", {
  p <- qplot(cty, hwy, data = mpg, geom = "smooth", method = "loess")
  p %>%
    expect_auto_cogs(
      c("_x", "_y", "_bivar", "_smooth", "_loess", "_n"),
      c(5, 5, 2, 3, 6, 5)
    )
  cogs <- plot_cogs(p)
  expect_equal(cogs[[1]]$"_loess"$span[1], 0.75)
  expect_equal(cogs[[1]]$"_loess"$degree[1], 2)

  p <- qplot(
    cty, hwy, data = mpg,
    geom = "smooth", method = "loess",
    span = 0.9, method.args = list(degree = 1)
  )
  p %>%
    expect_auto_cogs(
      c("_x", "_y", "_bivar", "_smooth", "_loess", "_n"),
      c(5, 5, 2, 3, 6, 5)
    )
  cogs <- plot_cogs(p)
  expect_equal(cogs[[1]]$"_loess"$span[1], 0.9)
  expect_equal(cogs[[1]]$"_loess"$degree[1], 1)
})


test_that("ggplot2::geom_smooth_lm", {
  p <- qplot(cty, hwy, data = mpg, geom = "smooth", method = "lm")
  p %>%
    expect_auto_cogs(
      c("_x", "_y", "_bivar", "_smooth", "_lm", "_n"),
      c(5, 5, 2, 3, 19, 5)
    )
})



test_that("ggplot2::geom_line", {
  p <- qplot(cty, hwy, data = mpg, geom = "line")
  p %>%
    expect_auto_cogs(
      c("_x", "_y", "_bivar", "_n"),
      c(5, 5, 2, 5)
    )
})
test_that("ggplot2::geom_path", {
  p <- qplot(cty, hwy, data = mpg, geom = "path")
  p %>%
    expect_auto_cogs(
      c("_x", "_y", "_bivar", "_n"),
      c(5, 5, 2, 5)
    )
})
test_that("ggplot2::geom_step", {
  p <- qplot(cty, hwy, data = mpg, geom = "step")
  p %>%
    expect_auto_cogs(
      c("_x", "_y", "_step", "_bivar", "_n"),
      c(5, 5, 11, 2, 5)
    )
})
