context("plot_cogs")

library(ggplot2)

expect_auto_cogs <- function(p, names, lengths) {
  cat("") # make the tests show up while running

  large_cog_list <- p %>% calculate_auto_cogs()

  expect_list(large_cog_list, types = "list", len = 1)

  x <- large_cog_list[[1]]

  ans1 <- expect_list(x, types = c("data.frame"), len = length(lengths))
  ans2 <- expect_names(names(x), identical.to = names)

  lapply(seq_along(x), function(i) {
    item <- x[[i]]

    expect_tibble(item, nrows = 1, ncols = lengths[[i]])
  })

  TRUE
}



test_that("ggplot2::geom_bar", {

  qplot(Sepal.Length, data = iris, geom = "bar") %>%
    expect_auto_cogs(
      names = c("_x", "_n"),
      lengths = c(5, 2)
    )
})

test_that("ggplot2::geom_col", {
  # geom_col
  qplot(Sepal.Length, Sepal.Width, data = iris, geom = "col") %>%
    expect_auto_cogs(
      names = c("_x", "_n"),
      lengths = c(5, 5)
    )
})

test_that("ggplot2::geom_hex", {
  qplot(carat, price, data = diamonds, geom = "hex") %>%
    expect_auto_cogs(
      names = c("_x", "_y", "_bivar", "_hex_bins", "_n"),
      lengths = c(5, 5, 2, 6, 5)
    )
})

test_that("ggplot2::geom_bin2d", {
  qplot(carat, price, data = diamonds, geom = "bin2d") %>%
    expect_auto_cogs(
      names = c("_square_bins", "_n"),
      lengths = c(6, 5)
    )
})

test_that("ggplot2::geom_point", {
  qplot(Sepal.Length, Sepal.Width, data = iris, geom = "point") %>%
    expect_auto_cogs(
      c("_x", "_y", "_bivar", "_scagnostic", "_n"),
      c(5, 5, 2, 9, 5)
    )
})

test_that("ggplot2::geom_boxplot", {
  qplot(x = 1, y = Sepal.Length, data = iris, geom = "boxplot") %>%
    expect_auto_cogs(
      c("_y", "_boxplot", "_n"),
      c(5, 7, 2)
    )
})

test_that("ggplot2::geom_count", {
  qplot(cty, hwy, data = mpg, geom = "count") %>%
    expect_auto_cogs(
      c("_x", "_y", "_x_counts", "_y_counts", "_bivar", "_xy_counts", "_scagnostic", "_n"),
      c(5, 5, 5, 5, 2, 6, 9, 5)
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
      c(5, 5, 2, 6, 6, 3, 5)
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
      c(5, 6, 6, 2)
    )
})

test_that("ggplot2::geom_rug", {
  qplot(cty, data = mpg, geom = "rug") %>%
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
      c(5, 5)
    )
})
