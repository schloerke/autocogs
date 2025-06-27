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
  expect_auto_cogs(
    ggplot(iris, aes(Sepal.Length)) +
      geom_bar(),
    c("_x", "_n"),
    c(5, 2)
  )
})

test_that("ggplot2::geom_col", {
  expect_auto_cogs(
    ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
      geom_col(),
    c("_x", "_y", "_n"),
    c(5, 5, 5)
  )
})

test_that("ggplot2::geom_hex", {
  expect_auto_cogs(
    ggplot(diamonds, aes(carat, price)) + geom_hex(),
    c("_x", "_y", "_bivar", "_hex_bins", "_n"),
    c(5, 5, 2, 6, 5)
  )
})

test_that("ggplot2::geom_bin2d", {
  expect_auto_cogs(
    ggplot(diamonds, aes(carat, price)) + geom_bin2d(),
    c("_x", "_y", "_bivar", "_square_bins", "_n"),
    c(5, 5, 2, 6, 5)
  )
})

test_that("ggplot2::geom_point", {
  expect_auto_cogs(
    ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
      geom_point(),
    c("_x", "_y", "_bivar", "_scagnostic", "_n"),
    c(5, 5, 2, 1, 5)
  )
})

test_that("ggplot2::geom_boxplot", {
  expect_auto_cogs(
    ggplot(iris, aes(x = factor(1), y = Sepal.Length)) +
      geom_boxplot(),
    c("_y", "_boxplot", "_n"),
    c(5, 7, 5)
  )
})

test_that("ggplot2::geom_count", {
  expect_auto_cogs(
    ggplot(mpg, aes(x = cty, y = hwy)) +
      geom_count(),
    c(
      "_x",
      "_y",
      "_x_counts",
      "_y_counts",
      "_bivar",
      "_xy_counts",
      "_scagnostic",
      "_n"
    ),
    c(5, 5, 5, 5, 2, 6, 1, 5)
  )
})

test_that("ggplot2::geom_density", {
  expect_auto_cogs(
    ggplot(mpg, aes(x = cty)) +
      geom_density(),
    c("_x", "_density", "_n"),
    c(5, 6, 2)
  )
})

test_that("ggplot2::geom_violin", {
  expect_auto_cogs(
    ggplot(mpg, aes(x = factor(1), y = cty)) +
      geom_violin(),
    c("_y", "_violin", "_n"),
    c(5, 6, 2)
  )
})

test_that("ggplot2::geom_density_2d", {
  expect_auto_cogs(
    ggplot(diamonds[1:100, ], aes(x = carat, y = price)) +
      geom_density_2d(),
    c("_x", "_y", "_bivar", "_density_x", "_density_y", "_density2d", "_n"),
    c(5, 5, 2, 6, 6, 4, 5)
  )

  expect_auto_cogs(
    ggplot(diamonds[1:1001, ], aes(x = carat, y = price)) +
      geom_density_2d(),
    c("_x", "_y", "_bivar", "_density_x", "_density_y", "_density2d", "_n"),
    c(5, 5, 2, 6, 6, 4, 5)
  )
})

test_that("ggplot2::geom_dotplot", {
  expect_auto_cogs(
    ggplot(mpg, aes(x = cty)) +
      geom_dotplot(),
    c("_x", "_density_x", "_n"),
    c(5, 6, 2)
  )
})

test_that("ggplot2::geom_histogram", {
  expect_auto_cogs(
    ggplot(iris, aes(x = Sepal.Length)) +
      geom_histogram(),
    c("_x", "_density_x", "_hist_x", "_n"),
    c(5, 6, 8, 2)
  )
})

test_that("ggplot2::geom_rug", {
  expect_auto_cogs(
    ggplot(mpg, aes(x = cty, y = hwy)) +
      geom_rug(),
    c("_x", "_y", "_density_x", "_density_y", "_n_x", "_n_y"),
    c(5, 5, 6, 6, 2, 2),
    debug = TRUE
  )

  expect_auto_cogs(
    ggplot(mpg, aes(x = cty, y = hwy)) +
      geom_rug(sides = "ltrb"),
    c("_x", "_y", "_density_x", "_density_y", "_n_x", "_n_y"),
    c(5, 5, 6, 6, 2, 2)
  )

  expect_auto_cogs(
    ggplot(mpg, aes(x = cty, y = hwy)) +
      geom_rug(sides = "r"),
    c("_y", "_density_y", "_n"),
    c(5, 6, 2)
  )

  expect_auto_cogs(
    ggplot(mpg, aes(x = cty, y = hwy)) +
      geom_rug(sides = "t"),
    c("_x", "_density_x", "_n"),
    c(5, 6, 2)
  )
})

test_that("ggplot2::geom_spoke", {
  expect_auto_cogs(
    ggplot(
      iris,
      aes(
        x = Sepal.Length,
        y = Sepal.Width,
        radius = Petal.Length,
        angle = Petal.Width
      )
    ) +
      geom_spoke(),
    c(
      "_x",
      "_density_x",
      "_y",
      "_density_y",
      "_angle",
      "_density_angle",
      "_radius",
      "_density_radius",
      "_n_xy",
      "_n_angle_radius"
    ),
    c(
      5,
      6,
      5,
      6,
      5,
      6,
      5,
      6,
      5,
      5
    )
  )

  expect_auto_cogs(
    ggplot(
      iris,
      aes(x = Sepal.Length, y = Sepal.Width, radius = 1, angle = Petal.Width)
    ) +
      geom_spoke(),
    c(
      "_x",
      "_density_x",
      "_y",
      "_density_y",
      "_angle",
      "_density_angle",
      "_radius",
      "_n_xy",
      "_n_angle_radius"
    ),
    c(
      5,
      6,
      5,
      6,
      5,
      6,
      5,
      5,
      5
    )
  )

  expect_auto_cogs(
    ggplot(
      iris,
      aes(x = Sepal.Length, y = Sepal.Width, radius = Petal.Length, angle = 1)
    ) +
      geom_spoke(),
    c(
      "_x",
      "_density_x",
      "_y",
      "_density_y",
      "_angle",
      "_radius",
      "_density_radius",
      "_n_xy",
      "_n_angle_radius"
    ),
    c(
      5,
      6,
      5,
      6,
      5,
      5,
      6,
      5,
      5
    )
  )
})


test_that("ggplot2::geom_qq", {
  expect_auto_cogs(
    ggplot(iris, aes(sample = Sepal.Length)) +
      geom_qq(),
    c("_x", "_qq"),
    c(5, 4)
  )
})


test_that("ggplot2::geom_smooth", {
  expect_auto_cogs(
    ggplot(mpg, aes(x = cty, y = hwy)) +
      geom_smooth(),
    c("_x", "_y", "_bivar", "_smooth", "_n"),
    c(5, 5, 2, 3, 5)
  )
})


test_that("ggplot2::geom_smooth_loess", {
  p <- ggplot(mpg, aes(x = cty, y = hwy)) +
    geom_smooth(method = "loess")

  expect_auto_cogs(
    p,
    c("_x", "_y", "_bivar", "_smooth", "_loess", "_n"),
    c(5, 5, 2, 3, 6, 5)
  )

  cogs <- plot_cogs(p)
  expect_equal(cogs[[1]]$"_loess"$span[1], 0.75)
  expect_equal(cogs[[1]]$"_loess"$degree[1], 2)

  p <- ggplot(mpg, aes(x = cty, y = hwy)) +
    geom_smooth(method = "loess", span = 0.9, method.args = list(degree = 1))

  expect_auto_cogs(
    p,
    c("_x", "_y", "_bivar", "_smooth", "_loess", "_n"),
    c(5, 5, 2, 3, 6, 5)
  )

  cogs <- plot_cogs(p)
  expect_equal(cogs[[1]]$"_loess"$span[1], 0.9)
  expect_equal(cogs[[1]]$"_loess"$degree[1], 1)
})


test_that("ggplot2::geom_smooth_lm", {
  p <- ggplot(mpg, aes(x = cty, y = hwy)) +
    geom_smooth(method = "lm")

  expect_auto_cogs(
    p,
    c("_x", "_y", "_bivar", "_smooth", "_lm", "_n"),
    c(5, 5, 2, 3, 19, 5)
  )
})


test_that("ggplot2::geom_line", {
  p <- ggplot(mpg, aes(x = cty, y = hwy)) +
    geom_line()

  expect_auto_cogs(
    p,
    c("_x", "_y", "_bivar", "_n"),
    c(5, 5, 2, 5)
  )
})

test_that("ggplot2::geom_path", {
  p <- ggplot(mpg, aes(x = cty, y = hwy)) +
    geom_path()

  expect_auto_cogs(
    p,
    c("_x", "_y", "_bivar", "_n"),
    c(5, 5, 2, 5)
  )
})

test_that("ggplot2::geom_step", {
  p <- ggplot(mpg, aes(x = cty, y = hwy)) +
    geom_step()

  expect_auto_cogs(
    p,
    c("_x", "_y", "_step", "_bivar", "_n"),
    c(5, 5, 11, 2, 5)
  )
})
