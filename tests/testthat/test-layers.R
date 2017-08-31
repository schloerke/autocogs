
context("layers")

library(ggplot2)


test_that("ggplot2 layers", {

  p <- qplot(Sepal.Length, Sepal.Width, data = iris) +
    geom_smooth(method = "lm")

  expect_silent({
    ignore <- plot_cogs(p, layers = TRUE)
    ignore <- plot_cogs(p, layers = c(FALSE, TRUE))
    ignore <- plot_cogs(p, layers = c(TRUE, FALSE))
    ignore <- plot_cogs(p, layers = c(TRUE, TRUE))
  })

  expect_error({
    plot_cogs(p, layers = FALSE)
  })
  expect_error({
    plot_cogs(p, layers = c(TRUE, TRUE, TRUE))
  })
})
