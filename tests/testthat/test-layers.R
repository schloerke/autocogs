context("layers")

test_that("ggplot2 layers", {

  p <- qplot(Sepal.Length, Sepal.Width, data = iris) +
    geom_smooth(method = "lm")

  expect_silent({
    ignore <- calculate_auto_cogs(p, layers = TRUE)
    ignore <- calculate_auto_cogs(p, layers = c(FALSE, TRUE))
    ignore <- calculate_auto_cogs(p, layers = c(TRUE, FALSE))
    ignore <- calculate_auto_cogs(p, layers = c(TRUE, TRUE))
  })

  expect_error({
    calculate_auto_cogs(p, layers = FALSE)
  })
  expect_error({
    calculate_auto_cogs(p, layers = c(TRUE, TRUE, TRUE))
  })
})
