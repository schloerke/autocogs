
context("layers")

suppressPackageStartupMessages(library(ggplot2))


test_that("ggplot2 layers", {

  p <- ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
    geom_point() +
    geom_smooth(method = "auto")

  check_plot_count <- function(spec, count) {
    expect_silent({
      pc <- plot_cogs(p, spec = spec)
    })
    expect_equal(length(pc), count)
  }

  check_plot_count(TRUE, 2)
  check_plot_count(c(FALSE, TRUE), 1)
  check_plot_count(c(TRUE, FALSE), 1)
  check_plot_count(c(TRUE, TRUE), 2)

  expect_error({
    plot_cogs(p, spec = FALSE)
  })
  expect_error({
    plot_cogs(p, spec = c(TRUE, TRUE, TRUE))
  })


  check_plot_count(list(cog_spec(), cog_spec()), 2)
  check_plot_count(list(cog_spec(), TRUE), 2)
  check_plot_count(list(cog_spec(), FALSE), 1)
  check_plot_count(list(cog_spec(scagnostics = FALSE), TRUE), 2)
  check_plot_count(list(cog_spec(.keep_layer = FALSE), TRUE), 1)

  expect_equal(
    cog_spec()$remove,
    character(0)
  )
  expect_equal(
    cog_spec(scagnostics = FALSE)$remove,
    c("scagnostics")
  )
  expect_equal(
    cog_spec(scagnostics = TRUE)$remove,
    character(0)
  )

})
