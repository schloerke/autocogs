#' @include plot_fn.R
NULL




#   geom_bar geom_col stat_count
#     - Bars charts
#     * univariate_counts(x)
#     ** grouped_counts(counts...)
#     ** grouped_testing(x...)
add_plot_cog(
  # load_all(); p <- qplot(Sepal.Length, data = iris, geom = "bar"); print(p); calculate_auto_cogs(p)
  "geom_bar",
  "bar plot. (height is calculated)",
  tribble(
    ~ auto_cog, ~ cols, ~ store_name,
    "univariate_continuous", "x", "_x",
    "univariate_counts", "x", "_n"
  )
)
add_plot_cog(
  # load_all(); p <- qplot(Sepal.Length, Sepal.Width, data = iris, geom = "col"); print(p); calculate_auto_cogs(p)
  "geom_col",
  "bar plot with known height",
  tribble(
    ~ auto_cog, ~ cols, ~ store_name,
    "univariate_continuous", "x", "_x",
    "bivariate_counts", c("x", "y"), "_n"
  )
)


#   geom_hex stat_bin_hex
#     - Hexagonal heatmap of 2d bin counts
#     * bivariate_counts(x, y)
#     * chi_square_test(bin_counts)
add_plot_cog(
  # load_all(); p <- qplot(carat, price, data = diamonds, geom = "hex"); print(p); calculate_auto_cogs(p)
  "geom_hex",
  "hexegon density plot",
  tribble(
    ~ auto_cog, ~ cols, ~ store_name,
    "univariate_continuous", "x", "_x",
    "univariate_continuous", "y", "_y",
    "bivariate_continuous", c("x", "y"), "_bivar",
    "hex_counts", c("x", "y"), "_hex_bins",
    "bivariate_counts", c("x", "y"), "_n"
  )
)
#   geom_bin2d stat_bin_2d
#     - Heatmap of 2d bin counts
#     * bivariate_counts(x, y)
#     * chi_square_test(bin_counts)
add_plot_cog(
  # load_all(); p <- qplot(carat, price, data = diamonds, geom = "bin2d"); print(p); calculate_auto_cogs(p)
  "geom_bin2d",
  "hexegon density plot",
  tribble(
    ~ auto_cog, ~ cols, ~ store_name,
    "square_counts", c("x", "y"), "_square_bins",
    "bivariate_counts", c("x", "y"), "_n"
  )
)


#   geom_point
#     - Points
#     * univariate_continuous(x)
#     * univariate_continuous(y)
#     * bivariate_continuous(x,y)
#     * bivariate_counts(x,y)
#     * bivariate_scagnostics(x,y)
#     ** grouped_counts(counts...)
#     ** grouped_testing(y...)
#   geom_jitter
#     - Jittered points
#     * univariate_continuous(x)
#     * univariate_continuous(y)
#     * bivariate_continuous(x,y)
#     * bivariate_counts(x,y)
#     ** grouped_counts(counts...)
#     ** grouped_testing(y...)
add_plot_cog(
  # load_all(); p <- qplot(Sepal.Length, Sepal.Width, data = iris); calculate_auto_cogs(p)
  "geom_point",
  "scatter plot points",
  tribble(
    ~ auto_cog, ~ cols, ~ store_name,
    "univariate_continuous", "x", "_x",
    "univariate_continuous", "y", "_y",
    "bivariate_continuous", c("x","y"), "_bivar",
    "scagnostics", c("x","y"), "_scagnostic",
    "bivariate_counts", c("x","y"), "_n"
  )
)


#   geom_boxplot stat_boxplot
#     - A box and whiskers plot (in the style of Tukey)
#     * univariate_continuous(x)
#     * univariate_boxpot(x)
#     ^ univariate_counts(x)
#     ** grouped_counts(counts...)
#     ** grouped_testing(x...)
#     ** ('n_outlier_lower', 'count of lower outliers')
#     ** ('min_non_outlier', 'minimum point')
#     ** ('max_non_outlier', 'maximum non outlier point')
#     ** ('n_outlier_upper', 'count of upper outliers')
add_plot_cog(
  # load_all(); p <- qplot(x = 1, y = Sepal.Length, data = iris, geom = "boxplot"); calculate_auto_cogs(p)
  "geom_boxplot",
  "boxplot plot",
  tribble(
    ~ auto_cog, ~ cols, ~ store_name,
    "univariate_continuous", "y", "_y",
    "boxplot_metrics", "y", "_boxplot",
    "univariate_counts", "x", "_counts"
  )
)


#   geom_count stat_sum
#     - Count overlapping points
#     * expected count(x)
#     * bivariate_counts(x, y)
#     ** grouped_counts(counts...)
add_plot_cog(
  # load_all(); p <- qplot(cty, hwy, data = mpg, geom = "count"); calculate_auto_cogs(p)
  "geom_count",
  "count of discrete scatter plot points",
  tribble(
    ~ auto_cog, ~ cols, ~ store_name,
    "univariate_continuous", "x", "_x",
    "univariate_continuous", "y", "_y",
    "univariate_discrete", "x", "_x_counts",
    "univariate_discrete", "y", "_y_counts",
    "bivariate_continuous", c("x","y"), "_bivar",
    "pairwise_counts", c("x","y"), "_xy_counts",
    "scagnostics", c("x","y"), "_scagnostic",
    "bivariate_counts", c("x","y"), "_n"
  )
)


#   geom_density stat_density
#     - Smoothed density estimates
#     * ('max_density', 'maximum density value')
#     * univariate_continuous(x)
#     ** grouped_testing(x...)
#     ** grouped_counts(counts...)
add_plot_cog(
  # load_all(); p <- qplot(cty, data = mpg, geom = "density"); calculate_auto_cogs(p)
  "geom_density",
  "density of a single variable",
  tribble(
    ~ auto_cog, ~ cols, ~ store_name,
    "univariate_continuous", "x", "_x",
    "density_continuous", "x", "_density",
    "univariate_counts", "x", "_n"
  )
)
#   geom_violin stat_ydensity
#     - Violin plot
#     * ('max_density', 'maximum density value')
#     * univariate_continuous(x)
#     ** grouped_testing(x...)
#     ** grouped_counts(counts...)
add_plot_cog(
  # load_all(); p <- qplot(1, cty, data = mpg, geom = "violin"); calculate_auto_cogs(p)
  "geom_violin",
  "density of a single y variable",
  tribble(
    ~ auto_cog, ~ cols, ~ store_name,
    "univariate_continuous", "y", "_y",
    "density_continuous", "y", "_density",
    "univariate_counts", "y", "_n"
  )
)

#   geom_density_2d stat_density_2d
#     - Contours of a 2d density estimate
#     * max density(x, y)
#     * bivariate_continuous(x,y)
#     ** grouped_counts(counts...)
#     ** grouped_testing(y...)
add_plot_cog(
  # load_all(); p <- qplot(carat, price, data = diamonds, geom = "density_2d"); calculate_auto_cogs(p)
  "geom_density2d",
  "Contours of a 2d density estimate. Similar to a topographical map",
  tribble(
    ~ auto_cog, ~ cols, ~ store_name,
    "univariate_continuous", "x", "_x",
    "univariate_continuous", "y", "_y",
    "bivariate_continuous", c("x","y"), "_bivar",
    "density_continuous", c("x"), "_density_x",
    "density_continuous", c("y"), "_density_y",
    "density_2d_continuous", c("x", "y"), "_density2d",
    "bivariate_counts", c("x","y"), "_n"
  )
)



#   geom_dotplot
#     - Dot plot
#     * univariate_continuous(x)
#     * univariate_counts(x)
#     ** grouped_counts(counts...)
#     ** grouped_testing(x...)
add_plot_cog(
  # load_all(); p <- qplot(cty, data = mpg, geom = "dot"); calculate_auto_cogs(p)
  # TODO could be done better
  "geom_dotplot",
  "stacked dots where each dot represents an observation.  The width of the dot represents the bin width.",
  tribble(
    ~ auto_cog, ~ cols, ~ store_name,
    "univariate_continuous", "x", "_x",
    "density_continuous", c("x"), "_density_x",
    "univariate_counts", c("x"), "_n"
  )
)


#   geom_freqpoly geom_histogram stat_bin
#     - Histograms and frequency polygons
#     * univariate_continuous(x)
#     * univariate_counts(x)
#     * univariate_continuous(counts)
#     ** grouped_counts(counts...)
#     ** grouped_testing(x...)
add_plot_cog(
  # load_all(); p <- qplot(cty, data = mpg, geom = "histogram"); calculate_auto_cogs(p)
  "geom_histogram",
  "histogram of continuous data",
  tribble(
    ~ auto_cog, ~ cols, ~ store_name,
    "univariate_continuous", "x", "_x",
    "density_continuous", c("x"), "_density_x",
    "histogram_counts", c("x"), "_hist_x",
    "univariate_counts", c("x"), "_n"
  )
)


#   geom_rug
#     - Rug plots in the margins
#     * univariate_continuous(x)
#     * univariate_counts(x)
#     ** grouped_counts(counts...)
#     ** grouped_testing(x...)
add_plot_cog(
  # load_all(); p <- qplot(cty, data = mpg, geom = "rug"); calculate_auto_cogs(p)
  "geom_rug",
  "Rug plots in the margins",
  tribble(
    ~ auto_cog, ~ cols, ~ store_name,
    "univariate_continuous", "x", "_x",
    "density_continuous", c("x"), "_density_x",
    "univariate_counts", c("x"), "_n"
  )
)



#   geom_spoke
#     - Line segments parameterised by location, direction and distance
#     * univariate_continuous(angle) | angle is aes
#     * univariate_continuous(spoke) | spoke is aes
#     ** grouped_counts(counts...)
#     ** grouped_testing(angle...)
#     ** grouped_testing(spoke...)
add_plot_cog(
  # load_all(); p <- qplot(cty, data = mpg, geom = "rug"); calculate_auto_cogs(p)
  "geom_rug",
  "Rug plot of continuous data",
  tribble(
    ~ auto_cog, ~ cols, ~ store_name,
    "univariate_continuous", "x", "_x",
    "density_continuous", c("x"), "_density_x",
    "univariate_counts", c("x"), "_n"
  )
)


#   geom_qq stat_qq
#     - A quantile-quantile plot
#     * sum of delta^2 | above
#     * sum of delta^2 | below
#     * sum of delta^2
#     * KS test (x, dist)
add_plot_cog(
  # load_all(); p <- qplot(cty, data = mpg, geom = "rug"); calculate_auto_cogs(p)
  "geom_qq",
  "Quantile-Quantile plot of continuous data",
  tribble(
    ~ auto_cog, ~ cols, ~ store_name,
    "univariate_continuous", "x", "_x",
    "quantile_quantile", "x", "_qq"
  )
)


#   geom_quantile stat_quantile
#     - Quantile regression
#     ^ contains N points
#     ^ contains N points above
#     ^ contains N points below
#     ^ integral of area
#     ** contains N points in group
#     ** contains N points above in group
#     ** contains N points below in group
#     ** integral of area in group
#     ** grouped_counts(counts...)
#     ** grouped_testing(y...)



# Geoms
#   geom_smooth stat_smooth
#     - Smoothed conditional means
#     * bivariate_counts(x,y)
#     ^ linear_model(x, y)
#     ** linear_model(x, y, group)
#     ** group variable significance (intercept)
#     ** group and x interaction significance (slope)
#     ** grouped_counts(counts...)
add_plot_cog(
  # load_all(); p <- qplot(cty, hwy, data = mpg, geom = "smooth"); calculate_auto_cogs(p)
  "geom_smooth",
  "Smooth line plot of continuous x-y data",
  tribble(
    ~ auto_cog, ~ cols, ~ store_name,
    "univariate_continuous", "x", "_x",
    "univariate_continuous", "y", "_y",
    "bivariate_continuous", c("x", "y"), "_bivar",
    "smooth_line", c("x", "y"), "_smooth",
    "bivariate_counts", c("x", "y"), "_n"
  )
)
add_plot_cog(
  # load_all(); p <- qplot(cty, hwy, data = mpg, geom = "smooth", method = "loess", span = 0.9, method.args = list(degree = 1)); calculate_auto_cogs(p)
  "geom_smooth_loess",
  "Loess plot of continuous x-y data",
  tribble(
    ~ auto_cog, ~ cols, ~ store_name,
    "univariate_continuous", "x", "_x",
    "univariate_continuous", "y", "_y",
    "bivariate_continuous", c("x", "y"), "_bivar",
    "smooth_line", c("x", "y"), "_smooth",
    "loess_model", c("x", "y"), "_loess",
    "bivariate_counts", c("x", "y"), "_n"
  )
)

add_plot_cog(
  # load_all(); p <- qplot(cty, hwy, data = mpg, geom = "smooth", method = "lm"); calculate_auto_cogs(p)
  "geom_smooth_lm",
  "Smooth line plot of continuous x-y data",
  tribble(
    ~ auto_cog, ~ cols, ~ store_name,
    "univariate_continuous", "x", "_x",
    "univariate_continuous", "y", "_y",
    "bivariate_continuous", c("x", "y"), "_bivar",
    "smooth_line", c("x", "y"), "_smooth",
    "linear_model", c("x", "y"), "_lm",
    "bivariate_counts", c("x", "y"), "_n"
  )
)


# Maybe Nothing?
#   geom_contour stat_contour
#     - 2d contours of a 3d surface
#   geom_path geom_line geom_step
#     - Connect observations
#   geom_raster geom_rect geom_tile
#     - Rectangles
#
# Nothing
#   geom_abline geom_hline geom_vline
#     - Reference lines: horizontal, vertical, and diagonal
#   geom_blank
#     - Draw nothing
#   geom_errorbarh
#     - Horizontal error bars
#   geom_crossbar geom_errorbar geom_linerange geom_pointrange
#     - Vertical intervals: lines, crossbars & errorbars
#   geom_map
#     - Polygons from a reference map
#   geom_polygon
#     - Polygons
#   geom_ribbon geom_area
#     - Ribbons and area plots
#   geom_segment geom_curve
#     - Line segments and curves
#   geom_label geom_text
#     - Text
#   stat_sf geom_sf coord_sf
#     - Visualise sf objects