#' @include add_cog_group.R field_info.R
NULL



add_cog_group(
  "univariate_continuous",
  field_info("x", "continuous"),
  "univariate metrics for continuous data",
  # function(data, mapping) {
  #   str_c("_univar_", mapping$x)
  # },
  function(x, ...) {
    if (is.null(x)) return(NULL)

    x_range <- range(x, na.rm = TRUE)
    list(
      min = cog_desc(x_range[1], "minimum of non NA data"),
      max = cog_desc(x_range[2], "maximum of non NA data"),
      mean = cog_desc(mean(x, na.rm = TRUE), "mean of non NA data"),
      median = cog_desc(median(x, na.rm = TRUE), "median of non NA data"),
      var = cog_desc(var(x, na.rm = TRUE), "variance of non NA data")
    )
  }
)


add_cog_group(
  "univariate_discrete",
  field_info("x", "discrete"),
  "univariate metrics for discrete data",
  function(x, ...) {
    if (is.null(x)) return(NULL)

    table_counts <- x %>% table() %>% sort()
    count_names <- names(table_counts)
    n_groups <- length(table_counts)
    list(
      min_name = cog_desc(count_names[1], "minimum group count name"),
      count_min = cog_desc(table_counts[1], "minimum group count"),
      count_mean = cog_desc(sum(table_counts) / n_groups, "average group count"),
      count_max = cog_desc(table_counts[n_groups], "maximum group count"),
      max_name = cog_desc(count_names[n_groups], "maximum group count name")
    )
  }
)


add_cog_group(
  "boxplot",
  field_info("x", "continuous"),
  "univariate boxplot metrics for continuous data",
  function(x, ...) {
    if (is.null(x)) return(NULL)

    boxplot_info <- ggplot2::StatBoxplot$compute_group(
      data.frame(x = 1, y = x), scales = NULL, width = 1, na.rm = FALSE, coef = 1.5
    )
    median <- boxplot_info$middle[[1]]
    outliers <- boxplot_info$outliers[[1]]
    list(
      "n_outlier_lower" = cog_desc(sum(outliers < median), "count of lower outliers"),
      "lower_whisker" = cog_desc(boxplot_info$lower[[1]], "minimum point"),
      "q1" = cog_desc(boxplot_info$notchlower[[1]], "first quartile value"),
      "median" = cog_desc(median, "median value"),
      "q3" = cog_desc(boxplot_info$notchupper[[1]], "third quartile value"),
      "upper_whisker" = cog_desc(boxplot_info$upper[[1]], "maximum non outlier point"),
      "n_outlier_upper" = cog_desc(sum(outliers > median), "count of upper outliers")
    )
  }
)


add_cog_group(
  "bivariate_continuous",
  bind_rows(
    field_info("x", "continuous"),
    field_info("y", "continuous")
  ),
  "continuous bivariate metrics",
  function(x, y, ...) {
    if (is.null(x)) return(NULL)
    if (is.null(y)) return(NULL)
    x <- as.numeric(x)
    y <- as.numeric(y)

    list(
      "covariance" = cog_desc(cov(x, y, use = "na.or.complete"), "covariance of non na pairs"),
      "correlation" = cog_desc(cor(x, y, use = "na.or.complete"), "correlation of non na pairs")
    )
  }
)


# #' @import scagnostics
#' @importFrom stats cor
add_cog_group(
  "scagnostics",
  bind_rows(
    field_info("x", "continuous"),
    field_info("y", "continuous")
  ),
  "scagnostics of bivariate continuous data",
  function(x, y, ...) {
    if (is.null(x)) return(NULL)
    if (is.null(y)) return(NULL)
    if (!is.numeric(x)) return(NULL)
    if (!is.numeric(y)) return(NULL)

    # info <- as.list(scagnostics(x, y))
    info <- list(monotonic = stats::cor(x, y, method = "spearman") ^ 2)
    list(
      # Outlying = cog_desc(info$Outlying, "proportion of the total edge length due to extremely long edges connected to points of single degree"),
      # Skewed = cog_desc(info$Skewed, "distribution of edge lengths of a minimum spanning tree gives us information about the relative density of points in a scattered configuration"),
      # Clumpy = cog_desc(info$Clumpy, "the Hartigan and Mohanty RUNT statistic"),
      # Sparse = cog_desc(info$Sparse, "the 90% quantile of the edge lengths of the MST"),
      # Striated = cog_desc(info$Striated, "sumation of angles over all adjacent edges of a MST"),
      # Convex = cog_desc(info$Convex, "ratio of the area of the alpha hull and the area of the convex hull"),
      # Skinny = cog_desc(info$Skinny, "ratio of perimeter to area of a polygon measures, roughly, how skinny it is"),
      # Stringy = cog_desc(info$Stringy, "the ratio of width to length of a network"),
      monotonic = cog_desc(info$monotonic, "squared Spearman correlation coefficient, which is a Pearson correlation on the ranks of x and y")
    )
  }
)


add_cog_group(
  "univariate_counts",
  bind_rows(
    field_info("x", "any")
  ),
  "univariate count information",
  function(x, ...) {
    n <- length(x)
    x_is_na <- is.na(x)
    list(
      "n" = cog_desc(n, "count of non NA data"),
      "n_na" = cog_desc(sum(x_is_na), "count of NA data")
    )
  }
)
add_cog_group(
  "bivariate_counts",
  bind_rows(
    field_info("x", "any"),
    field_info("y", "any")
  ),
  "bivariate count information",
  function(x, y, ...) {
    n <- length(x)
    x_is_na <- is.na(x)
    y_is_na <- is.na(y)
    list(
      "n" = cog_desc(n, "count of non NA data"),
      "n_both_na" = cog_desc(sum(x_is_na & y_is_na), "count of data where both X and Y are NA"),
      "n_or_na" = cog_desc(sum(x_is_na | y_is_na), "count of data where either X or Y are NA"),
      "n_x_na" = cog_desc(sum(x_is_na), "count of X NA data"),
      "n_y_na" = cog_desc(sum(y_is_na), "count of Y NA data")
    )
  }
)


add_cog_group(
  "hex_counts",
  bind_rows(
    field_info("x", "continuous"),
    field_info("y", "continuous")
  ),
  "hexagon count information",
  function(x, y, ..., bins = 30) {
    n <- length(x)
    x_is_na <- is.na(x)
    dt <- data.frame(x = x, y = y, weight = 1, PANEL = 1)
    binwidth <- c(
      diff(range(x, na.rm = TRUE)) / bins,
      diff(range(y, na.rm = TRUE)) / bins
    )
    hex_info <- ggplot2::StatBinhex$compute_group(dt, NULL, binwidth)
    counts <- hex_info$count
    list(
      count_min = cog_desc(min(counts), "minimum count"),
      count_max = cog_desc(max(counts), "maximum count"),
      count_mean = cog_desc(mean(counts), "mean (expected) count"),
      count_median = cog_desc(median(counts), "median count"),
      count_var = cog_desc(var(counts), "variance of counts"),
      "chisq" = cog_desc(chisq.test(counts)$p.value, "p-value of chi square test of counts. Low value means not statistically 'uniform' counts")
    )
  }
)
add_cog_group(
  "square_counts",
  bind_rows(
    field_info("x", "continuous"),
    field_info("y", "continuous")
  ),
  "square count information",
  function(x, y, ..., breaks = NULL, origin = NULL, binwidth = NULL, bins = 30, drop = TRUE) {
    n <- length(x)
    x_is_na <- is.na(x)
    dt <- data.frame(x = x, y = y, weight = 1, PANEL = 1)
    scales <- list(
      x = ggplot2::ScaleContinuous$clone(),
      y = ggplot2::ScaleContinuous$clone()
    )
    scales$x$train(dt$x)
    scales$y$train(dt$y)
    bin_dt <- ggplot2::StatBin2d$compute_group(dt, scales,
      binwidth = binwidth, bins = bins,
      breaks = breaks, origin = origin, drop = drop
    )
    counts <- bin_dt$count
    list(
      count_min = cog_desc(min(counts), "minimum count"),
      count_max = cog_desc(max(counts), "maximum count"),
      count_mean = cog_desc(mean(counts), "mean (expected) count"),
      count_median = cog_desc(median(counts), "median count"),
      count_var = cog_desc(var(counts), "variance of counts"),
      "chisq" = cog_desc(chisq.test(counts)$p.value, "p-value of chi square test of counts. Low value means not statistically 'uniform' counts")
    )
  }
)


# need to import the BIC function as it's called internally in Mclust()
#' @importFrom mclust Mclust mclustBIC
add_cog_group(
  "density_continuous",
  field_info("x", "continuous"),
  "density plot information",
  function(
    x, ...,
    # StatDensity parameters
    bw = "nrd0", adjust = 1, kernel = "gaussian", n = 512, trim = FALSE, na.rm = FALSE,
    clusters = FALSE
  ) {

    n <- length(x)
    x_is_na <- is.na(x)
    dt <- data.frame(x)

    if (length(unique(dt$x)) == 1) {
      return(NULL)
    }

    scales <- list(
      x = ggplot2::ScaleContinuous$clone()
    )
    scales$x$train(dt$x)

    ret <- ggplot2::StatDensity$compute_group(
      dt, scales,
      bw = bw, adjust = adjust, kernel = kernel, n = n, trim = trim, na.rm = na.rm
    )

    # density_slopes = (ret$density[-1] - ret$density[-nrow(ret)]) - (ret$x[-1] - ret$x[-nrow(ret)])
    # max_slope = max(abs(density_slopes))

    # x_range <- range(ret$x)
    # mse <- mean((ret$density - 1 / (x_range[2] - x_range[1]))^2)

    # silvermans
    # reference: https://stats.stackexchange.com/questions/138223/how-to-test-if-my-distribution-is-multimodal
    # non cran package: https://www.mathematik.uni-marburg.de/~stochastik/R_packages/

    max_density <- max(ret$density)
    max_density_location <- ret$x[which(ret$density == max_density)[1]]

    list(
      max_density = cog_desc(max_density, "maximum density height"),
      max_density_location = cog_desc(max_density_location, "location of maximum density height"),
      # max_density_slope = cog_desc(max_slope, "maximum absolute value density slope"),
      # uniform_mse = cog_desc(mse, "mean squared error compared to uniform")
      clusters = cog_desc(
        ifelse(
          isTRUE(clusters),
          mclust::Mclust(dt[, c("x")], verbose = FALSE)$G,
          NA
        ),
        "optimal number of components found using Model-Based Clustering. Cluster value is NA if there are more than 1000 points in subset"
      ),
      unimodal_p_value = cog_desc(
        diptest::dip.test(dt$x)$p.value,
        "Hartigans' dip test for unimodality / multimodality. (Low p-value means non-unimodal density)"
      ),
      skew = cog_desc(moments::skewness(x, na.rm = TRUE), "skewness of non NA data"),
      kurt = cog_desc(moments::kurtosis(x, na.rm = TRUE), "kurtosis of non NA data")
    )
  }
)


add_cog_group(
  "density_2d_continuous",
  bind_rows(
    field_info("x", "continuous"),
    field_info("y", "continuous")
  ),
  "2d density plot information. (Displays contour curves)",
  function(
    x, y, ...,
    # StatDensity2d parameters
    na.rm = FALSE, h = NULL,
    # contour = TRUE,
    n = 100, bins = NULL, binwidth = NULL,
    clusters = FALSE
  ) {

    n <- length(x)
    dt <- data.frame(x = x, y = y, group = 1, PANEL = 1)

    scales <- list(
      x = ggplot2::ScaleContinuous$clone(),
      y = ggplot2::ScaleContinuous$clone()
    )
    scales$x$train(dt$x)
    scales$y$train(dt$y)

    ret <- ggplot2::StatDensity2d$compute_group(
      dt, scales,
      na.rm = na.rm, h = h, contour = FALSE, n = n, bins = bins, binwidth = binwidth
    )

    max_density <- max(ret$density)
    max_row <- which(ret$density == max_density)[1]
    max_density_x <- ret$x[max_row]
    max_density_y <- ret$y[max_row]

    list(
      max_density = cog_desc(max_density, "maximum density height"),
      max_density_x = cog_desc(max_density_x, "X location of maximum density height"),
      max_density_y = cog_desc(max_density_y, "Y location of maximum density height"),
      # max_density_slope = cog_desc(max_slope, "maximum absolute value density slope"),
      # uniform_mse = cog_desc(mse, "mean squared error compared to uniform"),
      clusters = cog_desc(
        ifelse(
          isTRUE(clusters),
          mclust::Mclust(dt[, c("x", "y")], verbose = FALSE)$G,
          NA
        ),
        "optimal number of components found using Model-Based Clustering. Cluster value is NA if there are more than 1000 points in subset"
      )
    )
  }
)



add_cog_group(
  "histogram_counts",
  bind_rows(
    field_info("x", "continuous")
  ),
  "histogram count information",
  function(
    x, ...,
    # StatBin parameters
    binwidth = NULL, bins = 30, center = NULL,
    boundary = NULL, closed = c("right", "left"), pad = FALSE,
    breaks = NULL
  ) {

    n <- length(x)
    dt <- data.frame(x = x, PANEL = 1)

    scales <- list(
      x = ggplot2::ScaleContinuous$clone()
    )
    scales$x$train(dt$x)

    params <- list(
      binwidth = binwidth, bins = bins, center = center,
      boundary = boundary, closed = closed, pad = pad,
      breaks = breaks
    )
    params <- ggplot2::StatBin$setup_params(dt, params)
    ret <- do.call(ggplot2::StatBin$compute_group, append(list(dt, scales), params))

    counts <- ret$count

    list(
      binwidth = cog_desc(
        ifelse(length(params$binwidth) == 1, params$binwidth, NA),
        "binwidth of histogram"
      ),
      bins = cog_desc(
        ifelse(length(params$bins) == 1, params$bins, NA),
        "number of bins"
      ),
      count_min = cog_desc(min(counts), "minimum count"),
      count_max = cog_desc(max(counts), "maximum count"),
      count_mean = cog_desc(mean(counts), "mean (expected) count"),
      count_median = cog_desc(median(counts), "median count"),
      count_var = cog_desc(var(counts), "variance of counts"),
      "chisq" = cog_desc(chisq.test(counts)$p.value, "p-value of chi square test of counts. Low value means not statistically 'uniform' counts")
    )
  }
)


add_cog_group(
  "pairwise_counts",
  bind_rows(
    field_info("x", "any"),
    field_info("y", "any")
  ),
  "count information for grouped data",
  function(x, y, ...) {
    dt <- tibble(x, y)

    counts <- dt %>% group_by(x, y) %>% count()

    is_na_xy <- is.na(counts$x) | is.na(counts$y)

    if (any(is_na_xy)) {
      na_count <- counts$n[is_na_xy]
      counts <- counts %>% filter(!is_na_xy)
    } else {
      na_count <- 0
    }

    count_values <- counts$n

    list(
      min = cog_desc(min(count_values), "minimum occurances of an x,y pair"),
      max = cog_desc(max(count_values), "maximum occurances of an x,y pair"),
      mean = cog_desc(mean(count_values), "mean occurances of an x,y pair"),
      median = cog_desc(median(count_values), "median occurances of an x,y pair"),
      var = cog_desc(var(count_values), "variance of occurances of an x,y pair"),
      na_count = cog_desc(na_count, "count of NA occurances of a x,y pair")
    )
  }
)


add_cog_group(
  "grouped_counts",
  bind_rows(
    field_info("x", "continuous"),
    field_info("group", "any")
  ),
  "count information for grouped data",
  function(x, group, ...) {
    tibble(x, group) %>%
      group_by(group) %>%
      count() ->
    counts

    na_group <- is.na(counts$group)
    if (any(na_group)) {
      na_count <- counts$n[na_group]
      counts <- counts %>% filter(!na_group)
    } else {
      na_count <- 0
    }

    count_values <- counts$n

    list(
      min = cog_desc(min(count_values), "minimum occurances of a group"),
      max = cog_desc(max(count_values), "maximum occurances of a group"),
      mean = cog_desc(mean(count_values), "mean occurances of a group"),
      median = cog_desc(median(count_values), "median occurances of a group"),
      var = cog_desc(var(count_values), "variance of occurances of a group"),
      na_count = cog_desc(na_count, "count of NA occurances of the grouping variable")
    )
  }
)


add_cog_group(
  "grouped_testing",
  bind_rows(
    field_info("x", "continuous"),
    field_info("group", "continuous")
  ),
  "test differences between univariate groups",
  fn = function(x, groups, ...) {
    mod <- lm(x ~ groups)
    an <- anova(mod)

    an_broom <- broom::tidy(an)

    ao <- aov(x ~ groups)
    tukey <- broom::tidy(TukeyHSD(ao))
    num_sig_diff <- sum(tukey$conf.low > 0 | tukey$conf.high < 0)

    list(
      anova_model_df = cog_desc(an_broom$df[1], "ANOVA model degrees of freedom"),
      anova_model_df_resid = cog_desc(an_broom$df[2], "ANOVA residual degrees of freedom"),
      anova_f_value = cog_desc(an_broom$statistic[1], "ANOVA F value"),
      anova_p_value = cog_desc(an_broom$p.value[1], "ANOVA p value"),
      sig_diff_pairs = cog_desc(num_sig_diff, "Number of significantly different groups according to `stats::TukeyHSD`"),
    )
  }
)




add_cog_group(
  "quantile_quantile",
  field_info("x", "continuous"),
  "quantile quantile plot diagnostics",
  fn = function(
    x, ...,
    distribution = stats::qnorm, dparams = list(), na.rm = FALSE
  ) {

    dt <- data.frame(sample = x)
    ret <- ggplot2::StatQq$compute_group(
      dt, NULL,
      distribution = distribution,
      dparams = dparams,
      na.rm = na.rm
    )

    y_line <- quantile(dt$sample, probs = c(0.25, 0.75), type = 7, na.rm = TRUE)
    x_line <- distribution(c(0.25, 0.75))

    slope <- diff(y_line) / diff(x_line)
    int <- y_line[1L] - slope * x_line[1L]

    ret$theo_line <- ret$theoretical * slope + int

    sum_above <- sum(
      (ret$sample - ret$theo_line) > 0
    )
    sum_below <- sum(
      (ret$sample - ret$theo_line) < 0
    )

    ks_ans <- suppressWarnings(
      ks.test(ret$sample, ret$theoretical)
    )
    qq_mse <- mean(
      (ret$sample - ret$theo_line) ^ 2
    )

    max_deviation <- max(abs(ret$sample - ret$theoretical))

    list(
      points_above = cog_desc(sum_above, "Sum of sample points above qqline"),
      points_below = cog_desc(sum_below, "Sum of sample points below qqline"),
      ks_test = cog_desc(ks_ans$p.value, "Kolmogorov-Smirnov test p value. Low value represents differing distributions"),
      qq_mse = cog_desc(qq_mse, "Mean Squared Error of sample points to theoretical QQ line")

      # # Should not add as the line is not shifted.  What is displayed does not make sense when looking at the max deviation
      # ,
      # max_deviation = cog_desc(max_deviation, "Max deviation from the theoretical QQ line")
    )
  }
)


add_cog_group(
  "smooth_line",
  bind_rows(
    field_info("x", "continuous"),
    field_info("y", "continuous")
  ),
  "Smooth curve diagnostics",
  fn = function(
    x, y, ...,
    method = "auto", formula = y ~ x, se = TRUE,
    n = 80, span = 0.75, fullrange = FALSE, xseq = NULL, level = 0.95,
    method_args = list(), na.rm = FALSE
  ) {

    dt <- data.frame(
      x = x,
      y = y,
      group = 1,
      PANEL = 1
    )

    scales <- list(
      x = ggplot2::ScaleContinuous$clone(),
      y = ggplot2::ScaleContinuous$clone()
    )
    scales$x$train(dt$x)
    scales$y$train(dt$y)

    params <- list(method = method, formula = formula, se = se,
    n = n, span = span, fullrange = fullrange, xseq = xseq, level = level,
    method.args = method_args, na.rm = na.rm)

    params <- suppressMessages(ggplot2::StatSmooth$setup_params(dt, params))

    ret <- do.call(ggplot2::StatSmooth$compute_group, append(list(dt, scales), params))

    dt$y_fit <- approx(ret$x, ret$y, xout = dt$x)$y
    mse <- mean(
      (dt$y - dt$y_fit) ^ 2
    )
    deviations <- abs(dt$y - dt$y_fit)
    max_pos <- which.max(deviations)[1]
    max_deviation <- deviations[max_pos]
    max_deviation_location <- dt$x[max_pos]

    # y_max_out <- approx(ret$x, ret$ymax, xout = dt$x)
    # outliers_above <- sum(dt$y > y_max_out)
    #
    # y_min_out <- approx(ret$x, ret$ymax, xout = dt$x)
    # outliers_below <- sum(dt$y < y_min_out)
    #
    # outliers_n <- outliers_above = outliers_below

    # max_se <- max(ret$se)

    list(
      mse = cog_desc(mse, "Mean Squared Error of fitted points and y data"),
      max_deviation = cog_desc(max_deviation, "Max deviation from the fitted points"),
      max_deviation_location = cog_desc(max_deviation_location, "X location of the max deviation from the fitted points")
    )
  }
)


add_cog_group(
  "linear_model",
  bind_rows(
    field_info("x", "continuous"),
    field_info("y", "continuous")
  ),
  "Linear model diagnostics",
  fn = function(
    x, y, ...,
    weights = 1,
    formula = y ~ x,
    # se = TRUE,
    # n = 80, span = 0.75, fullrange = FALSE, xseq = NULL, level = 0.95,
    method_args = list(), na.rm = FALSE
  ) {
    method <- "lm"

    dt <- data.frame(
      x = x,
      y = y,
      group = 1,
      PANEL = 1,
      weights = weights
    )

    params <- list(
      method = method, formula = formula,
      # se = se,
      # n = n, span = span, fullrange = fullrange, xseq = xseq, level = level,
      method.args = method_args, na.rm = na.rm
    )

    params <- suppressMessages(ggplot2::StatSmooth$setup_params(dt, params))
    core_params <- list(formula, data = dt, weights = dt$weights)

    mod <- do.call(lm, c(core_params, params$method.args))

    coefs <- broom::tidy(mod)
    infos <- broom::glance(mod)
    dta <- broom::augment(mod)

    p <- nrow(coefs)
    n <- nrow(dta)

    ret <- list()
    if ("(Intercept)" %in% coefs$term) {
      ret$intercept <- cog_desc(coefs$estimate[1], "intercept of model")
      ret$intercept_p_value <- cog_desc(coefs$p.value[1], "intercept of model")
    }
    coefs <- coefs[coefs$term != "(Intercept)", ]

    bc <- MASS::boxcox(mod, plotit = FALSE)
    bc_range <- range(bc$x[bc$y > max(bc$y) - (1 / 2) * qchisq(.95, 1)])

    ret %>%
      append(list(
        beta = cog_desc(coefs$estimate[1], "beta value of coefficient"),
        beta_p_value = cog_desc(coefs$p.value[1], "significance of coefficient"),
        r2 = cog_desc(infos$r.squared, "fraction of variance explained by the model"),
        sigma = cog_desc(infos$sigma, "square root of the estimated residual variance"),
        statistic = cog_desc(infos$statistic, "F-statistic of the model"),
        p_value = cog_desc(infos$p.value, "p-value from the F test"),
        df = cog_desc(infos$p.value, "degrees of freedom used by the coefficients"),
        log_lik = cog_desc(infos$logLik, "log-likelihood value under the model"),
        aic = cog_desc(infos$AIC, "Akaike's Information Criterion"),
        bic = cog_desc(infos$BIC, "Schwarz's Bayesian Criterion"),
        deviance = cog_desc(infos$deviance, "quality-of-fit statistic of the model"),
        df_residual = cog_desc(infos$df.residual, "residual degrees of freedom"),
        n_sig_cooks = cog_desc(
          sum(dta$.cooksd > qf(0.5, p, n - p)),
          "number of significant cooks distance points. (sum(cooks_distance > F_{p, n-p}(0.5)))"
        ),
        n_sig_hat = cog_desc(
          sum(dta$.hat > (2 * 1 / nrow(dta))),
          "number of significant influence points. (sum(hat > 2 * p / n))"
        ),
        resid_shapiro = cog_desc(
          shapiro.test(dta$.resid)$p.value,
          "an approximate p-value for the Shapiro-Wilk test of normality.  \"This is said in Royston
          (1995) to be adequate for 'p.value < 0.1'\""
        ),
        bc_lower = cog_desc(bc_range[1], "lower bound of 95% CI of Box Cox Transformation"),
        bc_upper = cog_desc(bc_range[2], "upper bound of 95% CI of Box Cox Transformation")
      ))
  }
)



add_cog_group(
  "loess_model",
  bind_rows(
    field_info("x", "continuous"),
    field_info("y", "continuous")
  ),
  "Loess model diagnostics",
  fn = function(
    x, y, ...,
    weights = 1,
    formula = y ~ x,
    # se = TRUE,
    # n = 80,
    span = 0.75,
    # fullrange = FALSE, xseq = NULL, level = 0.95,
    method_args = list(), na.rm = FALSE
  ) {
    method <- "loess"

    params <- list(
      method = method, formula = formula,
      # se = se,
      # n = n,
      span = span,
      # fullrange = fullrange, xseq = xseq, level = level,
      method.args = method_args, na.rm = na.rm
    )

    dt <- data.frame(x = x, y = y, weights = weights)

    params <- suppressMessages(ggplot2::StatSmooth$setup_params(dt, params))
    core_params <- list(formula, data = dt, weights = dt$weights, span = span)

    mod <- do.call(loess, c(core_params, params$method.args))
    infos <- mod[c(
      "n", "enp", "s", "trace.hat"
    )]
    infos <- append(infos, mod$pars)

    list(
      enp = cog_desc(infos$enp, "effective number of parameters"),
      s = cog_desc(infos$s, "sigma of loess model"),
      trace.hat = cog_desc(infos$trace.hat, "trace of hat matrix"),
      span = cog_desc(infos$span, "parameter alpha which controls the degree of smoothing"),
      degree = cog_desc(infos$degree, "polynomial degree used in loess model"),
      # normalize = cog_desc(infos$normalize, "was data normalized before fit?"),
      # parametric = cog_desc(infos$parametric, "should any terms be fitted globally rather than locally?"),
      # drop.square = cog_desc(infos$drop.square, "for fits with more than one predictor and 'degree = 2', should the quadratic term be dropped for particular predictors?"),
      # family = cog_desc(infos$family, "if 'gaussian' fitting is by least-squares, and if 'symmetric' a re-descending M estimator is used with Tukey's biweight function"),
      iterations = cog_desc(infos$iterations, "number of iterations used to calculate model")
    )
  }
)



add_cog_group(
  "bivariate_step",
  bind_rows(
    field_info("x", "continuous"),
    field_info("y", "continuous")
  ),
  "Step function",
  fn = function(
    x, y, ...,
    direction = "hv"
  ) {

    dt <- data.frame(x = x, y = y, group = 1, PANEL = 1)

    stairstep <- getFromNamespace("stairstep", "ggplot2")
    step_path <- stairstep(dt, direction)

    unique_x_steps <- unique(step_path$x)
    if (length(unique_x_steps) > 1) {
      step_diff <- unique_x_steps[-1] - unique_x_steps[-length(unique_x_steps)]

      min_step_width <- min(step_diff)
      mean_step_width <- mean(step_diff)
      median_step_width <- median(step_diff)
      max_step_width <- max(step_diff)
      var_step_width <- var(step_diff)
    } else {
      min_step_width <-
        mean_step_width <-
        median_step_width <-
        max_step_width <-
        var_step_width <- NA
    }

    group_counts <- step_path %>%
      group_by(x) %>%
      count() %>%
      filter(n > 1)

    if (nrow(group_counts) > 0) {
      multi_groups <- group_counts$x
      grouped_step_path <- step_path[step_path$x %in% multi_groups, ]
      heights <- grouped_step_path %>%
        group_by(x) %>%
        summarise(
          min = min(y),
          max = max(y),
        ) %>%
        mutate(
          diff = max - min
        )
      diffs <- heights$diff
      min_step_height <- min(diffs)
      mean_step_height <- mean(diffs)
      median_step_height <- median(diffs)
      max_step_height <- max(diffs)
      var_step_height <- var(diffs)
    } else {
      min_step_height <-
        mean_step_height <-
        median_step_height <-
        max_step_height <-
        var_step_height <- NA
    }

    list(
      steps = cog_desc(length(unique_x_steps) - 1, "number of steps"),

      min_step_width = cog_desc(min_step_width, "minimum step width"),
      mean_step_width = cog_desc(mean_step_width, "mean step width"),
      median_step_width = cog_desc(median_step_width, "median step width"),
      max_step_width = cog_desc(max_step_width, "max step width"),
      var_step_width = cog_desc(var_step_width, "variance of step widths"),

      min_step_height = cog_desc(min_step_height, "minimum step group height"),
      mean_step_height = cog_desc(mean_step_height, "mean step group height"),
      median_step_height = cog_desc(median_step_height, "median step group height"),
      max_step_height = cog_desc(max_step_height, "max step group height"),
      var_step_height = cog_desc(var_step_width, "variance of step group heights")
    )
  }
)







# known_cog_groups() %>% tibble::glimpse() %>% as.list() %>% print()
