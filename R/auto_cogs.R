#' @include known.R
NULL



add_auto_cog(
  "univariate_continuous",
  field_info("x", "continuous"),
  "univariate metrics for continuous data",
  # function(data, mapping) {
  #   str_c("_univar_", mapping$x)
  # },
  function(x, ...) {
    x_range <- range(x, na.rm = TRUE)
    list(
      min = cog(x_range[1], "minimum of non NA data"),
      max = cog(x_range[2], "maximum of non NA data"),
      mean = cog(mean(x, na.rm = TRUE), "mean of non NA data"),
      median = cog(median(x, na.rm = TRUE), "median of non NA data"),
      var = cog(var(x, na.rm = TRUE), "variance of non NA data")
    )
  }
)


add_auto_cog(
  "univariate_discrete",
  field_info("x", "discrete"),
  "univariate metrics for discrete data",
  function(x, ...) {
    table_counts <- x %>% table() %>% sort()
    count_names <- names(table_counts)
    n_groups <- length(table_counts)
    list(
      min_name = cog(count_names[1], "minimum group count name"),
      count_min = cog(table_counts[1], "minimum group count"),
      count_mean = cog(sum(table_counts) / n_groups, "average group count"),
      count_max = cog(table_counts[n_groups], "maximum group count"),
      max_name = cog(count_names[n_groups], "maximum group count name")
    )
  }
)


add_auto_cog(
  "boxplot_metrics",
  field_info("x", "continuous"),
  "univariate boxplot metrics for continuous data",
  function(x, ...) {
    boxplot_info <- StatBoxplot$compute_group(
      data.frame(x = 1, y = x), scales = NULL, width = 1, na.rm = FALSE, coef = 1.5
    )
    median <- boxplot_info$middle[[1]]
    outliers <- boxplot_info$outliers[[1]]
    list(
      "n_outlier_lower" = cog(sum(outliers < median), "count of lower outliers"),
      "lower_whisker" = cog(boxplot_info$lower[[1]], "minimum point"),
      "q1" = cog(boxplot_info$notchlower[[1]], "first quartile value"),
      "median" = cog(median, "median value"),
      "q3" = cog(boxplot_info$notchupper[[1]], "third quartile value"),
      "upper_whisker" = cog(boxplot_info$upper[[1]], "maximum non outlier point"),
      "n_outlier_upper" = cog(sum(outliers > median), "count of upper outliers")
    )
  }
)


add_auto_cog(
  "bivariate_continuous",
  bind_rows(
    field_info("x", "continuous"),
    field_info("y", "continuous")
  ),
  "continuous bivariate metrics",
  function(x, y, ...) {
    list(
      "covariance" = cog(cov(x, y, use = "na.or.complete"), "covariance of non na pairs"),
      "correlation" = cog(cor(x, y, use = "na.or.complete"), "correlation of non na pairs")
    )
  }
)


#' @import scagnostics
add_auto_cog(
  "scagnostics",
  bind_rows(
    field_info("x", "continuous"),
    field_info("y", "continuous")
  ),
  "scagnostics of bivariate continuous data",
  function(x, y, ...) {
    info <- as.list(scagnostics(x, y))
    list(
      Outlying = cog(info$Outlying, "proportion of the total edge length due to extremely long edges connected to points of single degree"),
      Skewed = cog(info$Skewed, "distribution of edge lengths of a minimum spanning tree gives us information about the relative density of points in a scattered configuration"),
      Clumpy = cog(info$Clumpy, "An extremely skewed distribution of MST edge lengths does not necessarily indicate clustering of points. For this, we turn to another measure based on the MST: the Hartigan and Mo- hanty RUNT statistic [20]. This statistic is most easily un- derstood in terms of the single-linkage hierarchical clustering tree called a dendrogram."),
      Sparse = cog(info$Sparse, "the 90% quantile of the edge lengths of the MST"),
      Striated = cog(info$Striated, "sumation of angles over all adjacent edges of a MST"),
      Convex = cog(info$Convex, "ratio of the area of the alpha hull and the area of the convex hull"),
      Skinny = cog(info$Skinny, "ratio of perimeter to area of a polygon measures, roughly, how skinny it is"),
      Stringy = cog(info$Stringy, "The stringy measure is based on the π index, which is the ratio of width to length of a network"),
      Monotonic = cog(info$Monotonic, "squared Spearman corre- lation coefficient, which is a Pearson correlation on the ranks of x and y (corrected for ties)")
    )
  }
)


add_auto_cog(
  "univariate_counts",
  bind_rows(
    field_info("x", "any")
  ),
  "univariate count information",
  function(x, ...) {
    n <- length(x)
    x_is_na <- is.na(x)
    list(
      "n" = cog(n, "count of non NA data"),
      "n_na" = cog(sum(x_is_na), "count of NA data")
    )
  }
)
add_auto_cog(
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
      "n" = cog(n, "count of non NA data"),
      "n_both_na" = cog(sum(x_is_na & y_is_na), "count of data where both X and Y are NA"),
      "n_or_na" = cog(sum(x_is_na | y_is_na), "count of data where either X or Y are NA"),
      "n_x_na" = cog(sum(x_is_na), "count of X NA data"),
      "n_y_na" = cog(sum(y_is_na), "count of Y NA data")
    )
  }
)


add_auto_cog(
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
    hex_info <- StatBinhex$compute_group(dt, NULL, binwidth)
    counts <- hex_info$count
    list(
      count_min = cog(min(counts), "minimum count"),
      count_max = cog(max(counts), "maximum count"),
      count_mean = cog(mean(counts), "mean (expected) count"),
      count_median = cog(median(counts), "median count"),
      count_var = cog(var(counts), "variance of counts"),
      "chisq" = cog(chisq.test(counts)$p.value, "p-value of chi square test of counts. Low value means non statistically 'uniform' counts")
    )
  }
)
add_auto_cog(
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
      x = ScaleContinuous$clone(),
      y = ScaleContinuous$clone()
    )
    scales$x$train(dt$x)
    scales$y$train(dt$y)
    bin_dt <- StatBin2d$compute_group(dt, scales,
      binwidth = binwidth, bins = bins,
      breaks = breaks, origin = origin, drop = drop
    )
    counts <- bin_dt$count
    list(
      count_min = cog(min(counts), "minimum count"),
      count_max = cog(max(counts), "maximum count"),
      count_mean = cog(mean(counts), "mean (expected) count"),
      count_median = cog(median(counts), "median count"),
      count_var = cog(var(counts), "variance of counts"),
      "chisq" = cog(chisq.test(counts)$p.value, "p-value of chi square test of counts. Low value means non statistically 'uniform' counts")
    )
  }
)


# need to import the BIC function as it's called internally in Mclust()
#' @importFrom mclust Mclust mclustBIC
add_auto_cog(
  "density_continuous",
  field_info("x", "continuous"),
  "density plot information",
  function(
    x, ...,
    # StatDensity parameters
    bw = "nrd0", adjust = 1, kernel = "gaussian", n = 512, trim = FALSE, na.rm = FALSE
  ) {

    n <- length(x)
    x_is_na <- is.na(x)
    dt <- data.frame(x)

    if (length(unique(dt$x)) == 1) {
      return(NULL)
    }

    scales <- list(
      x = ScaleContinuous$clone()
    )
    scales$x$train(dt$x)

    ret <- StatDensity$compute_group(
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
      max_density = cog(max_density, "maximum density height"),
      max_density_location = cog(max_density_location, "location of maximum density height"),
      # max_density_slope = cog(max_slope, "maximum absolute value density slope"),
      # uniform_mse = cog(mse, "mean squared error compared to uniform")
      cluters = cog(
        mclust::Mclust(dt$x, verbose = FALSE)$G,
        "optimal number of components found using Model-Based Clustering"
      ),
      uni_modal_p_value = cog(
        diptest::dip.test(dt$x)$p.value,
        "Hartigans' dip test for unimodality / multimodality. (Low p-value means non-unimodal density)"
      ),
      skew = cog(moments::skewness(x, na.rm = TRUE), "skewness of non NA data"),
      kurt = cog(moments::kurtosis(x, na.rm = TRUE), "kurtosis of non NA data")
    )
  }
)


add_auto_cog(
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
    clusters = length(x) <= 1000
  ) {

    n <- length(x)
    dt <- data.frame(x = x, y = y, group = 1, PANEL = 1)

    scales <- list(
      x = ScaleContinuous$clone(),
      y = ScaleContinuous$clone()
    )
    scales$x$train(dt$x)
    scales$y$train(dt$y)

    ret <- StatDensity2d$compute_group(
      dt, scales,
      na.rm = na.rm, h = h, contour = FALSE, n = n, bins = bins, binwidth = binwidth
    )

    max_density <- max(ret$density)
    max_row <- which(ret$density == max_density)[1]
    max_density_x <- ret$x[max_row]
    max_density_y <- ret$y[max_row]

    ret <- list(
      max_density = cog(max_density, "maximum density height"),
      max_density_x = cog(max_density_x, "X location of maximum density height"),
      max_density_y = cog(max_density_y, "Y location of maximum density height")#,
      # max_density_slope = cog(max_slope, "maximum absolute value density slope"),
      # uniform_mse = cog(mse, "mean squared error compared to uniform")
    )
    if (isTRUE(clusters)) {
      ret$clusters <- cog(
        mclust::Mclust(dt[, c("x", "y")], verbose = FALSE)$G,
        "optimal number of components found using Model-Based Clustering"
      )
    }

    ret
  }
)



add_auto_cog(
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
      x = ScaleContinuous$clone()
    )
    scales$x$train(dt$x)

    params <- list(
      binwidth = binwidth, bins = bins, center = center,
      boundary = boundary, closed = closed, pad = pad,
      breaks = breaks
    )
    params <- StatBin$setup_params(dt, params)
    ret <- do.call(StatBin$compute_group, append(list(dt, scales), params))

    counts <- ret$count

    list(
      count_min = cog(min(counts), "minimum count"),
      count_max = cog(max(counts), "maximum count"),
      count_mean = cog(mean(counts), "mean (expected) count"),
      count_median = cog(median(counts), "median count"),
      count_var = cog(var(counts), "variance of counts"),
      "chisq" = cog(chisq.test(counts)$p.value, "p-value of chi square test of counts. Low value means non statistically 'uniform' counts")
    )
  }
)


add_auto_cog(
  "pairwise_counts",
  bind_rows(
    field_info("x", "any"),
    field_info("y", "any")
  ),
  "count information for grouped data",
  function(x, y, ...) {
    dt <- data_frame(x, y)

    counts <- dt %>% group_by(x, y) %>% count()

    is_na_xy <- is.na(counts$x) | is.na(counts$y)

    if (any(is_na_xy)) {
      na_count <- counts$n[is_na_xy]
      counts <- counts %>% filter_(!is_na_xy)
    } else {
      na_count <- 0
    }

    count_values <- counts$n

    list(
      min = cog(min(count_values), "minimum occurances of an x,y pair"),
      max = cog(max(count_values), "maximum occurances of an x,y pair"),
      mean = cog(mean(count_values), "mean occurances of an x,y pair"),
      median = cog(median(count_values), "median occurances of an x,y pair"),
      var = cog(var(count_values), "variance of occurances of an x,y pair"),
      na_count = cog(na_count, "count of NA occurances of a x,y pair")
    )
  }
)


add_auto_cog(
  "grouped_counts",
  bind_rows(
    field_info("x", "continuous"),
    field_info("group", "any")
  ),
  "count information for grouped data",
  function(x, group, ...) {
    data_frame(x, group) %>%
      group_by(group) %>%
      count() ->
    counts

    na_group <- is.na(counts$group)
    if (any(na_group)) {
      na_count <- counts$n[na_group]
      counts <- counts %>% filter_(!na_group)
    } else {
      na_count <- 0
    }

    count_values <- counts$n

    list(
      min = cog(min(count_values), "minimum occurances of a group"),
      max = cog(max(count_values), "maximum occurances of a group"),
      mean = cog(mean(count_values), "mean occurances of a group"),
      median = cog(median(count_values), "median occurances of a group"),
      var = cog(var(count_values), "variance of occurances of a group"),
      na_count = cog(na_count, "count of NA occurances of the grouping variable")
    )
  }
)


add_auto_cog(
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
      anova_model_df = cog(an_broom$df[1], "ANOVA model degrees of freedom"),
      anova_model_df_resid = cog(an_broom$df[2], "ANOVA residual degrees of freedom"),
      anova_f_value = cog(an_broom$statistic[1], "ANOVA F value"),
      anova_p_value = cog(an_broom$p.value[1], "ANOVA p value"),
      sig_diff_pairs = cog(num_sig_diff, "Number of significantly different groups according to `stats::TukeyHSD`"),
    )
  }
)




add_auto_cog(
  "quantile_quantile",
  field_info("x", "continuous"),
  "quantile quantile plot diagnostics",
  fn = function(
    x, ...,
    distribution = stats::qnorm, dparams = list(), na.rm = FALSE
  ) {

    dt <- data.frame(sample = x)
    ret <- StatQq$compute_group(
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
      points_above = cog(sum_above, "Sum of sample points above qqline"),
      points_below = cog(sum_below, "Sum of sample points below qqline"),
      ks_test = cog(ks_ans$p.value, "Kolmogorov-Smirnov test p value. Low value represents differing distributions"),
      qq_mse = cog(qq_mse, "Mean Squared Error of sample points to theoretical QQ line"),
      max_deviation = cog(max_deviation, "Max deviation from the theoretical QQ line")
    )
  }
)


add_auto_cog(
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
      x = ScaleContinuous$clone(),
      y = ScaleContinuous$clone()
    )
    scales$x$train(dt$x)
    scales$y$train(dt$y)

    params <- list(method = method, formula = formula, se = se,
    n = n, span = span, fullrange = fullrange, xseq = xseq, level = level,
    method.args = method_args, na.rm = na.rm)

    params <- suppressMessages(StatSmooth$setup_params(dt, params))

    ret <- do.call(StatSmooth$compute_group, append(list(dt, scales), params))

    dt$y_fit <- approx(ret$x, ret$y, xout = dt$x)$y
    mse <- mean(
      (dt$y - dt$y_fit) ^ 2
    )
    max_deviation <- max(abs(dt$y - dt$y_fit))

    # y_max_out <- approx(ret$x, ret$ymax, xout = dt$x)
    # outliers_above <- sum(dt$y > y_max_out)
    #
    # y_min_out <- approx(ret$x, ret$ymax, xout = dt$x)
    # outliers_below <- sum(dt$y < y_min_out)
    #
    # outliers_n <- outliers_above = outliers_below

    # max_se <- max(ret$se)

    list(
      mse = cog(mse, "Mean Squared Error of fitted points and y data"),
      max_deviation = cog(max_deviation, "Max deviation from fitted points")
    )
  }
)


add_auto_cog(
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

    params <- suppressMessages(StatSmooth$setup_params(dt, params))
    core_params <- list(formula, data = dt, weight = dt$weights)

    mod <- do.call(lm, c(core_params, params$method.args))

    coefs <- broom::tidy(mod)
    infos <- broom::glance(mod)
    dta <- broom::augment(mod)

    ret <- list()
    if ("(Intercept)" %in% coefs$term) {
      ret$intercept <- cog(coefs$estimate[1], "intercept of model")
      ret$intercept_p_value <- cog(coefs$p.value[1], "intercept of model")
    }
    coefs <- coefs[coefs$term != "(Intercept)", ]

    bc <- MASS::boxcox(mod)
    bc_range <- range(bc$x[bc$y > max(bc$y) - (1 / 2) * qchisq(.95, 1)])

    ret %>%
      append(list(
        beta = cog(coefs$estimate[1], "beta value of coefficient"),
        beta_p_value = cog(coefs$p.value[1], "significance of coefficient"),
        r2 = cog(infos$r.squared, "fraction of variance explained by the model"),
        sigma = cog(infos$sigma, "square root of the estimated residual variance"),
        statistic = cog(infos$statistic, "F-statistic of the model"),
        p_value = cog(infos$p.value, "p-value form the F test"),
        df = cog(infos$p.value, "degrees of freedom used by the coefficients"),
        log_lik = cog(infos$logLik, "log-likelihood value under the model"),
        aic = cog(infos$AIC, "Akaike's An Information Criterion"),
        bic = cog(infos$BIC, "Schwarz's Bayesian criterion"),
        deviance = cog(infos$deviance, "quality-of-fit statistic of the model"),
        df_residual = cog(infos$df.residual, "residual degrees of freedom"),
        n_sig_cooks = cog(
          sum(dta$.cooksd > 4 / nrow(dta)),
          "number of significant cooks distance points. (sum(cooks_distance >= 4/n))"
        ),
        n_sig_hat = cog(
          sum(dta$.hat > (2 * 1 / nrow(dta))),
          "number of significant influence points. (sum(hat > 2 * p / n))"
        ),
        resid_shapiro = cog(
          shapiro.test(dta$.resid)$p.value,
          "an approximate p-value for the Shapiro-Wilk test of normality.  \"This is said in Royston
          (1995) to be adequate for ‘p.value < 0.1’\""
        ),
        bc_lower = cog(bc_range[1], "lower bound of 95% CI of Box Cox Transformation"),
        bc_upper = cog(bc_range[2], "upper bound of 95% CI of Box Cox Transformation")
      ))
  }
)



add_auto_cog(
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

    params <- suppressMessages(StatSmooth$setup_params(dt, params))
    core_params <- list(formula, data = dt, weight = dt$weights, span = span)

    mod <- do.call(loess, c(core_params, params$method.args))
    infos <- mod[c(
      "n", "enp", "s", "trace.hat"
    )]
    infos <- append(infos, mod$pars)

    list(
      enp = cog(infos$enp, "effective number of parameters"),
      s = cog(infos$s, "sigma of loess model"),
      trace.hat = cog(infos$trace.hat, "trace of hat matrix"),
      span = cog(infos$span, "parameter alpha which controls the degree of smoothing"),
      degree = cog(infos$degree, "polynomial degree used in loess model"),
      # normalize = cog(infos$normalize, "was data normalized before fit?"),
      # parametric = cog(infos$parametric, "should any terms be fitted globally rather than locally?"),
      # drop.square = cog(infos$drop.square, "for fits with more than one predictor and ‘degree = 2’, should the quadratic term be dropped for particular predictors?"),
      # family = cog(infos$family, "if 'gaussian' fitting is by least-squares, and if 'symmetric' a re-descending M estimator is used with Tukey's biweight function"),
      iterations = cog(infos$iterations, "number of iterations used to calculate model")
    )
  }
)







# known_cogs %>% tibble::glimpse() %>% as.list() %>% print()
