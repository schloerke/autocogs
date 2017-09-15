

cog_spec <- function(
  bivariate_continuous = TRUE,
  bivariate_counts = TRUE,
  bivariate_step = TRUE,
  boxplot_metrics = TRUE,
  density_2d_continuous = TRUE,
  density_continuous = TRUE,
  grouped_counts = TRUE,
  grouped_testing = TRUE,
  hex_counts = TRUE,
  histogram_counts = TRUE,
  linear_model = TRUE,
  loess_model = TRUE,
  pairwise_counts = TRUE,
  quantile_quantile = TRUE,
  scagnostics = TRUE,
  smooth_line = TRUE,
  square_counts = TRUE,
  univariate_continuous = TRUE,
  univariate_counts = TRUE,
  univariate_discrete = TRUE,
  ...,
  keep = TRUE
) {

  dots <- list(...)
  if (length(dots) > 0) {
    dot_names <- names(dots)
    if (is.null(dot_names) || any(dot_names == "")) {
      stop("all arguments supplied to `cog_spec()` should be named")
    }
  }

  args <- as.list(sys.call())[-1]
  args$keep <- NULL

  known_vals <- character(0)

  if (length(args) > 0) {
    # turn symbols into values
    args <- lapply(args, eval, envir = parent.frame())
    assert_list(args, min.len = 1, types = "logical", any.missing = FALSE)

    known_vals <- known_cog_groups$name

    matched_arg_pos <- pmatch(names(args), known_cog_groups$name, duplicates.ok = FALSE)
    if (any(is.na(matched_arg_pos))) {
      stop(
        "all arguments supplied to `cog_spec()` must be able to be matched to known cognostic groups.\n",
        "unmatched arguments: \n\t", paste(names(args)[is.na(matched_arg_pos)], collapse = "\n\t"), "\n",
        "known cognostic groups: \n\t", paste(known_cog_groups$name, collapse = "\n\t")
      )
    }

    known_vals <- known_vals[matched_arg_pos][!unlist(args)]
  }

  ret <- list(
    keep = keep,
    remove = known_vals
  )
  class(ret) <- "cog_spec"
  ret
}



as_cog_specs <- function(p, specs) {
  if (inherits(specs, "cog_spec")) {
    specs <- list(specs)
  }

  if (length(specs) < 1) {
    stop("`specs` must have a length of at least 1")
  }
  if (length(specs) == 1) {
    if (is.logical(specs[[1]])) {
      specs <- rep(specs[[1]], layer_count(p))
    }
  }
  specs <- as.list(specs)

  if (length(specs) != layer_count(p)) {
    stop("`specs` should have a length equal to the numer of layers in the plot (", layer_count(p), ") or 1, not ", length(specs))
  }

  specs <- lapply(specs, function(spec) {
    if (inherits(spec, "cog_spec")) {
      return(spec)
    }
    if (!test_logical(spec, any.missing = FALSE)) {
      stop("`spec` values should either by logical or created from `cog_spec()`. Found: ", paste(class(spec), collapse = ", "))
    }
    # only true of false values
    cog_spec(keep = spec)
  })

  specs
}
