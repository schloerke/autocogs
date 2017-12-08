

#' Cognostic Specification
#'
#' @param bivariate_continuous,bivariate_counts,bivariate_step,boxplot,density_2d_continuous,density_continuous,grouped_counts,grouped_testing,hex_counts,histogram_counts,linear_model,loess_model,pairwise_counts,quantile_quantile,scagnostics,smooth_line,square_counts,univariate_continuous,univariate_counts,univariate_discrete names of cognostic groups to calculate.  The boolean value (TRUE) supplied to each argument determines if the value should be displayed if possible or removed if possible.
#' @param ... ignored.  Will cause error if any are supplied
#' @param .keep_layer boolean (TRUE) that determines if the layer should be kept at all
#' @export
#' @rdname cog_spec
#' @return cognostic specification that determines which cogs are added or removed if possible
#' @examples
#' # example cog specifications
#' # display like normal
#' cog_spec(); TRUE
#' # remove scagnostics
#' cog_spec(scagnostics = FALSE)
#' # remove layer
#' cog_spec(.keep_layer = FALSE); FALSE
#'
#' # set up data
#' p <- ggplot2::qplot(Sepal.Length, Sepal.Width, data = iris, geom = c("point", "smooth"))
#' dt <- tibble::data_frame(panel = list(p))
#'
#' # compute cognostics like normal
#' add_panel_cogs(dt)
#'
#' # do not compute scagnostics for geom_point cognostics
#' # compute geom_smooth cognostics
#' add_panel_cogs(dt, spec = list(cog_spec(scagnostics = FALSE), TRUE))
#'
#' # do not compute scagnostics for geom_point cognostics
#' # do not compute geom_smooth cognostics
#' add_panel_cogs(dt, spec = list(cog_spec(scagnostics = FALSE), FALSE))
cog_spec <- function(
  bivariate_continuous = TRUE,
  bivariate_counts = TRUE,
  bivariate_step = TRUE,
  boxplot = TRUE,
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
  .keep_layer = TRUE
) {

  dots <- list(...)
  # check for unnamed args
  if (length(dots) > 0) {
    dot_names <- names(dots)
    if (is.null(dot_names) || any(dot_names == "")) {
      stop("all arguments supplied to `cog_spec()` should be named")
    }
  }

  args <- as.list(sys.call())[-1]
  args$.keep_layer <- NULL # do not match the keep_layer argument

  known_vals <- character(0)

  if (length(args) > 0) {
    # turn symbols into values
    args <- lapply(args, eval, envir = parent.frame())
    assert_list(args, min.len = 1, types = "logical", any.missing = FALSE)

    known_vals <- known_cog_groups_name()

    matched_arg_pos <- pmatch(names(args), known_cog_groups_name(), duplicates.ok = FALSE)
    if (any(is.na(matched_arg_pos))) {
      stop(
        "all arguments supplied to `cog_spec()` must be able to be matched to known cognostic groups.\n",
        "unmatched arguments: \n\t", paste(names(args)[is.na(matched_arg_pos)], collapse = "\n\t"), "\n",
        "known cognostic groups: \n\t", paste(known_cog_groups_name(), collapse = "\n\t"), "\n",
        "extra arguments: \n\t",
        ".keep_layer", "\n"
      )
    }

    known_vals <- known_vals[matched_arg_pos][!unlist(args)]
  }

  ret <- list(
    keep_layer = .keep_layer,
    remove = known_vals
  )
  class(ret) <- "cog_spec"
  ret
}


#' @export
#' @rdname cog_spec
#' @param p plot object in question
#' @param specs list of cog_spec outputs for each layer of the plot object
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
    cog_spec(.keep_layer = spec)
  })

  specs
}



cog_specs_keep_layer <- function(cog_specs) {
  lapply(cog_specs, `[[`, "keep_layer") %>% unlist()
}
