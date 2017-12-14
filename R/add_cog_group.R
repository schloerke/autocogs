#' @include known_cog_groups.R autocog.R

#' @title Add a cognostic group
#'
#' @description Add a new cognostic to be used when calculating automatic cognostics.
#'
#' @param name Name of cognostic group
#' @param fields \code{data.frame} of \code{'dimension'} and \code{'type'} columns. \code{dplyr::\link[dplyr]{bind_rows}()} of \code{\link{field_info}} outputs for convenience
#' @param description Description of cognostic group
#' @param fn function to calculate a cognostic group.  May return a named list or a single row tibble.  Each value of the return data should be the output of \code{\link{cog_desc}}
#' @param ... ignored
#' @export
#' @examples
#' \dontrun{
#' add_cog_group(
#'   "univariate_continuous",
#'   field_info("x", "continuous"),
#'   "univariate metrics for continuous data",
#'   function(x, ...) {
#'     x_range <- range(x, na.rm = TRUE)
#'     list(
#'       min = cog_desc(x_range[1], "minimum of non NA data"),
#'       max = cog_desc(x_range[2], "maximum of non NA data"),
#'       mean = cog_desc(mean(x, na.rm = TRUE), "mean of non NA data"),
#'       median = cog_desc(median(x, na.rm = TRUE), "median of non NA data"),
#'       var = cog_desc(var(x, na.rm = TRUE), "variance of non NA data")
#'     )
#'   }
#' )
#' }
add_cog_group <- function(
  name,
  fields,
  description = NA,
  fn,
  ...
  # ,
  # verbose = TRUE
) {

  assert_character(name, len = 1, any.missing = FALSE)
  assert_data_frame(fields, c("character", "character"), min.rows = 1)
  assert_names(names(fields), identical.to = c("dimension", "type"))
  assert_character(description, any.missing = TRUE)
  assert_function(fn)
  # assert_logical(verbose, len = 1, any.missing = FALSE)

  cog_group <- data_frame(
    name,
    fields = list(fields),
    description = description,
    fn = list(fn)
  )

  known_cog_groups_ <<- bind_rows(known_cog_groups_, cog_group)

  assign(
    paste("autocog_", name, sep = ""),
    autocog(name, .fn_only = TRUE),
    envir = autocog_env
  )

  invisible(cog_group)
}

#' @exportPattern autocog_
autocog_env <- environment()
