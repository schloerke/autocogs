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
    envir = autocogs_env
  )

  invisible(cog_group)
}

autocogs_env <- environment()



#' @title Default Cognostic Group Functions
#'
#' @description These set of functions comprise the default cognostic groups.  Each function produces it's own cognostic information given the required pieces of data.
#'
#' The functions' print method will display the description.  \code{autocog_*} functions will take the \code{\link{known_cog_groups}()} functions and format the output into a single row tibble.  Any new known cognostic group function, NAME,  will create a function called autocog_NAME, which may be called.
#'
#' Default Cognostic Group Functions:
#' \itemize{
#' \item{autocog_bivariate_continuous}
#' \item{autocog_bivariate_counts}
#' \item{autocog_bivariate_step}
#' \item{autocog_boxplot}
#' \item{autocog_density_2d_continuous}
#' \item{autocog_density_continuous}
#' \item{autocog_grouped_counts}
#' \item{autocog_grouped_testing}
#' \item{autocog_hex_counts}
#' \item{autocog_histogram_counts}
#' \item{autocog_linear_model}
#' \item{autocog_loess_model}
#' \item{autocog_pairwise_counts}
#' \item{autocog_quantile_quantile}
#' \item{autocog_scagnostics}
#' \item{autocog_smooth_line}
#' \item{autocog_square_counts}
#' \item{autocog_univariate_continuous}
#' \item{autocog_univariate_counts}
#' \item{autocog_univariate_discrete}
#' }
#'
#' @param x data that should appear on an x axis
#' @param y data that should appear on an y axis
#' @param ... ignored
#' @param direction step direction. Defaults to \code{"hv"}
#' @param na.rm should \code{NA} points be removed when performing calculations
#' @param h,n,bins,binwidth,clusters,bw,adjust,kernel,trim,group,groups,center,boundary,closed,pad,breaks,weights,formula,method_args,span,distribution,dparams,method,se,fullrange,xseq,level,origin,drop parameters usually set by corresponding "geoms" to be used within ggplot2 Stat* methods
#' @exportPattern autocog_
#' @name autocog_
#' @seealso \code{\link{known_cog_groups}()}
#' @rdname autocog_
#' @aliases autocog_bivariate_continuous autocog_bivariate_counts autocog_bivariate_step autocog_boxplot autocog_density_2d_continuous autocog_density_continuous autocog_grouped_counts autocog_grouped_testing autocog_hex_counts autocog_histogram_counts autocog_linear_model autocog_loess_model autocog_pairwise_counts autocog_quantile_quantile autocog_scagnostics autocog_smooth_line autocog_square_counts autocog_univariate_continuous autocog_univariate_counts autocog_univariate_discrete
#'
#' @examples
#' autocog_bivariate_continuous
#' autocog_bivariate_continuous(iris$Sepal.Length, iris$Sepal.Width)
NULL
