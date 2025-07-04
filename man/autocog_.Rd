% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_cog_group.R
\name{autocog_}
\alias{autocog_}
\alias{autocog_bivariate_continuous}
\alias{autocog_bivariate_counts}
\alias{autocog_bivariate_step}
\alias{autocog_boxplot}
\alias{autocog_density_2d_continuous}
\alias{autocog_density_continuous}
\alias{autocog_grouped_counts}
\alias{autocog_grouped_testing}
\alias{autocog_hex_counts}
\alias{autocog_histogram_counts}
\alias{autocog_linear_model}
\alias{autocog_loess_model}
\alias{autocog_pairwise_counts}
\alias{autocog_quantile_quantile}
\alias{autocog_scagnostics}
\alias{autocog_smooth_line}
\alias{autocog_square_counts}
\alias{autocog_univariate_continuous}
\alias{autocog_univariate_counts}
\alias{autocog_univariate_discrete}
\title{Default Cognostic Group Functions}
\arguments{
\item{x}{data that should appear on an x axis}

\item{y}{data that should appear on an y axis}

\item{...}{ignored}

\item{direction}{step direction. Defaults to \code{"hv"}}

\item{na.rm}{should \code{NA} points be removed when performing calculations}

\item{h, n, bins, binwidth, clusters, bw, adjust, kernel, trim, group, groups, center, boundary, closed, pad, breaks, weights, formula, method_args, span, distribution, dparams, method, se, fullrange, xseq, level, origin, drop}{parameters usually set by corresponding "geoms" to be used within ggplot2 Stat* methods}
}
\description{
These set of functions comprise the default cognostic groups.  Each function produces it's own cognostic information given the required pieces of data.

The functions' print method will display the description.  \verb{autocog_*} functions will take the \code{\link[=known_cog_groups]{known_cog_groups()}} functions and format the output into a single row tibble.  Any new known cognostic group function, NAME,  will create a function called autocog_NAME, which may be called.

Default Cognostic Group Functions:
\itemize{
\item{autocog_bivariate_continuous}
\item{autocog_bivariate_counts}
\item{autocog_bivariate_step}
\item{autocog_boxplot}
\item{autocog_density_2d_continuous}
\item{autocog_density_continuous}
\item{autocog_grouped_counts}
\item{autocog_grouped_testing}
\item{autocog_hex_counts}
\item{autocog_histogram_counts}
\item{autocog_linear_model}
\item{autocog_loess_model}
\item{autocog_pairwise_counts}
\item{autocog_quantile_quantile}
\item{autocog_scagnostics}
\item{autocog_smooth_line}
\item{autocog_square_counts}
\item{autocog_univariate_continuous}
\item{autocog_univariate_counts}
\item{autocog_univariate_discrete}
}
}
\examples{
autocog_bivariate_continuous
autocog_bivariate_continuous(iris$Sepal.Length, iris$Sepal.Width)
}
\seealso{
\code{\link[=known_cog_groups]{known_cog_groups()}}
}
