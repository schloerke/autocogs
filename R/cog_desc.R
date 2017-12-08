
#' Cognostic
#'
#' Add a description to a cognostic (subset metric)
#' @param x univariate scalar
#' @param desc description of \code{x}
#' @export
#' @examples
#' cog_desc(mean(1:10), "mean of 10 numbers")
cog_desc <- function(x, desc = NULL) {
  assert_scalar(x, na.ok = TRUE)
  assert_character(desc, len = 1, any.missing = FALSE)
  attr(x, "desc") <- desc
  x
}
