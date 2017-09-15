
#' Plot class
#'
#' First class of the plot object. Exception is ggplot2 as many objects are of class 'gg'
#' @param p plot object to retrieve class from
#' @rdname plot_class
#' @export
#' @examples
#' library(ggplot2)
#' p <- qplot(Sepal.Length, Sepal.Width, data = iris)
#' plot_class(p)
plot_class <- function(p) {
  UseMethod("plot_class", p)
}


#' @rdname plot_class
#' @export
plot_class.default <- function(p) {
  class(p)[1]
}


#' @rdname plot_class
#' @export
plot_class.gg <- function(p) {
  NextMethod()
}


#' @rdname plot_class
#' @export
plot_class.ggplot <- function(p) {
  return("ggplot")
}
