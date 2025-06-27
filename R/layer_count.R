#' Plot layer count
#'
#' Retrieves the number of layers in a given plot
#' @param p plot object
#' @return number
#' @export
#' @rdname layer_count
#' @examples
#' library(ggplot2)
#' p <- ggplot(iris, aes(Sepal.Length, Sepal.Width)) + geom_point()
#' layer_count(p) # 1
#' layer_count(p + geom_smooth(method = "lm") + geom_density_2d()) # 3
layer_count <- function(p) {
  UseMethod("layer_count")
}


#' @export
#' @rdname layer_count
layer_count.default <- function(p) {
  stop("Please implement `layer_count.", class(p)[1], "(p)`")
}


#' @export
#' @rdname layer_count
layer_count.ggplot <- function(p) {
  length(p$layers)
}
