

is_date <- function(x) {
  inherits(x, "Date") ||
  inherits(x, "Time") ||
  inherits(x, "Datetime")
}

is_numeric <- function(x) {
  is.numeric(x)
}

is_discrete <- function(x) {
  !is_numeric(x)
}
