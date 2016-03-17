#' Calculate percent error
#'
#' @param x Comparison value
#' @param y Reference value
#'
#' @return a numeric vector of \code{length(x)} containing the percent deviation
#'   from \code{y}
#'
#'
pct_error <- function(x, y) {
  (x - y) / y * 100
}
