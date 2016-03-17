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

#' Calculate RMSE
#'
#' @inheritParams pct_error
#'
#' @return The root mean squared error between \code{x} and \code{y}
#'
rmse <- function(x, y){
  n <- length(x)
  sq_error <- ( x - y )^2
  sqrt( sum(sq_error) / (n - 1) )
}

#' Calculate percent RMSE
#'
#' @inheritParams pct_error
#'
#' @return The percent root mean squared error between \code{x} and \code{y}
#'
pct_rmse <- function(x, y){
  rmse(x, y) / mean(y) * 100
}
