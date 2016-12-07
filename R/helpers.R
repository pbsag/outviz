#' Calculate percent error
#'
#' @param x Comparison value
#' @param y Reference value
#'
#' @return a numeric vector of \code{length(x)} containing the percent deviation
#'   from \code{y}
#'
#' @export
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
#' @export
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
#' @export
#'
pct_rmse <- function(x, y){
  rmse(x, y) / mean(y) * 100
}




#' Cut volumes into pretty levels
#'
#' @param x Volume levels
#' @param breaks Breakpoints for the volume groups
#' @return A labeled factor variable of \code{length(x)} with the levels of
#'   \code{x} cut into bins.
#'
#' @export
#'
cut_volumes <- function(x, breaks = c(0, 5, 10, 15, 20, 40, 60, Inf)) {

  breaks <- breaks * 1000
  n <- length(breaks)
  labels <- c(
    paste(breaks[2:n-2], "-", breaks[3:n-1]),
    paste(">", breaks[n-1])
  )
  cut(x, breaks = breaks, labels = labels, include.lowest = TRUE)

}

