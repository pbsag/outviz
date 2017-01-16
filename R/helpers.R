#' Numerify a mixed vector
#'
#' @param x A mixed vector consisting of items such as `person1` and `3auto`
#'
#' @details Serves as a wrapper to `gsub` and `type.convert`
#'
#' @examples
#' x <- c("person1", "person2", "person3")
#' numerify(x)
#'
#' @export
#'
numerify <- function(x){
  type.convert(gsub("[^0-9]", "", x))
}


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
  ifelse(
    x == 0 & y == 0, 0, # protect against divide by zero on empty links
    (x - y) / y * 100)
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




#' Cut percent error into ranges
#'
#' @param x a vector of error measurements
#' @return a factor showing the bin
#'
cut_error <- function(x){
  brks <- c(0.05, 0.1, 0.2, 0.5, 1)
  cut(x, breaks = c(0, brks, Inf))
}

#' Cut diverging percent error into ranges
#'
#' @param x a vector of percent error measurements, as from \link{pct_error}
#' @return a factor variable with each entry in x binned.
#'
cut_diverror <- function(x){
  brks <- c(0.05, 0.10,  0.20, 1) * 100
  cut(x, breaks = c(-Inf, rev(-1 * brks), 0, brks, Inf))
}


#' Cut absolute differences into ranges
#'
#' @param x a vector of absolute error measurements, as in \eqn{x - y}
#' @return a factor variable with each entry in x binned.
#'
cut_abserror <- function(x){
  brks <- c(1, 10, 100, 1000)
  cut(x, breaks = c(-Inf, rev(-1 * brks), 0, brks, Inf))
}
