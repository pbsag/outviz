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
#' @details Given in percentage points (already multiplied by 100),
#'   \eqn{ ((x - y) / y) * 100}
#'
#' @examples
#' pct_error(435, 422)
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
#' @details \eqn{\sqrt{\sum(x - y)^2/(n - 1)}}
#'
#' @examples
#' x <- runif(10)
#' y <- x + rnorm(10)
#' rmse(x, y)
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
#' @details \eqn{\sqrt{\sum(x - y)^2/(n - 1)} / \bar{y} * 100}
#'
#' @seealso rmse
#'
#' @examples
#' x <- runif(10)
#' y <- x + rnorm(10)
#' pct_rmse(x, y)
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
#' @examples
#' cuts <- cut_volumes(links$volume)
#' cuts[1:10]
#' table(cuts)
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


#' Cut error measurements into ranges
#'
#' @param x a vector of percent error measurements, as from \link{pct_error}
#'
#' @param breaks A vector of error ranges; zero and infinity will be added.
#' @param negative Mirror the breaks on the negative side, default is TRUE.
#'
#' @return a factor variable with each entry in x binned.
#'
#' @details This is a convenience wrapper to \code{\link[base](cut)} with
#'   sensible pre-coded options for travel demand output analysis.
#'
#' @examples
#' cuts <- cut_error(rnorm(100, 0, 10))
#' table(cuts)
#'
#' @export
#'
cut_error <- function(x, breaks = c(5, 10, 20, 1), negative = TRUE){

  if(negative){
    breaks <- c(-Inf, rev(-1 * breaks), 0, breaks, Inf)
  } else {
    breaks <- c(0, breaks, Inf)
  }

  cut(x, breaks)

}


#' Cut diverging differences into ranges
#'
#' @param x a vector of diverging error measurements, as in \eqn{x - y}
#' @return a factor variable with each entry in x binned.
#'
cut_diverror <- function(x){
  .Deprecated("cut_error")
  brks <- c(1, 10, 100, 1000)
  cut(x, breaks = c(-Inf, rev(-1 * brks), 0, brks, Inf))
}

#' Cut absolute differences into ranges
#'
#' @param x a vector of absolute error measurements, as in \eqn{x - y}
#' @return a factor variable with each entry in x binned.
#'
cut_abserror <- function(x){
  .Deprecated("cut_error")
  brks <- c(1, 10, 100, 1000)
  cut(x, breaks = c(-Inf, rev(-1 * brks), 0, brks, Inf))
}
