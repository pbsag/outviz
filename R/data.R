#' outviz: A package for standardized model outputs
#'
#' @docType package
#' @name outviz
NULL

#' Maximum desirable deviation curve
#'
#' A dataset containing the maximum desirable error for links by volume.
#'
#' @format A data frame with 7 rows and 2 variables:
#' \describe{
#'   \item{volume}{the volume category of the road}
#'   \item{mdd}{maximum desirable deviation, in percent}
#' }
#'
"mdd"

#' Link output table
#'
#' A dataset containing example link volume and count information to demonstrate
#' package functionality.
#'
#' @format A data frame with 505 observations and 8 variables.
#' \describe{
#'   \item{a}{Node identifier}
#'   \item{b}{Node identifier}
#'   \item{facility_type}{The facility type of the links}
#'   \item{facility_group}{High-level facility types}
#'   \item{area_type}{The area type of the links}
#'   \item{area_name}{The descriptive area type of the links}
#'   \item{volume}{Modeled link volume}
#'   \item{screenline}{The screenline that the links belong to}
#'   \item{count}{The calibration year count value}
#' }
#'
"links"
