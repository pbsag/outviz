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
#'   \item{a, b}{Node identifiers}
#'   \item{facility_type}{The facility type of the links}
#'   \item{facility_group}{High-level facility types}
#'   \item{area_type}{The area type of the links}
#'   \item{area_name}{The area type of the links with named description}
#'   \item{volume}{Modeled link volume}
#'   \item{screenline}{The screenline that the links belong to}
#'   \item{count}{The calibration year count value}
#'   \item{distance}{The distance value in miles}
#'   \item{capacity}{The capacity value of the links}
#'   \item{ffspeed}{The free-flow speed of the links in mph}
#'   \item{speed}{The speed of the links in mph}
#'
#' }
#'
"links"

#' District-to-District Flows
#'
#' A dataset containing district-to-district flows.
#'
#' @format A data frame with 20 observations and 3 variables.
#' \describe{
#'   \item{origin}{Origin zone}
#'   \item{destination}{Destination zone}
#'   \item{flow}{Flow from origin to destination}
#' }
#'
"flows"
