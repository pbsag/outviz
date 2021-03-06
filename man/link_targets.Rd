% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/link_targets.R
\name{link_targets}
\alias{link_targets}
\title{Percent of links on target}
\usage{
link_targets(links, volume, count, freeway_scope = NULL,
  volume_scope = NULL)
}
\arguments{
\item{links}{Model link table as a \code{tidy} data frame, with each row
representing an analysis link. The function assumes that the data is
already tidy: two-way links should already be converted to single values,
etc.}

\item{volume}{Character string identifying the modeled volume in the link
table.}

\item{count}{Character string identifying the calibration/validation counts
in the link table.}

\item{freeway_scope}{A character string that will \code{dplyr::filter(links,
freeway_scope)} down to freeway/expressway links. See examples.}

\item{volume_scope}{A character string that will \code{dplyr::filter(links,
volume_scope)} down to high-volume links. See examples.}
}
\value{
A \code{data_frame} with the tests listed by row
}
\description{
Percent of links on target
}
\examples{
link_targets(links, "volume", "count",
             "facility_group == 'Expressway'", "volume > 10000")

}
