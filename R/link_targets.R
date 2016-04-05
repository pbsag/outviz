#' Percent of links on target
#'
#' @inheritParams plot_validation
#' @param freeway_scope A character string that will \code{dplyr::filter(links,
#'   freeway_scope)} down to freeway/expressway links. See examples.
#' @param volume_scope A character string that will \code{dplyr::filter(links,
#'   volume_scope)} down to high-volume links. See examples.
#'
#' @return A \code{data_frame} with the tests listed by row
#'
#' @examples
#' link_targets(links, "volume", "count",
#'              "facility_group == 'Expressway'", "volume > 10000")
#'
#' @export
#'
link_targets <- function(links, volume, count,
                         freeway_scope = NULL,
                         volume_scope = NULL ) {

  # compute link error
  l <- links %>%
    mutate_(
      error = lazyeval::interp(~abs(pct_error(x, y)),
                               x = as.name(volume), y = as.name(count))
    )

  test_frame <- list()

  # High volume links
  if(!is.null(volume_scope)){
    f <- l %>%
      filter_(.dots = volume_scope)

    # links w/in 20%
    test_frame[["highv"]] <- bind_rows(
      link_test(f, quote(error <= 30),
                "Links greater than 10k volume under 30% error", 75),
      link_test(f, quote(error <= 15),
                "Links greater than 10k volume under 15% error", 50)
    )

  }

  # filter freeway links
  if(!is.null(freeway_scope)){
    f <- l %>%
      filter_(.dots = freeway_scope)

    # links w/in 20%
    test_frame[["expressways"]] <- bind_rows(
      link_test(f, quote(error <= 20), "Expressway links under 20% error", 75),
      link_test(f, quote(error <= 10), "Expressway links under 10% error", 50)
    )

  }

  bind_rows(test_frame)
}


#' Construct a test call
#'
#' @param d A links data_frame with the universe of the test
#' @param crit A quoted evaluationa call describing a passed test.
#' @param string A character string defining the test
#' @param target The target passing value
#'
#' @return A \code{data_frame} with a description and result of the test.
#'
#' @examples
#' \dontrun{
#' # This is a pretend test to illustrate
#' link_test(links, quote(volume > count), "Volume over count", 50)
#' }
link_test <- function(d, crit, string, target){

  d %>%
    mutate_(test = lazyeval::interp(~ifelse(x, 1, 0), x = crit))  %>%

    summarise(
      `Test` = string,
      `Percent passing` = sum(test) / n() * 100,
      `Target` = target
    )
}

