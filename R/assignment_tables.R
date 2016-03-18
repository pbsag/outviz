#' Percent RMSE table
#'
#' @inheritParams plot_validation
#' @param group_field Character string identifying variable to
#'   group observations by, for example facility type. If set to same value
#'   as \code{volume}, will cut into bins.
#'
#' @return A \code{data_frame} with the link summary table.
#'
#' @import dplyr
#' @import lazyeval
#'
#' @examples
#' table_rmse(links, "volume", "count")
#'
#' a <- table_rmse(links, "volume", "count", group_field = "facility_group")
#' knitr::kable(a)
#'
#' @export
table_rmse <- function(links, volume, count, group_field = NULL) {

  # must supply group_field
  if(is.null(group_field)){
    stop("Must supply grouping variable")
  }

  # If group and volume are the same, cut into a pretty vector
  if(group_field == volume){
    group_field = "Volume"
    links <- links %>%
      mutate_(
        .dots = setNames(
          list(lazyeval::interp(~ cut_volumes(x), x = as.name(volume))),
          group_field)
      )
  }

  # table by grouping
  dots <- list(
    lazyeval::interp(~n()),
    lazyeval::interp(~pct_rmse(x, y), x = as.name(volume), y = as.name(count))
  )

  lt <- links %>%
    group_by_(as.character(group_field)) %>%
    summarise_(.dots = setNames(dots, c("Number of Links", "Percent RMSE")))

  #totals row
  dots[[3]] <- lazyeval::interp(~as.character(x), x = "Total")

  tot <- links %>%
    ungroup() %>%
    summarise_(.dots = setNames(
      dots, c("Number of Links", "Percent RMSE", as.character(group_field))
    ))

  suppressWarnings(
    # this will complain because we are joining a factor to a
    # character. don't need to worry
    bind_rows(lt, tot)
  )


}

#' Total flow table
#'
#' This table sums the volume on all links in a group, and compares against the
#' total count volume, as in a screenline comparison.
#'
#' @inheritParams table_rmse
#'
#' @return A \code{data_frame} with the link summary table.
#'
#' @import dplyr
#' @import lazyeval
#'
#' @examples
#' table_rmse(links, "volume", "count")
#'
#' a <- table_flow(links, "volume", "count", group_field = "facility_group")
#' knitr::kable(a)
#'
#' @export
#'
table_flow <- function(links, volume, count, group_field = NULL) {

  # must supply group_field
  if(is.null(group_field)){
    stop("Must supply grouping variable")
  }

  # If group and volume are the same, cut into a pretty vector
  if(group_field == volume){
    group_field <- "Volume"
    links <- links %>%
      mutate_(
        .dots = setNames(
          list(lazyeval::interp(~ cut_volumes(x), x = as.name(volume))),
          group_field)
      )
  }

  # table by grouping
  dots <- list(
    lazyeval::interp(~sum(x), x = as.name(volume)),
    lazyeval::interp(~sum(x), x = as.name(count))
  )

  lt <- links %>%
    group_by_(as.character(group_field)) %>%
    summarise_(.dots = setNames(dots, c("Model Flow", "Observed Flow")))

  #totals row
  dots[[3]] <- lazyeval::interp(~as.character(x), x = "Total")

  tot <- links %>%
    ungroup() %>%
    summarise_(.dots = setNames(
      dots,  c("Model Flow", "Observed Flow", as.character(group_field))
    ))

  suppressWarnings(
    # this will complain because we are joining a factor to a
    # character. don't need to worry
    bind_rows(lt, tot) %>%
      mutate(
        `Percent Deviation` = pct_error(`Model Flow`, `Observed Flow`)
      )
  )

}
