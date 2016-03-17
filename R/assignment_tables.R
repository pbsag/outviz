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
    links <- links %>%
      mutate_(
        "Group" = lazyeval::interp(~ cut_volumes(x), x = as.name(volume))
      )
  } else {
    links <- links %>%
      mutate_(
        "Group" = group_field
      )
  }

  # table by grouping
  lt <- links %>%
    group_by(Group) %>%
    summarise(
      `Number of Links` = n(),
      `Percent RMSE` = pct_rmse(volume, count)
    )

  #totals row
  tot <- links %>%
    ungroup() %>%
    mutate(Group = "Total") %>%
    summarise(
      Group = Group[1],
      `Number of Links` = n(),
      `Percent RMSE` = pct_rmse(volume, count)
    )

  suppressWarnings(
    # this will complain because we are joining a factor to a
    # character. don't need to worry
    bind_rows(lt, tot)
  )

}



