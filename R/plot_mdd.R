#' Maximum desirable deviation plot
#'
#' @inheritParams plot_validation
#'
#' @return A ggplot2 plot object
#'
#' @import ggplot2
#' @import dplyr
#' @import lazyeval
#'
#' @examples
#' plot_mdd(links, "volume", "count")
#' plot_mdd(links, "volume", "count", color_field = "facility_group")
#'
#' @export
plot_mdd <- function(links, volume, count, color_field = NULL) {

  # calculate absolute percent error on links
  links <- links %>%
    mutate_(
      "error" = lazyeval::interp(
        ~ abs(pct_error(x, y)),
        x = as.name(volume),
        y = as.name(count)
    ))

  # if split by color, then add factor variable of the color field
  if(!is.null(color_field)){
    links <- links %>%
      mutate_(
        "color" = lazyeval::interp(~ factor(var), var = as.name(color_field))
      )

    p <- ggplot(links, aes_string(x = count, y = "error", color = "color")) +
      scale_color_discrete(color_field)
  } else {
    p <- ggplot(links, aes_string(x = volume, y = "error"))
  }

  # Add geometries for points and statistics, and return
  p +
    geom_point(alpha = 0.3) +
    geom_line(data = mdd, aes_string(x = "volume", y = "mdd", color = NULL)) +
    coord_cartesian(ylim = c(0, 100), xlim = c(0, max(links[, volume]))) +
    stat_smooth() +

    # Add labels
    ylab("Percent error from observed volume") +
    xlab("Modeled link volume")

}