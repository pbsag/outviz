#' Link validation plot
#'
#'
#' @param links Model link table as a \code{tidy} data frame, with each row
#'   representing a single direction of a single link.
#' @param volume Character string identifying the modeled volume in the link
#'   table.
#' @param count Character string identifying the calibration/validation counts
#'   in the link table.
#' @param color_field (Optional) character string identifying variable to color
#'   observations by, for example facility type.
#'
#' @return A ggplot2 plot object
#'
#' @import ggplot2
#' @import dplyr
#'
#' @examples
#' plot_validation(links, "volume", "count")
#' plot_validation(links, "volume", "count", color_field = "facility_group")
#'
#' @export
plot_validation <- function(links, volume, count, color_field = NULL) {

  # if split by color, then add factor variable of the color field
  if(!is.null(color_field)){
    links <- links %>%
      mutate_(
        "color" = lazyeval::interp(~ factor(var), var = as.name(color_field))
      )

    p <- ggplot(links, aes_string(x = count, y = volume, color = "color")) +
      scale_color_discrete(color_field)
  } else {
    p <- ggplot(links, aes_string(x = volume, y = count))
  }

  # Add geometries for points and statistics, and return
  p +
    geom_point(alpha = 0.3) +
    geom_abline(slope = 1, intercept = 0) +
    stat_smooth() +

    # Add labels
    ylab("Modeled link volume") +
    xlab("Observed link volume")

}
