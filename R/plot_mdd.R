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
        ~ pct_error(x, y),
        x = as.name(volume),
        y = as.name(count)
    ))

  # super high-error links, which can arise because of mistakes in the counts,
  # will disrupt the smoothing. Remove these and throw a warning
  if(any(links$error > 1e3)){
    warning("Some links have extremely high error. Confirm the count data.")
    links <- links %>% filter(error < 1e3)
  }


  # Add
  p <- ggplot() +
    geom_ribbon(
      data = mdd %>% mutate(mdd1 = -1 * mdd),
      aes(x = volume, ymax = mdd, ymin = mdd1), alpha = 0.2) +
    coord_cartesian(ylim = c(-100, 125), xlim = c(0, max(links[, count]))) +

    # Add labels
    ylab("Percent error from observed volume") +
    xlab("Observed link volume")

  # if split by color, then add factor variable of the color field
  if(!is.null(color_field)){
    links <- links %>%
      mutate_(
        "color" = lazyeval::interp(~ factor(var), var = as.name(color_field))
      )

    p +
      geom_point(data = links, aes_(
        x = as.name(count), as.name("error"), color = as.name("color")),
        alpha = 0.7) +
      scale_color_discrete(color_field)
  } else {
    p +
      geom_point(data = links, aes_(x = as.name(count), as.name("error")),
        alpha = 0.7)
  }


}

#' Maximum desirable devation plotly object
#'
#' @inheritParams plot_mdd
#'
#' @return a plotly object
#'
#' @importFrom plotly plot_ly add_trace layout
#' @importFrom magrittr '%>%'
#'
#' @examples
#' plotly_mdd(links, "volume", "count", "facility_group")
#'
#' @export
#'
plotly_mdd <- function(links, volume, count, color_field = NULL){


  plotly::plot_ly() %>%
    plotly::add_trace(
      data = mdd, x = ~volume, y = ~mdd,
      mode = "lines", type = "scatter", color = I("grey"), showlegend = FALSE) %>%
    plotly::add_trace(
      data = mdd, x = ~volume, y = ~mdd * -1, fill = "tonexty",
      mode = "lines", type = "scatter", color = I("grey"), showlegend = FALSE) %>%
    plotly::add_trace(
      x = links[[count]], y = pct_error(links[[volume]], links[[count]]),
      mode = "markers", type = "scatter", color = links[[color_field]]) %>%
    plotly::layout(
      xaxis = list(title = "Count"),
      yaxis = list(title = "Percent Error from Count", range = c(-100, 200))
    )

}
