#' Link validation plot
#'
#'
#' @param links Model link table as a \code{tidy} data frame, with each row
#'   representing an analysis link. The function assumes that the data is
#'   already tidy: two-way links should already be converted to single values,
#'   etc.
#' @param volume Character string identifying the modeled volume in the link
#'   table.
#' @param count Character string identifying the calibration/validation counts
#'   in the link table.
#' @param color_field (Optional) character string identifying variable to color
#'   observations by, for example facility type.
#' @param show_lm If \code{TRUE}, plot a linear model regression fit alongside
#'   the fitted model's equation. Default \code{FALSE} will print spline
#'   regression fit without any equation.
#' @param id A link identification field. If NULL, then use \code{rownames(links)}
#'
#' @return A ggplot2 plot object.
#'
#'
#' @import ggplot2
#' @import dplyr
#' @import lazyeval
#'
#' @examples
#' plot_validation(links, "volume", "count", show_lm = TRUE)
#' plot_validation(links, "volume", "count", color_field = "facility_group")
#'
#' @export
plot_validation <- function(links, volume, count, color_field = NULL,
                            show_lm = TRUE, id = NULL) {

  # if split by color, then add factor variable of the color field
  if(!is.null(color_field)){
    links <- links %>%
      mutate_(
        "color" = lazyeval::interp(~ factor(var), var = as.name(color_field))
      )

    p <- ggplot(links, aes_string(x = count, y = volume, color = "color")) +
      scale_color_discrete(color_field)
  } else {
    p <- ggplot(links, aes_string(x = count, y = volume))
  }

  if(!is.null(id)){ row.names(links) <- links[[id]] }

  # Add geometries for points and statistics, and return
  p <- p +
    geom_abline(slope = 1, intercept = 0) +
    geom_point(alpha = 0.7) +
    # Add labels
    ylab("Modeled link volume") +
    xlab("Observed link volume")

  if(show_lm){
    p +
      # stat_smooth_func(
      stat_smooth_func(
        geom = "text", method = "lm",
        hjust = 0,
        parse = TRUE,
        size = 3) +
      geom_smooth(method = "lm", se = ifelse(is.null(color_field), TRUE, FALSE))
  } else {
    p
  }

}

#' Link validation plot with plot.ly
#'
#' @inheritParams plot_validation
#'
#' @importFrom plotly plot_ly add_lines add_trace layout
#' @examples
#' plotly_validation(links, "volume", "count", "facility_group")
#'
#' @export
#'
plotly_validation <- function(links, volume, count, color_field = NULL,
                              id = NULL, show_lm = TRUE){

  # The floating text in the chart gives the link id. If no field provided,
  # create one full of "na" values.
  if(is.null(id)){
    links$id <- "na"
    id <- "id"
  }

  # if split by color, then add factor variable of the color field
  if(!is.null(color_field)){
    links <- links %>%
      mutate_(
        "color" = lazyeval::interp(~ factor(var), var = as.name(color_field))
      )
  }

  # Calculate a linear model and create a text annotation for plot
  df <- data_frame(
    y = links[[volume]],
    x = links[[count]]
  )

  model <- lm(y ~ x, data = df)
  slope = round(coefficients(model)[2], 3)
  intercept = round(coefficients(model)[1], 2)
  if (intercept > 0) {
    sign = "+"
  } else {
    sign = "-"
  }
  equation <- paste0("y = ", slope, "x ", sign, " ", abs(intercept))

  # Create initial plot with a simple y = x line
  p <- plotly::plot_ly() %>%
    plotly::add_lines(
      x = c(1, max(links[[volume]])), y = c(1, max(links[[volume]])),
      alpha = 0.5, showlegend = FALSE, color = I("grey"))

  # Add data colored by color_field if it was provided. Otherwise leave the
  # color option blank.
  if(!is.null(color_field)){
    p <- p %>%
      plotly::add_trace(
        x = ~links[[count]], y = ~links[[volume]],
        color = ~links[[color_field]],
        type = "scatter", mode = "markers",
        text = ~paste("ID: ", links[[id]])
      )
  } else {
    p <- p %>%
      plotly::add_trace(
        x = ~links[[count]], y = ~links[[volume]],
        type = "scatter", mode = "markers",
        text = ~paste("ID: ", links[[id]])
      )
  }

  # Add the linear regression line and equation
  if(show_lm){
    p <- p %>%
      plotly::add_lines(
        x = links[[count]], y = stats::fitted(model),
        alpha = 0.5, showlegend = FALSE, color = I("blue")
      ) %>%
      plotly::add_text(
        x = max(links[[count]]) * .33, y = max(links[[volume]]), text = equation,
        showlegend = FALSE
      )
  }

  # Add axis information
  p <- p %>%
    plotly::layout( xaxis = list(title = "Count"), yaxis = list(title = "Model Volume") )

  p
}
