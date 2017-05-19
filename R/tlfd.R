#' Plot a trip length frequency distribution comparison
#'
#' @param model A dataframe with the following fields
#'   \describe{
#'     \item{bin}{A column listing the bin values}
#'     \item{count}{A column listing the count per bin}
#'   }
#' @param target A dataframe with the following fields
#'   \describe{
#'     \item{bin}{A column listing the bin values}
#'     \item{count}{A column listing the count per bin}
#'   }
#'
#' @param names Character vector of names for traces
#' @param xaxis Name to plot on x axis
#' @param yaxis Name to plot on y axis
#'
#'
#' @return A ggplot2 object
#'
#'
#'
#'
#'
plot_tlfd <- function(model, target, names, xaxis, yaxis){


}



#' Plot a trip length frequency distribution comparison as an interactive figure
#'
#' @inheritParams plot_tlfd
#'
#'
#' @return A plotly object
#'
#' @export
#'
plotly_tlfd <- function(model, target = NULL, names = c("model", "target"),
                        xaxis = "bin", yaxis = "count"){

  p <- plot_ly(x = ~bin, y = ~count) %>%
    add_trace(data = model, name = names[1], type = "scatter", mode = "lines")

  if (!is.null(target)) {
    p <- p %>%
      add_trace(data = target, name = names[2], type = "bar")
  }

  # Set axis labels
  p <- p %>%
    layout(
      xaxis = list(title = xaxis),
      yaxis = list(title = yaxis)
    )

  return(p)
}

#' Prepares model data for tlfd plotting. Given a skim table and model trip
#' table, will return a table in the format needed by \code{plotly_tlfd} and
#' \code{plot_tlfd}.
#'
#' @param skim An impedance dataframe with the following columns
#'   \describe{
#'     \item{from}{From TAZ}
#'     \item{to}{To TAZ}
#'     \item{imp}{Impendance between from and to TAZs
#'     }
#'   }
#'
#' @param model A trip dataframe with the following columns.
#'   \describe{
#'     \item{from}{From TAZ}
#'     \item{to}{To TAZ}
#'     \item{trips}{Number of trips between from and to TAZs}
#'   }
#'
#' @param max_dist Maximum distance bin to create
#'
#' @param pct \code{TRUE/FALSE} If true, a percentage distribution will be
#'   returned. If false, the raw counts will be returned.
#'
#' @return A dataframe with a \code{bin} column of impedance and a \code{count}
#'   column of observations.
#'
#' @export
#'
prep_tlfd_data <- function(skim, model, max_dist = 60, pct = TRUE) {

  # Join skim to model trip table and process
  final <- model %>%
    dplyr::left_join(skim, by = c("from" = "from", "to" = "to")) %>%
    dplyr::mutate(bin = pmin(floor(imp), max_dist)) %>%
    dplyr::group_by(bin) %>%
    dplyr::summarize(count = sum(trips)) %>%
    ungroup()

  if (pct) {
    final <- final %>%
      mutate(count = round(count / sum(count) * 100, 2))
  }

  return(final)
}
