#' Create an HTML chord diagram from matrix information
#'
#' @param df A data-frame representation of flows
#' @param origin The character name of the variable to use as an origin.
#' @param destination The character name of the variable to use as a destination.
#' @param flow The character name of the variable to use as a flow.
#' @inheritDotParams chorddiag::chorddiag
#'
#' @return An htmlwidgets object showing the flow between origins and destinations
#'   in the data frame.
#'
#' @importFrom dplyr select_
#' @importFrom magrittr '%>%'
#' @importFrom chorddiag chorddiag
#'
#' @export
#'
chord_chart <- function(df, origin = "origin", destination = "destination",
                        flow = NULL, ...){

  if (is.null(flow)){
    stop("Please supply a flow variable.")
  }

  m <- df %>% select_(origin, destination, flow) %>%
    convert_table_to_matrix(destination, flow)

  chorddiag::chorddiag(data = m, ...)

}


#' Convert a dataframe to a matrix
#'
#' @param df A data frame with three columns: a row variable, a column variable,
#'   and a value variable.
#' @param column_var The name of the variable to use for columns.
#' @param cell The name of the variable to use for filling the matrix.
#'
#' @return a square matrix
#'
#' @importFrom tidyr spread
#'
convert_table_to_matrix <- function(df, column_var, cell){
  a <- tidyr::spread_(df, column_var, cell, fill = 0)

  # change first column (with row value) into a row name
  m <- as.matrix(a[, -1])
  rownames(m) <- colnames(m)

  return(m)
}
