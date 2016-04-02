#' Build an attractive cross-tabulation
#'
#'
#' @param df a \code{data.frame} containing the variables to cross-tabulate.
#' @param var1 a character field name in \code{df} that will make the rows of the table.
#' @param var2 a character field name in \code{df} that will make the columns of the table.
#' @param weight_var a character field name in \code{df} that weights the data. If
#'   \code{NULL}, will treat each record equally.
#' @param margins boolean, should the row and column sums be appended?
#'   default \code{FALSE}
#' @param percent boolean, should the table be a percentage?
#'   default \code{FALSE}
#'
#'
#' @return an array that can be cast with \code{kable()} or examined directly.
#'
#' @import dplyr
#' @import tidyr
#'
#' @examples
#' crosstable(links, "facility_group", "area_name")
#' crosstable(links, "facility_group", "area_name", margins = TRUE)
#' crosstable(links, "facility_group", "area_name", percent = TRUE)
#'
#'
#' @export
crosstable <- function(df, var1, var2, weight_var = NULL,
                       margins = FALSE, percent = FALSE){

  a <-
    df %>%
    transmute_(
      var1, var2,
      # if no weight given, equal weight
      weight_var = ifelse(is.null(weight_var), 1, weight_var)
    ) %>%
    group_by_(var1, var2) %>%
    summarise(n = sum(weight_var)) %>%
    spread_(var2, "n", fill = 0)

  m <- as.matrix(a[, -1])

  rownames(m) <- paste(var1, names(table(df[, var1])))
  colnames(m) <- paste(var2, names(table(df[, var2])))

  # Convert raw numbers to percentages if requested
  if(percent){
    m <- m / sum(m) * 100
  }

  # Add row and column totals if requested
  if(margins){
    m <- cbind(m, rowSums(m))
    m <- rbind(m, colSums(m))
    colnames(m)[ncol(m)] <- paste(var1, "total")
    rownames(m)[nrow(m)] <- paste(var2, "total")
  }

  return(m)
}#'
