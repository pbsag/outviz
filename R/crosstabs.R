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
#' @details If \code{var1,var2} are numeric vectors, then the name of the field
#'   will be prepended to the row or column names. If not, then the bare
#'   attribute name will be given.
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
#' crosstable(links, "facility_group", "area_type", percent = TRUE)
#'
#'
#' @export
crosstable <- function(df, var1, var2, weight_var = NULL,
                       margins = FALSE, percent = FALSE){

  a <- df %>%
    transmute_(
      var1, var2,
      # if no weight given, equal weight
      weight_var = ifelse(is.null(weight_var), 1, weight_var)
    ) %>%
    group_by_(var1, var2) %>%
    summarise(n = sum(weight_var)) %>%
    spread_(var2, "n", fill = 0)

  m <- as.matrix(a[, -1])

  # If the variables are numeric, prepend with variable names
  for(i in 1:2){
    #rows are 1, cols are 2
    if(i == 1){v <- var1} else {v <- var2}

    if(is.numeric(df[[v]])){
      dimnames(m)[[i]] <- paste(v, names(table(df[, v])))
    } else {
      dimnames(m)[[i]] <- names(table(df[, v]))
    }
  }


  # Convert raw numbers to percentages if requested
  if(percent){
    m <- m / sum(m) * 100
  }

  # Add row and column totals if requested
  if(margins){
    m <- cbind(m, rowSums(m))
    m <- rbind(m, colSums(m))
    colnames(m)[ncol(m)] <- "Total"
    rownames(m)[nrow(m)] <- "Total"
  }

  return(m)
}
