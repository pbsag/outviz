#' Link Statistics Table
#'
#' @inheritParams plot_validation
#' @param group_field Character string identifying variable to
#'   group observations by, for example facility type. If set to same value
#'   as \code{volume}, will cut into bins.
#' @param volume_breaks Numeric vector passed on to \code{cut()} identifying the
#'   breakpoints in the volume groups. Number in thousands, i.e.: 10, 20
#' @param type Which type of table to print. Currently supports percent RMSE and
#'   total flow deviation.
#'
#' @return A \code{data_frame} with the link summary table.
#'
#' @import dplyr
#' @import lazyeval
#'
#' @examples
#' link_stats_table(links, "volume", "count", group_field = "area_name", type = "rmse")
#' link_stats_table(links, "volume", "count", group_field = "area_name", type = "flow")
#' link_stats_table(links, "volume", "count", group_field = "facility_group", type = "rmse")
#' link_stats_table(links, "volume", "count", group_field = "facility_group", type = "flow")
#'
#'
#'
#'
#' @export
link_stats_table <- function(links, volume, count, group_field = NULL,
                             volume_breaks = c(0, 5, 10, 15, 20, 40, 60, Inf),
                             type = c("rmse", "flow")){

  # must supply group_field
  if(is.null(group_field)){
    stop("Must supply grouping variable")
  }


  # If group and volume are the same, cut into a pretty vector,
  # if they are different, make sure that the variable is factored.
  if(group_field == volume){
    links <- volume_levels(links, group_field, volume_breaks)
    group_field <- "Volume_Group"
  } else {
    links <- refactor_levels(links, group_field)
  }

  # If the type is RMSE, then calculate perc
  if(type == "rmse"){
    dots <- list(
      lazyeval::interp(~n()),
      lazyeval::interp(~pct_rmse(x, y), x = as.name(volume), y = as.name(count))
    )

    lt <- links %>%
      group_by_(group_field) %>%
      summarise_(.dots = setNames(dots, c("Number of Links", "Percent RMSE")))

    # Make the totals row
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

  } else if(type == "flow"){
    # table by grouping
    dots <- list(
      lazyeval::interp(~sum(x), x = as.name(volume)),
      lazyeval::interp(~sum(x), x = as.name(count))
    )

    lt <- links %>%
      group_by_(group_field) %>%
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



}


#' Link Measures Table
#'
#' @inheritParams plot_validation
#' @param group_field Character string identifying variable to
#'   group observations by, for example facility type. If set to same value
#'   as \code{volume}, will cut into bins.
#' @param volume_breaks Numeric vector passed on to \code{cut()} identifying the
#'   breakpoints in the volume groups. Number in thousands, i.e.: 10, 20
#' @param type Which type of table to print. Currently supports VMT, VHT, VHD and VOC.
#'
#' @param distance Character string identifying the distance in the link table.
#'
#' @param speed Character string identifying the modeled speed in the link table.
#'
#' @param ffspeed Character string identifying the free-flow speed in the link table.
#'
#' @param capacity Character string identifying the capacity in the link table.
#'
#' @return A \code{data_frame} with the link summary table.
#'
#' @import dplyr
#' @importFrom lazyeval interp
#' @importFrom magrittr '%>%'
#'
#' @examples
#' link_measures_table(links, "volume", distance = "distance",
#'   group_field = "area_name", type = "vmt")
#' link_measures_table(links, "volume", distance = "distance",
#'   group_field = "facility_group", type = "vmt")
#' link_measures_table(links, "volume", distance = "distance", speed = "speed",
#'   group_field = "area_name", type = "vht")
#' link_measures_table(links, "volume", distance = "distance", speed = "speed",
#'   group_field = "facility_group", type = "vht")
#' link_measures_table(links, "volume", distance = "distance", speed = "speed",
#'   ffspeed = "ffspeed",  group_field = "area_name", type = "vhd")
#' link_measures_table(links, "volume", distance = "distance", speed = "speed",
#'   ffspeed = "ffspeed",  group_field = "facility_group", type = "vhd")
#' link_measures_table(links, "volume", capacity = "capacity",
#'   group_field = "area_name", type = "voc")
#' link_measures_table(links, "volume", capacity = "capacity",
#'   group_field = "facility_group", type = "voc")
#'
#'
#' @export
link_measures_table <- function(links, volume, distance = NULL,
                                speed = NULL, ffspeed = NULL,
                                capacity = NULL, group_field = NULL,
                             volume_breaks = c(0, 5, 10, 15, 20, 40, 60, Inf),
                             type = c("vmt", "vht", "vhd", "voc")){

  # must supply group_field
  if(is.null(group_field)){
    stop("Must supply grouping variable")
  }


  # If group and volume are the same, cut into a pretty vector,
  # if they are different, make sure that the variable is factored.
  if(group_field == volume){
    links <- volume_levels(links, group_field, volume_breaks)
    group_field <- "Volume_Group"
  } else {
    links <- refactor_levels(links, group_field)
  }

  if(type == "vmt"){
    # vehicle miles traveled
    fn_agg <- lazyeval::interp(
      ~sum(x*y), x = as.name(volume), y = as.name(distance))
  } else if(type == "vht"){
    # vehicle hours traveled
    fn_agg <- lazyeval::interp(
      ~sum(x*y/z), x = as.name(volume), y = as.name(distance),
      z = as.name(speed))
  } else if(type == "vhd"){
    # vehicle hours of delay
    fn_agg <- lazyeval::interp(
      ~sum(x*(y/a-y/b)), x = as.name(volume), y = as.name(distance),
      a = as.name(speed), b = as.name(ffspeed))
  } else if(type == "voc"){
    # volume-to-capacity ratio
    fn_agg <- lazyeval::interp(
      ~sum(x/y), x = as.name(volume), y = as.name(capacity))
  }


  # build grouping table
  dots <- list( lazyeval::interp(~n()), fn_agg)

  lt <- links %>%
    group_by_(group_field) %>%
    summarise_(.dots = setNames(dots, c("Number of Links", toupper(type))))

  #totals row
  dots[[3]] <- lazyeval::interp(~as.character(x), x = "Total")

  tot <- links %>%
    ungroup() %>%
    summarise_(.dots = setNames(
      dots,  c("Number of Links", toupper(type), as.character(group_field))
    ))

  suppressWarnings(
    # this will complain because we are joining a factor to a
    # character. don't need to worry
    bind_rows(lt, tot)
  )



}

#' Refactor a variable so that it prints properly.
#'
#' @param df A data_frame
#' @param group_field a character name of a variable in DF to refactor
#'
#' @details If the variable is already a named factor, then this simply returns
#'
#' @import dplyr lazyeval
#' @return A data_frame with the appropriate factoring
refactor_levels <- function(df, group_field = NULL){

  if(!is.factor(df[[group_field]])){
    df %>% mutate_(
      .dots = setNames(
        list(lazyeval::interp(~factor(x), x = as.name(group_field))),
        group_field
      )
    )
  } else {
    df
  }

}

#' Cut a volumes variable into levels
#'
#' @param df A data_frame
#' @param volume_variable a numeric vector giving the modeled link volume
#' @param breaks breakpoints for the volumes vector in thousands
#'
#' @import dplyr lazyeval
#'
#' @return A data_frame with the volume variable recoded into factors
volume_levels <- function(df, volume_variable, breaks ){
  df %>%
    mutate_(
      .dots = setNames(
        list(lazyeval::interp(
          ~cut_volumes(x, b), x = as.name(volume_variable), b = breaks)),
        "Volume_Group")
    )
}
