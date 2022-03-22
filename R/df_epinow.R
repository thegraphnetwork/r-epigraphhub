#' Manipulate a dataset to use in an Epinow2 analysis
#'
#' \code{df_epinow} manipulates a dataset provided by the user to a dataset readily usable for Epinow2 analyses
#'
#' This function loads a \code{data.frame} object and makes some manipulation of it, in order to make it ready to be run in a Epinow2 local or regional analysis.
#'
#' @param x a vertical dataset typically used in ecological analyses.
#' @param cases the vector of cases in the provided dataset.
#' @param date the vector of dates in the format yyyy-mm-dd in the provided dataset.
#' @param regions if regional = TRUE, the vector of regions in the provided dataset.
#' @param period numeric. How many date periods should be left into the dataset?
#' @param regional logical. Does the user wants the dataset to be aggregated by regions?
#'
#' @examples \dontrun{
#' library("r-epigraphhub")
#' con <- egh_connection(auto_connect = TRUE, use_env = TRUE, path_env = ".env")
#' df_canton <- dbGetQuery(con, "SELECT datum, \"geoRegion\", entries FROM switzerland.foph_cases")
#' df <- df_epinow(df_canton, cases = "entries", date = "datum", regions = "geoRegion", regional = TRUE, period = 28)
#' #' egh_connection(auto_connect = TRUE, use_env = FALSE, user_db = "user", password_db = "password")
#' }
#' @export

df_epinow <- function(x, cases, date, regions, period = 14, regional = FALSE){

  require(dplyr)

  if (missing(cases)) stop("Vector of cases not provided.")
  if (missing(date)) stop("Vector of dates not provided.")

  cases <- enquo(cases)
  date <- enquo(date)

  if (regional == FALSE){
    x %>%
      rename(date = !!date,
             confirm = !!cases) %>%
      mutate(date = as.Date(date)) %>%
      select(date, confirm) %>%
      filter(date >= (max(date) - period)) %>%
      group_by(date) %>%
      summarise(confirm = sum(confirm, na.rm = TRUE)) %>%
      arrange(date) %>%
      as_tibble()
  } else if (regional == TRUE){

    if (missing(regions)) stop("Vector of regions not provided.")

    regions <- enquo(regions)

    x %>%
      rename(date = !!date,
             confirm = !!cases,
             regions = !!regions) %>%
      mutate(date = as.Date(date)) %>%
      select(date, confirm, regions) %>%
      filter(date >= (max(date) - period)) %>%
      group_by(date, regions) %>%
      summarise(confirm = sum(confirm, na.rm = TRUE)) %>%
      arrange(date, regions) %>%
      as_tibble()
  }
}

