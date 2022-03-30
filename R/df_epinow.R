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
#' library("epigraphhub")
#' con <- egh_connection(auto_connect = TRUE, use_env = TRUE, path_env = ".env")
#' df_canton <- dbGetQuery(con, "SELECT datum, \"geoRegion\", entries FROM switzerland.foph_cases")
#' df <- df_epinow(df_canton, cases = "entries", date = "datum", regions = "geoRegion", regional = TRUE, period = 28)
#' }
#' @import dplyr
#' @importFrom rlang enquo
#' @export

df_epinow <- function(x, cases, date, regions, period = 14, regional = FALSE){

  if (missing(x)) stop("Dataset not provided.")
  if (missing(cases)) stop("Vector of cases not provided.")
  if (missing(date)) stop("Vector of dates not provided.")
  # if (regional == TRUE & missing(regions)) stop("Vector of regions not provided.")

  cases <- rlang::enquo(cases)
  date <- rlang::enquo(date)

  if (regional == FALSE){
    x %>%
      dplyr::rename(date = !!date,
             confirm = !!cases) %>%
      dplyr::mutate(date = as.Date(date)) %>%
      dplyr::select(date, confirm) %>%
      dplyr::filter(date >= (max(date) - period)) %>%
      dplyr::group_by(date) %>%
      dplyr::summarise(confirm = sum(confirm, na.rm = TRUE)) %>%
      dplyr::arrange(date) %>%
      dplyr::as_tibble()
  } else if (regional == TRUE){

    if (missing(regions)) stop("Vector of regions not provided.")

    regions <- rlang::enquo(regions)

    x %>%
      dplyr::rename(date = !!date,
             confirm = !!cases,
             regions = !!regions) %>%
      dplyr::mutate(date = as.Date(date)) %>%
      dplyr::select(date, confirm, regions) %>%
      dplyr::filter(date >= (max(date) - period)) %>%
      dplyr::group_by(date, regions) %>%
      dplyr::summarise(confirm = sum(confirm, na.rm = TRUE)) %>%
      dplyr::arrange(date, regions) %>%
      dplyr::as_tibble()
  }
}

