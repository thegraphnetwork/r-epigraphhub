#' Downloads all available data from one or more datasets from the World Bank database.
#'
#' \code{get_wb_data} generalizes the function \code{WDI::WDI()} to download the whole dataset instead of single indicators. It defaults to a starting date of 1960 up to 2022. It returns a list of data.frames.
#'
#' @param datasets a \code{wb_data} object or a vector of strings containing the names of the datasets to be downloaded.
#' @param start_year starting year to pull data from. Must be 1960 or higher.
#' @param end_year last year to pull data from. Must be higher than start_year.
#'
#' @section Warning:
#' An internet connection is needed to use this function.
#'
#' @examples \dontrun{
#' library("r-epigraphhub")
#' datasets <- wb_data()
#' my_dataset <- datasets[2]
#' my_data <- get_wb_data(my_dataset)
#'
#' my_dataset <- "Environment, Social and Governance (ESG) Data"
#' my_data <- get_wb_data(my_dataset)
#' }
#' @importFrom dplyr `%>%` as_tibble select
#' @import WDI
#' @export

get_wb_data <- function(datasets, start_year = 1960, end_year = 2022){

  if (start_year < 1960) stop("Minimum year must be 1960.")
  if (end_year < start_year) stop("Ending year should be equal or higher than the starting year.")

  # retrieving list of datasets
  if (is.list(datasets)){
    dataset_names <- names(datasets)
  } else {
    dataset_names <- datasets
  }
  # querying indicators
  df_WDI <- WDI_data$series %>%
    dplyr::as_tibble() %>%
    dplyr::select(sourceDatabase, indicator)
  dataset_list <- list()
  # looping through the API and get the required datasets
  for (i in 1:length(datasets)){
    dataset_list[[i]] <- WDI::WDI(country = "all",
                                  indicator = df_WDI %>% filter(df_WDI$sourceDatabase %in% dataset_names[i]) %>% select(indicator) %>% pull,
                                  start = start_year,
                                  end = end_year,
                                  extra = TRUE,
                                  language = "en")
  }
  dataset_list
}
