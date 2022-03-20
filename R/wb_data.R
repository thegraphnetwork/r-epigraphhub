#' Get all currently available datasets and indicators from World Bank database.
#'
#' \code{wb_data} pulls all datasets and the respective indicators currently available from World Bank database. It returns an object consisting of a list of datasets and sublists of indicators. This object can be used to download the data using the \code{get_wb_data} function
#'
#' @section Warning:
#' An internet connection is needed to use this function.
#' 
#' @examples \dontrun{
#' library("r-epigraphhub")
#' datasets <- wb_data()
#' my_dataset <- datasets[2]
#' my_dataset
#' }
#' @export

wb_data <- function(){
    require(dplyr)
    require(WDI)
    # querying all datasets available
    df_WDI <- WDI_data$series %>%
        as_tibble()
    # list of datasets
    dataset_names <- df_WDI %>%
        select(sourceDatabase) %>%
        unique %>%
        pull
    # creating a list of datasets and their respective indicators
    datasets <- dataset_names %>%
        as.list()
    names(datasets) <- datasets
    # pulling list of datasets and indicators into the list
    for (i in dataset_names){
        datasets[[i]] <- df_WDI %>%
            filter(sourceDatabase %in% dataset_names[dataset_names == i]) %>%
            select(indicator) %>%
            pull
    }
    datasets
}
