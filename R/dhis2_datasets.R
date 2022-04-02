#' Get all currently available datasets from a DHIS2 server.
#'
#' \code{dhis2_datasets} queries a DHIS2 server and returns all available datasets.
#'
#' @param api_url the URL of the DHIS2 api. Usually it is the server URL ending with "/api/"
#' @param username the username required to access the DHIS2 server.
#' @param password the password required to access the DHIS2 server.
#'
#' @return A list with the unique id and names of each available dataset.
#'
#' @section Warning:
#' A Internet connection is needed to use this function.
#'
#' Watch out for hardcoded login and password.
#'
#' @examples \dontrun{
#' # Demo available at https://play.dhis2.org/demo/
#' library("r-epigraphhub")
#' api_url <- "https://play.dhis2.org/demo/api/"
#' username <- "admin"
#' password <- "district"
#' datasets <- dhis2_datasets(api_url = api_url, username = username, password = password)
#' datasets
#' }
#' @import httr
#' @export

dhis2_datasets <- function(api_url,
                           username,
                           password){
  # retrieving available datasets via API
  url1 <- "dataSets.csv?paging=false&links=false"
  # URL encode
  url <- paste0(api_url, url1)
  response <- httr::GET(url = url,
                        httr::authenticate(user = username,
                                           password = password))
  # retrieving datasets as a list
  dataSets <- cbind(httr::content(response,
                                  type = "text/csv",
                                  show_col_types = FALSE,
                                  encoding = "latin1"))
  dataSets <- as.list(dataSets)

}
