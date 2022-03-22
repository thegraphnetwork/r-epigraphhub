#' Downloads a dataset from a DHIS2 server. Currently works on datasets organized weekly or monthly.
#'
#' \code{dhis2_import} downloads a dataset from a DHIS2 server. It works on datasets that are organized weekly or monthly.
#'
#' @param df a \code{dhis2_datasets} object.
#' @param dataset string. The name of an available dataset from a DHIS2 server obtained from a \code{dhis2_datasets} object.
#' @param api_url the URL of the DHIS2 api. Usually it is the server URL ending with "/api/"
#' @param username the username required to access the DHIS2 server.
#' @param password the password required to access the DHIS2 server.
#' @param year_initial the initial year available in the dataset to import data from.
#' @param year_final the last year available in the dataset to import data from.
#' @param period either "weekly" or "monthly" periods are supported.
#'
#' @return A dataset containing all available data.
#'
#' @section Warning:
#' A Internet connection is needed to use this function. It might take a while depending on the connection.
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
#' my_dataset <- datasets$displayName[2]
#' df_dhis2 <- dhis2_import(df = datasets, dataset = my_dataset, api_url = api_url, username = username, password = password, year_initial = 2021, year_final = 2022, period = "monthly")
#' head(df_dhis2)
#' }
#' @import dplyr
#' @import stringr
#' @import httr
#' @importFrom jsonlite fromJSON
#' @importFrom readr cols
#' @export

dhis2_import <- function(df,
                         dataset,
                         api_url,
                         username,
                         password,
                         year_initial,
                         year_final,
                         period = c("weekly", "monthly")){
  # defining dataset unique id ---------------------------------------------------------------------
  dataset_id <- rbind.data.frame(df)
  dataset_id <- dplyr::filter(dataset_id, (displayName %in% dataset))
  dataset_id <- dplyr::pull(dplyr::select(dataset_id, id))

  # defining periods -------------------------------------------------------------------------------
  year_seq <- seq(year_initial, year_final)
  # monthly period ---------------------------------------------------------------------------------
  period_monthly <- list()
  # retrieving all months per year in the sequence
  for (i in 1:length(year_seq)){
    period_monthly[[i]] <- paste(paste0(year_seq[i],
                                        sprintf("%02d", seq(from = 1, to = 12, by = 1)),
                                        collapse = ";"),
                                 sep=";")
  }
  # collapsing period to pass to the query
  period_monthly <- unlist(period_monthly)
  period_monthly <- paste(period_monthly, collapse = ";")
  # weekly period ----------------------------------------------------------------------------------
  period_weekly <- list()
  # retrieving all weeks per year in the sequence
  for (i in 1:length(year_seq)){
    period_weekly[[i]] <- paste(paste0(year_seq[i],
                                       "W",
                                       seq(from = 1, to = 53, by = 1),
                                       collapse=";"),
                                sep=";")
  }
  # collapsing period to pass to the query
  period_weekly <- unlist(period_weekly)
  period_weekly <- paste(period_weekly, collapse = ";")

  # urls for queries--------------------------------------------------------------------------------
  # url for dataElements
  url_var <- paste0(api_url,
                    "dataSets/",
                    dataset_id,
                    "?fields=id,name,dataSetElements[dataElement[id,name]]")
  # url for indicators
  url_ind <- paste0(api_url,
                    "dataSets/",
                    dataset_id,
                    "?fields=id,name,indicators")
  # url for organisationUnits
  url_ou <- paste0(api_url,
                   "dataSets/",
                   dataset_id,
                   "?fields=id,name,organisationUnits")

  # getting metadata -------------------------------------------------------------------------------
  response_var <- httr::GET(url = url_var,
                            httr::authenticate(user = username,
                                               password = password))
  response_ind <- httr::GET(url = url_ind,
                            httr::authenticate(user = username,
                                               password = password))
  response_ou <- httr::GET(url = url_ou,
                           httr::authenticate(user = username,
                                              password = password))

  # querying metadata as JSON objects --------------------------------------------------------------
  r_var <- cbind(httr::content(response_var,
                               type = "text",
                               show_col_types = FALSE,
                               encoding = "latin1"))
  r_ind <- cbind(httr::content(response_ind,
                               type = "text",
                               show_col_types = FALSE,
                               encoding = "latin1"))
  r_ou <- cbind(httr::content(response_ou,
                              type = "text",
                              show_col_types = FALSE,
                              encoding = "latin1"))

  # converting JSON to R objects -------------------------------------------------------------------
  d_var <- jsonlite::fromJSON(r_var, flatten=TRUE)
  d_ind <- jsonlite::fromJSON(r_ind, flatten=TRUE)
  d_ou <- jsonlite::fromJSON(r_ou, flatten=TRUE)

  # pulling and organizing dataElements unique id
  varID <- split(d_var$dataSetElements$dataElement.id,
                 # to avoid fatal curl HTTP/2 error, queries should be limited to 500 objects at a time
                 ceiling(seq_along(d_var$dataSetElements$dataElement.id)/500))
  # looping through each batch of 500 objects and creating different query structures
  varID <- purrr::map(.x = varID,
                      ~ paste(.x, collapse = ";"))

  # pulling and organizing indicators unique id
  indID <- split(d_ind$id,
                 ceiling(seq_along(d_ind$id)/500))
  indID <- purrr::map(.x = indID,
                      ~ paste(.x, collapse = ";"))

  # pulling and organizing organisationUnits unique id
  ouID <- split(d_ou$organisationUnits$id,
                ceiling(seq_along(d_ou$organisationUnits$id)/500))
  ouID <- purrr::map(.x = ouID,
                     ~ paste(.x, collapse = ";"))

  # building the url -------------------------------------------------------------------------------
  urlA <- paste0(api_url, "analytics.csv?")
  urlB <- paste0("dimension=pe:",
                 # defining period of the query
                 if (period == "weekly"){
                   period_weekly} else {
                     period_monthly
                   })
  urlC <- "rows=ou%3Bpe&columns=dx&completedOnly=false&displayProperty=NAME&skipMeta=false"

  # looping through the DHIS2 platform and pulling data --------------------------------------------
  df2 <- purrr::map2(varID, ouID, function(x, y){
    # urls for importing data
    url <- URLencode(paste(paste0(urlA,
                                  urlB),
                           paste0("dimension=dx:", x),
                           paste0("dimension=ou:", y),
                           urlC,
                           sep="&"))
    # pulling the data
    r <- httr::GET(url,
                   httr::authenticate(user = username,
                                      password = password),
                   httr::timeout(1000))
    # converting into a csv structure
    r <- httr::content(r,
                       type = "text/csv",
                       show_col_types = FALSE,
                       col_types = readr::cols(.default = "c"),
                       encoding = "latin1")
  }
  )

  df2 <- data.table::rbindlist(df2)
  df2 <- dplyr::as_tibble(df2)

  # searching for error E7115 ----------------------------------------------------------------------
  # this errors refers to variables that can't be aggregated. So we will identify and remove them
  error_message <- if_else(is.na(stringr::str_extract(names(df2), "errorCode:E7115")),
                           "No error",
                           "errorCode:E7115")
  error_message <- if_else(any(error_message == "errorCode:E7115"),
                           "errorCode:E7115",
                           "No error")

  # testing for error
  if (error_message == "No error"){
    message("No errorCode:E7115 found. Whole dataset being retrieved")
    return(df2)
  } else {
    # Detecting variables that are producing the error E7115 and removing them ---------------------
    # extracting message
    vars_remove1 <- paste(names(df2), collapse = ",")
    # detecting the string pattern that contains variables
    vars_remove2 <- stringr::str_locate_all(vars_remove1, "\\[|\\]\\`") # "\\`(?=\\[)|(?=\\])"
    # cutting around special characters that contains variables
    vars_remove3 <- stringr::str_sub(vars_remove1, vars_remove2[[1]][1], vars_remove2[[1]][2])
    # isolating variables
    vars_remove4 <- stringr::str_remove_all(vars_remove3, "\\[|\\]|\\s")
    # pulling variables to remove them from the variables
    vars_remove5 <- stringr::str_split(vars_remove4, pattern = ",")
    vars_remove6 <- unlist(vars_remove5)
    vars_remove7 <- paste(vars_remove6, collapse = "|")
    # updating varID_new object removing the problematic variables
    varID_new <- stringr::str_remove_all(varID, vars_remove7)
    varID_new <- stringr::str_replace_all(varID_new, pattern = ";;", replacement = ";")

    # looping again --------------------------------------------------------------------------------
    df3 <- purrr::map2(varID_new, ouID, function(x, y){
      url <- URLencode(paste(paste0(urlA,
                                    urlB),
                             paste0("dimension=dx:", x),
                             paste0("dimension=ou:", y),
                             urlC,
                             sep="&"))
      r <- httr::GET(url,
                     httr::authenticate(user = username,
                                        password = password),
                     httr::timeout(1000))
      r <- httr::content(r,
                         type = "text/csv",
                         show_col_types = FALSE,
                         col_types = readr::cols(.default = "c"),
                         encoding = "latin1")
    }
    )

    # returning updated dataframe
    df3 <- data.table::rbindlist(df3)
    df3 <- dplyr::as_tibble(df3)
    message(paste0("errorCode:E7115 found. Retrieving dataset without the following variables: ", vars_remove4))
    return(df3)
  }

}
