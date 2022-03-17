# author: Daniel Câmara
# date: March 11th, 2022

dhis2_import <- function(
  df, # a dhis2_datasets object
  dataset, # a dhis2 dataset name
  api_url, # url to the dhis2 platform api
  username, # login
  password, # password
  year_initial,
  year_final,
  period = c("weekly", "monthly")){
  
  # loading libraries
  suppressPackageStartupMessages(library(tidyverse))
  suppressPackageStartupMessages(library(httr))
  suppressPackageStartupMessages(library(jsonlite))
  suppressPackageStartupMessages(library(purrr))
  suppressPackageStartupMessages(library(data.table))
  
  # defining dataset unique id ---------------------------------------------------------------------
  dataset_id <- df %>%
    rbind.data.frame() %>%
    filter(displayName %in% dataset) %>%
    select(id) %>%
    pull()
  
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
  period_monthly <- period_monthly %>% 
    unlist() %>% 
    paste(collapse = ";")
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
  period_weekly <- period_weekly %>% unlist() %>% paste(collapse = ";")
  
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
  response_var <- GET(url = url_var, 
                      authenticate(user = username, 
                                   password = password))
  response_ind <- GET(url = url_ind, 
                      authenticate(user = username, 
                                   password = password))
  response_ou <- GET(url = url_ou, 
                     authenticate(user = username, 
                                  password = password))
  
  # querying metadata as JSON objects --------------------------------------------------------------
  r_var <- cbind(content(response_var, 
                         type = "text",
                         show_col_types = FALSE,
                         encoding = "latin1"))
  r_ind <- cbind(content(response_ind, 
                         type = "text",
                         show_col_types = FALSE,
                         encoding = "latin1"))
  r_ou <- cbind(content(response_ou, 
                        type = "text",
                        show_col_types = FALSE,
                        encoding = "latin1"))
  
  # converting JSON to R objects -------------------------------------------------------------------
  d_var <- jsonlite::fromJSON(r_var, flatten=TRUE)
  d_ind <- jsonlite::fromJSON(r_ind, flatten=TRUE)
  d_ou <- jsonlite::fromJSON(r_ou, flatten=TRUE)
  
  # organizing metadata for API query --------------------------------------------------------------
  
  # pulling and organizing dataElements unique id
  varID <- d_var$dataSetElements$dataElement.id %>% 
    # to avoid fatal curl HTTP/2 error, queries should be limited to 500 objects at a time
    split(., ceiling(seq_along(.)/500))
  # looping through each batch of 500 objects and creating different query structures
  varID <- purrr::map(.x = varID,
             ~ paste(.x, collapse = ";"))
  
  # pulling and organizing indicators unique id
  indID <- d_ind$id %>% 
    # to avoid fatal curl HTTP/2 error, queries should be limited to 500 objects at a time
    split(., ceiling(seq_along(.)/500))
  # looping through each batch of 500 objects and creating different query structures
  indID <- purrr::map(.x = indID,
             ~ paste(.x, collapse = ";"))
  
  # pulling and organizing organisationUnits unique id
  ouID <- d_ou$organisationUnits$id %>% 
    # to avoid fatal curl HTTP/2 error, queries should be limited to 500 objects at a time
    split(., ceiling(seq_along(.)/500))
  # looping through each batch of 500 objects and creating different query structures
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
                   authenticate(user = username, 
                                password = password),
                   httr::timeout(1000))
    # converting into a csv structure
    r <- httr::content(r, 
                       type = "text/csv",
                       show_col_types = FALSE,
                       col_types = cols(.default = "c"),
                       encoding = "latin1")
  }
  )
  
  data.table::rbindlist(df2) %>% 
    as_tibble()
  
}

# End

