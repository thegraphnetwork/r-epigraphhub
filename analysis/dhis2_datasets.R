# author: Daniel Câmara
# date: March 10th, 2022

# dhis2 api access using the demo available at https://play.dhis2.org/demo/

dhis2_datasets <- function(api_url,
                           username,
                           password){
  
  # loading libraries
  suppressPackageStartupMessages(library(httr))
  suppressPackageStartupMessages(library(tidyverse))
  
  # retrieving available datasets via API
  url1 <- "dataSets.csv?paging=false&links=false"
  
  # URL encode
  url <- paste0(api_url, url1)
  response <- GET(url = url, 
                  authenticate(user = username, 
                               password = password))
  # retrieving datasets as a list
  dataSets <- cbind(content(response, 
                            type = "text/csv",
                            show_col_types = FALSE,
                            encoding = "latin1")) %>% 
    as.list()
  
}

# End

# api_url = "https://play.dhis2.org/demo/api/"
# username = "admin"
# password = "district"
# df <- dhis2_datasets(api_url = api_url, 
#                      username = username, 
#                      password = password)
# print(df)