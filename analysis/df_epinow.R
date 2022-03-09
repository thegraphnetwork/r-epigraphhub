# author: Daniel Câmara
# date: March 3rd, 2022

# libraries used
library(tidyverse)

# function to prepare the user dataset to use inside epinow function
df_epinow <- function(x, cases, date, region, period = 14, regional = FALSE){
  
  date <- enquo(date)
  cases <- enquo(cases)
  region <- enquo(region)
  
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
    x %>% 
      rename(date = !!date,
             confirm = !!cases,
             region = !!region) %>%
      mutate(date = as.Date(date)) %>%
      select(date, confirm, region) %>%
      filter(date >= (max(date) - period)) %>% 
      group_by(date, region) %>% 
      summarise(confirm = sum(confirm, na.rm = TRUE)) %>% 
      arrange(date, region) %>% 
      as_tibble()  
  }
}

