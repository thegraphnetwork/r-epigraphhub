# author: Daniel Câmara
# date: March 16th, 2022

library(WDI)
library(tidyverse)

# querying all datasets available
df_WDI <- WDI_data$series %>% 
  as_tibble()

# list of datasets
dataset_names <- df_WDI %>% 
  select(sourceDatabase) %>% 
  unique %>% 
  pull

# list to pull WDB data into
dataset_list <- list()

# looping to go through the API and get all the data
for (i in 1:length(dataset_names)){
  dataset_list[[i]] <- WDI::WDI(country = "all", 
                                indicator = df_WDI %>% filter(df_WDI$sourceDatabase %in% dataset_names[i]) %>% select(indicator) %>% pull, 
                                start = 1960, 
                                end = 2022, 
                                extra = TRUE, 
                                language = "en")
}

# End