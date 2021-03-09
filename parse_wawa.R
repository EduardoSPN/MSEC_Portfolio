library(httr)
library(jsonlite)
library(tidyverse)

# We read the data and put it into a dataframe
wd <- getwd()
wawa_files <- list.files(path = str_c(wd, "/data/wawa"), pattern = "\\d{5}.rds", full.names = T)
wawa_df <- wawa_files %>%
  map(readRDS) %>%
  map(content, as = "text", encoding = "UTF-8") %>%
  map(fromJSON) %>%
  # Flatten all nested entries
  map(function(x) { as.list(unlist(x)) }) %>%
  bind_rows
  

# This dataframe is mostly in good shape, just need to rearrange fuelType info
descriptions <- str_c("fuelTypes.description", 1:8)
prices <- str_c("fuelTypes.price", 1:8)
for (i in 1:8) {
  # Make one column for each type of fuel
  wawa_df <- wawa_df %>%
    spread(key = descriptions[i], value = prices[i])
}

# Remove the fuelTypes.currency column, and the column made from all the NA values
wawa_df <- wawa_df %>%
  select(-starts_with("fuelTypes"), -"<NA>")

saveRDS(wawa_df, file = str_c(wd, "/data/wawa/wawa.rds"))


