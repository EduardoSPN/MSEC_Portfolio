library(jsonlite)
library(tidyverse)

# Read the links as raw text
wd <- getwd()
sheetz_files <- list.files(path = str_c(wd, "/data/sheetz"), 
                           pattern = "\\d{2}.rds", full.names = T)
sheetz_raw <- map(sheetz_files, readRDS)

# fromJSON interprets the raw text saved in the RDS as JSON data, stores as messy dataframe
# Again, couple this with "safely" to avoid errors
parse_json <- safely(fromJSON)
sheetz_regions <- map(sheetz_raw, function(x) { parse_json(x)$result })

# Perform a few operations on each of the dataframes individually, then merge them together
for (i in 1:length(sheetz_regions)) {
  
  sheetz_regions[[i]] <- sheetz_regions[[i]] %>%
    # Change NULL values to NA (excluding the columns with dataframes as entries)
    mutate_at(vars(-fuelPrice, -storeAttribute),
              function(v) { lapply(v, function(e) { ifelse(is.null(e), NA, e) }) }) %>%
    # A lot of values get stored as matrices, so change those to single values
    mutate_at(vars(-fuelPrice, -storeAttribute), function(v) { sapply(v, as.vector) })
  
  # Pull out the variables within the storeAttribute dataframe, parse it like above
  storeAttributes <- sheetz_regions[[i]]$storeAttribute %>%
    mutate_all(function(v) { lapply(v, function(e) { ifelse(is.null(e), NA, e) }) }) %>%
    mutate_all(function(v) { sapply(v, as.vector) }) %>%
    # We already have a storeNumber attribute
    select(-storeNumber)
  sheetz_regions[[i]] <- cbind(sheetz_regions[[i]], storeAttributes)
  
  # Pull out the list of dataframes in fuelPrice, make grades into columns, with price as value
  modify_fuelPrice <- function(fuel_df) {
    # Pretend the NULL rows are a dataframe like the others
    if (is.null(fuel_df)) {
      return (data.frame(Regular = NA))
    }
    flat_df <- fuel_df %>%
      select(-lastUpdatedDate) %>%
      spread(key = gradeDescription, value = price)
    return (flat_df)
  }
  
  fuelPrice <- sheetz_regions[[i]]$fuelPrice %>%
    lapply(modify_fuelPrice) %>%
    bind_rows %>%
    mutate_all(function(v) { lapply(v, function(e) { ifelse(is.null(e), NA, e) }) }) %>%
    mutate_all(function(v) { sapply(v, as.vector) })
  sheetz_regions[[i]] <- cbind(sheetz_regions[[i]], fuelPrice)
    
  
  sheetz_regions[[i]] <- sheetz_regions[[i]] %>%
    select(-fuelPrice, -storeAttribute) %>%
    # Filter out any rows with NA in every row (from using {} in the raw JSON data)
    filter(!is.na(storeNumber))
}
  
sheetz_df <- as_tibble(bind_rows(sheetz_regions))
saveRDS(sheetz_df, file = str_c(wd, "/data/sheetz/sheetz.rds"))



