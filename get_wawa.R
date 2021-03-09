library(httr)
library(tidyverse)

'http://www2.stat.duke.edu/~sms185/data/fuel/bystore/awaw/awawstore=08078.json'
base_url <- 'http://www2.stat.duke.edu/~sms185/data/fuel/bystore/awaw/awawstore='

possible_stores <- c(str_pad(0:1000, 5, pad = "0"), str_pad(8000:9000, 5, pad = "0"))
possible_urls <- str_c(base_url, possible_stores, ".json")

# Iterate through each of the possible IDs
for (i in 1:length(possible_stores)) {
  
  response <- GET(possible_urls[i])
  # Check if the URL exists (if so, there should be data)
  if (status_code(response) == 200) {
    # Assumes the Makefile has successfully created these directories
    saveRDS(object = response,
            file = str_c("data/wawa/wawa_", possible_stores[i], ".rds"))
  }
  Sys.sleep(0.25)
}
