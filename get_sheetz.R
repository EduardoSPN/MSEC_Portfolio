library(rvest)
library(tidyverse)

base_url <- 'http://www2.stat.duke.edu/~sms185/data/fuel/bystore/zteehs/regions.html'
# Get the individual region URLs
page_links <- read_html(base_url) %>%
  html_nodes(css = "[href]") %>%
  html_attr(name = "href")
region_urls <- page_links[str_detect(page_links, "Page=")]
Sys.sleep(1)

# Access each of the "Region" links and extract the raw data there
for (i in 1:length(region_urls)) {
  
  raw_text <- read_html(region_urls[i]) %>%
    html_nodes(css = "body") %>%
    html_text() %>%
    saveRDS(file = str_c("data/sheetz/sheetz_", str_pad(i, 2, pad = "0"), ".rds"))
  Sys.sleep(1)
}
