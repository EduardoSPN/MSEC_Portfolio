
library(tidyverse)
library(R.matlab)
library(lubridate)
# install.packages("naniar") 
library(naniar)

library(dygraphs)
library(xts)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


min <- read.csv("DiD_data/gld_did_min.csv")

sta <- read.csv("DiD_data/gld_did_sta.csv")

boots_se <- read.csv("DiD_data/boots_se.csv")

#######################
######## PLOTS ########
#######################

min$date <- min$date %>% ymd()
head(min)


d <- xts(x = min$did_estimator, order.by = min$date)

lwr <- boots_se$gld[2]
upr <- boots_se$gld[3]

dygraph(d) %>% 
  dyOptions(stackedGraph = TRUE, fillAlpha = 0.4) %>% 
  dySeries(color = "green")







sta$date <- sta$date %>% ymd()
head(sta)


g <- xts(x = sta$did_estimator, order.by = sta$date)

dygraph(g) %>% 
  dyOptions(stackedGraph = TRUE, fillAlpha = 0.4) %>% 
  dySeries(color = "green")
