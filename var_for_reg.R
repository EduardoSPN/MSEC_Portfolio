
library(tidyverse)
library(lubridate)


### REL

data <- read.csv("SPY_r.csv",header = TRUE)

dates_release <- data[,1]
head(dates_release)

days_rel <- substr(dates_release, 5, 8)
head(days_rel)

rel_1 <- rep(1, length(days_rel)) %>% as.data.frame() 
colnames(rel_1) <- c("DATE", "REL")

rel_1 <- cbind(data$ï..DATE, rel_1)


data <- read.csv("SPY_nr.csv",header = TRUE)

rel_2 <- rep(0, nrow(data)) %>% as.data.frame() 

rel_2 <- cbind(data$ï..DATE, rel_2)

colnames(rel_2) <- c("DATE", "REL")

rel_final <- rbind(rel_1, rel_2)

rel_final <- rel_final %>% arrange(DATE)

head(rel_final)

head(rel_final)

table(rel_final$REL)

write.csv(rel_final, "rel.csv")


### D_t

d <- data$TIME_M
head(d)

nchar(str_extract(d[1], "..?:"))


hours <- ifelse(nchar(str_extract(d, "..?:")) == 2, 
                str_extract(d, "^.{4}"), 
                str_extract(d, "^.{5}"))

d_t <- hours %>% mutate("Dt" = )

date <- d %>% as.POSIXct(format = "%H:%M")

hh <- date %>% hour()
hh <- hh*60

mm <- date %>% minute()

hhmm <- hh+mm %>% as.data.frame()
d <- d %>% as.data.frame()

head(hh)
head(mm)

d_t <- cbind(d, hhmm)
class(d_t)
head(d_t)

colnames(d_t) <- c("TIME_M", "hhmm")

dums <- str_c("d", seq(12))    

conds <- list(c(810, 815), c(815,820), c(820, 825), c(825, 830), c(830,835), c(835, 840), 
              c(840, 845), c(845, 850), c(850, 855), c(855, 860), c(860, 865), c(865,870))



c <- lapply(seq(dums), function(i){
  assign(dums[i], ifelse(hhmm>=conds[[i]][1] & hhmm<conds[[i]][2], 1, 0), envir = .GlobalEnv)
})

d_t <- d_t %>% as.data.frame()

dummies <- do.call(cbind, c) %>% as.data.frame()

dummies <- cbind(d, dummies)

head(dummies)

colnames(dummies) <- c("TIME_M", dums)

write.csv(dummies, "dum_interv.csv")
