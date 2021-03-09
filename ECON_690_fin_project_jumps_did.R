
library(tidyverse)
library(R.matlab)
library(lubridate)
# install.packages("naniar") 
library(naniar)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


####################################################
######## LOAD DATA - DIFF IN SPOT VOLAT. ###########
####################################################

# This is already the (log) difference in spot volat before and after 2 pm
data_sv <- readMat("DiD_data/uup_sv.mat")
str(data_sv)

data_num_sv <- data_sv$sv %>% as.data.frame()

data_num_sv <- data_num_sv %>% replace_with_na(replace = list(V3=c(Inf, -Inf)))

data_num_sv[is.na(data_num_sv[,3]==-Inf),3] <- NA

##########################################################
######## COMPUTE AVERAGES FOR NON-RELEASE DAYS ###########
##########################################################

# Average of diff in spot volatility for non-release days


minu_rel_days  <- data.frame(minutes_2015 = c(20150218, 20150408, 20150520, 20150708, 20150819, 20151008, 20151118, 20160106),
                   minutes_2016 = c(20160217, 20160406, 20160518, 20160706, 20160817, 20161012, 20161123, 20170104),
                   minutes_2017 = c(20170222, 20170405, 20170524, 20170705, 20170816, 20171011, 20171122, 20180103),
                   minutes_2018 = c(20180221, 20180411, 20180523, 20180705, 20180822, 20181017, 20181129, 20190109),
                   minutes_2019 = c(20190220, 20190410, 20190522, 20190710, 20190821, 20191009, 20191120, 20200103),
                   minutes_2020 = c(20200219, 20200408, 20200520, 20200701, 20200819, 20201007, NA, NA))

sta_rel_days <- data.frame(statements_2015 = c(20150128, 20150318, 20150429, 20150617, 20150729, 20150917, 20151028, 20151216, NA),
                                statements_2016 = c(20160127, 20160316, 20160427, 20160615, 20160727, 20160921, 20161102, 20161214, NA),
                                statements_2017 = c(20170201, 20170315, 20170503, 20170614, 20170726, 20170929, 20171101, 20171213, NA),
                                statements_2018 = c(20180131, 20180321, 20180502, 20180613, 20180801, 20180926, 20181108, 20181219, NA),
                                statements_2019 = c(20190130, 20190320, 20190501, 20190619, 20190731, 20190918, NA, 20191030, 20191211),
                                statements_2020 = c(20200129, NA, NA, 20200323, 20200429, 20200610, 20200729, 20200916, 20201105))



#### Minutes #####

list_minu <- minu_rel_days %>% unlist %>% as.numeric

# Check if we actually have data for these days
setdiff(list_minu , data_num_sv$V1)

list_names_minu <- list_minu %>% as.character()

l_minu <- lapply(list_minu, function(i){
  c <- i %>% as.character()
  temp_1 <- ymd(i)
  temp_2 <- (ymd(i) - 30)
  d <- data_num_sv %>% filter(ymd(V1)<temp_1 & ymd(V1)>=temp_2) %>% select(V3)
  assign(list_names_minu, mean(d[,1], na.rm = TRUE))
})

names(l_minu) <- list_names_minu 

avg_minu_non_rel <- do.call(rbind, l_minu) %>% as.data.frame() %>% na.exclude()

rownames(avg_minu_non_rel) <- str_remove(rownames(avg_minu_non_rel), "X")


#### Statements ####

list_sta <- sta_rel_days %>% unlist %>% as.numeric

# Check if we actually have data for these days
setdiff(list_sta , data_num_sv$V1)

list_names_sta <- list_sta %>% as.character()

l_sta <- lapply(list_sta, function(i){
  c <- i %>% as.character()
  temp_1 <- ymd(i)
  temp_2 <- (ymd(i) - 30)
  d <- data_num_sv %>% filter(ymd(V1)<temp_1 & ymd(V1)>=temp_2) %>% select(V3)
  assign(list_names_sta, mean(d[,1], na.rm = TRUE))
})

names(l_sta) <- list_names_sta

avg_sta_non_rel <- do.call(rbind, l_sta) %>% as.data.frame() %>% na.exclude()

rownames(avg_sta_non_rel) <- str_remove(rownames(avg_sta_non_rel), "X")

# temp1 <- ymd(list_sta[37])
# temp2 <- ymd(list_sta[37]) - 20
# 
# data_num_sv %>% filter(ymd(V1)<temp1 & ymd(V1)>=temp2) %>% select(V3)

############################################
######## COMPUTE JUMPS - MINUTES ###########
############################################

release_index_minu <- match(list_minu, data_num_sv$V1)
  
sv_release_minu <- data_num_sv[release_index_minu, c(1,3)] %>% na.exclude()

data_jumps_rel_minu <- cbind(sv_release_minu, avg_minu_non_rel)

colnames(data_jumps_rel_minu) <- c("date", "spot_volat_rel", "avg_sv_non_rel")

data_jumps_rel_minu <- data_jumps_rel_minu %>% mutate(did_estimator = spot_volat_rel - avg_sv_non_rel)

plot(data_jumps_rel_minu$did_estimator, type = 'l')

############################################
######## COMPUTE JUMPS - STATEMENTS ########
############################################

release_index_sta <- match(list_sta, data_num_sv$V1)

sv_release_sta <- data_num_sv[release_index_sta, c(1,3)] %>% na.exclude()

data_jumps_rel_sta <- cbind(sv_release_sta, avg_sta_non_rel)

colnames(data_jumps_rel_sta) <- c("date", "spot_volat_rel", "avg_sv_non_rel")

data_jumps_rel_sta <- data_jumps_rel_sta %>% mutate(did_estimator = spot_volat_rel - avg_sv_non_rel)

plot(data_jumps_rel_sta$did_estimator, type = 'l')

############################
######## STORE DATA ########
############################

write.csv(data_jumps_rel_minu, file = "DiD_data_2/uup_did_min.csv")

write.csv(data_jumps_rel_sta, file = "DiD_data_2/uup_did_sta.csv")



