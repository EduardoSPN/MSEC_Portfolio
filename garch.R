
# install.packages('aTSA')
library(rugarch)
library(foreign)
library(haven)
library(aTSA)
library(tidyverse)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

d <- read_dta('ief.dta')
ief_resid <- d$e %>% ts()


write.csv(ief_resid, "ief_e.csv")

arima()
# arch.test(ief_resid, output = TRUE)
# ARCH TEst in Matlab

# External Regressors
rel <- d$REL

interact <- str_c("rel_","d", seq(12))   

ext_reg <- lapply(seq(12), function(i){
  assign(interact[i], rel*d[,paste0("D", i)])
})

ext_reg <- do.call(cbind, ext_reg) %>% as.data.frame()

ext_reg <- cbind(rel, ext_reg)

colnames(ext_reg) <- c("REL", interact)

# GARCH model
spec = ugarchspec(variance.model = list(external.regressors = cbind(z)), mean.model = list(armaOrder = c(0, 0), external.regressors = cbind(x)))
fit = ugarchfit(spec = spec, data = y)
