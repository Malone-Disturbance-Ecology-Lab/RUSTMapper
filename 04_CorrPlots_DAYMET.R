# Correlation Plots for Models:

library(corrplot)
library(tidyverse)

rm(list=ls())

data.dir <- "/Volumes/MaloneLab/Research/RUSTMAPPER"
figure.dir <- "/Users/sm3466/Dropbox (YSE)/Research/RUSTMapper/FIGURES"
setwd(data.dir)

# Load the data layers:
load("RF_MODELFIT_Results_DAYMET.RDATA")


corrMatrix <- function(data, vars){
  subset <- data[, vars[-c(1:2)] ] %>% na.omit()
  subset.corr <- cor(subset )
  return(subset.corr)
}

setwd(figure.dir)

png("CorrelationPlots_052023.png", width = 1100, height = 1000)
par(mfrow=c(3,2))

corrplot(corrMatrix(data.5years, vars.m.invading.5))
corrplot(corrMatrix(data.5years, vars.m.established.5))

corrplot(corrMatrix(data.10years, vars.m.invading.10))
corrplot(corrMatrix(data.10years, vars.m.established.10))

corrplot(corrMatrix(data.20years, vars.m.invading.20))
corrplot(corrMatrix(data.20years, vars.m.established.20))
dev.off()


