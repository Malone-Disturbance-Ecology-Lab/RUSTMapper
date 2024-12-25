# Correlation Plots for Models:

library(corrplot)
library(tidyverse)

rm(list=ls())

# Load the data layers:
load("/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/Final Scripts/RF_MODELFIT_Results_DAYMET.RDATA")


corrMatrix <- function(data, vars){
  subset <- data[, vars[-c(1:2)] ] %>% na.omit()
  subset.corr <- cor(subset )
  return(subset.corr)
}

setwd("/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/Final Scripts/FIGURES")

png("CorrelationPlots_052023", width = 1100, height = 1000)
par(mfrow=c(3,3))

corrplot(corrMatrix(data.5years, vars.m.invading.5))
corrplot(corrMatrix(data.5years, vars.m.established.5))
corrplot(corrMatrix(data.5years, vars.m.all.5))

corrplot(corrMatrix(data.10years, vars.m.invading.10))
corrplot(corrMatrix(data.10years, vars.m.established.10))
corrplot(corrMatrix(data.10years, vars.m.all.10))

corrplot(corrMatrix(data.20years, vars.m.invading.20))
corrplot(corrMatrix(data.20years, vars.m.established.20))
corrplot(corrMatrix(data.20years, vars.m.all.20))
dev.off()


