rm(list=ls())

library(sf)
library(terra)
library(tidyverse)
library(randomForest)
library(ggplot2)
library(caret)

# This Script produces a frequency layer and compares it to RF  rf.established.5df results:

data.dir <- "/Volumes/MaloneLab/Research/RUSTMAPPER"
figure.dir <- "/Users/sm3466/Dropbox (YSE)/Research/RUSTMapper/FIGURES"
setwd(data.dir)

load("RF_MODELFIT_Results_DAYMET.RDATA")
# Import Spatial  rf.established.5dfs:
# Testing
evaluations <- function(  model, df, threshold){
  
  df$rf.established.5df <- predict(model, df, type = "prob") [,2]
  
  df$ rf.established.5df.c[df$ rf.established.5df >=  threshold] <- 1
  df$ rf.established.5df.c[df$ rf.established.5df <  threshold] <- 0
  
  expected_value <- df$RUST %>% as.factor
  predicted_value <- df$ rf.established.5df.c %>%  as.factor
  
  #Creating confusion matrix
  example <- confusionMatrix(data=predicted_value, reference = expected_value)

  return.df <- example$overall %>% as.data.frame %>% round(2)
  names(return.df) <- 'data'
  return(return.df)
}


Val.established =evaluations(model = rf.established.5,
                    df= test.5.established,
                    threshold = 0.3) %>% mutate(test.5 = data) %>% select(test.5)

Val.established$test.10 =evaluations(model = rf.established.10,
                          df= test.10.established,
                          threshold = 0.3) %>% mutate(test.10 = data) %>% select(test.10)

Val.established$test.20 =evaluations(model = rf.established.20,
                                     df= test.20.established,
                                     threshold = 0.3) %>% mutate(test.20 = data) %>% select(test.20)

# 30% of the data was reserved for testing:
length(test.5$RUST)/ length(all.5$RUST)
length(test.10$RUST)/ length(all.10$RUST)
length(test.20$RUST)/ length(all.20$RUST)

# Invading:

Val.invading = evaluations(model = rf.invading.5,
                 df= train.5.invading,
                 threshold = 0.15) %>% mutate(train.5 = data) %>% select(train.5)

Val.invading$test.10 =evaluations(model = rf.invading.10,
                         df= test.10.invading,
                         threshold = 0.15) %>% mutate(test.10 = data) %>% select(test.10)
Val.invading$test.20 =evaluations(model = rf.invading.20,
                                  df= test.20.invading,
                                  threshold = 0.15) %>% mutate(test.20 = data) %>% select(test.20)

# Model Performance for traning: 

Val.established.train =evaluations(model = rf.established.5,
                             df= train.5.established,
                             threshold = 0.3) %>% mutate(train.5 = data) %>% select(train.5)
Val.established.train$train.10 =evaluations(model = rf.established.10,
                                      df= train.10.established,
                                      threshold = 0.3) %>% mutate(train.10 = data) %>% select(train.10)
Val.established.train$train.20 =evaluations(model = rf.established.20,
                                     df= train.20.established,
                                     threshold = 0.3) %>% mutate(train.20 = data) %>% select(train.20)


Val.invading.train = evaluations(model = rf.invading.5,
                           df= train.5.invading,
                           threshold = 0.15) %>% mutate(train.5 = data) %>% select(train.5)


Val.invading.train$train.20 =evaluations(model = rf.invading.20,
                                  df= train.20.invading,
                                  threshold = 0.15) %>% mutate(train.20 = data) %>% select(train.20)

Val.invading.train$train.10 =evaluations(model = rf.invading.10,
                                  df= train.10.invading,
                                  threshold = 0.15) %>% mutate(train.10 = data) %>% select(train.10)


