# Model Results 

library(tidyverse)
library(ggplot2)
library(gridExtra)
library(randomForest)

rm(list=ls())

data.dir <- "/Volumes/MaloneLab/Research/RUSTMAPPER"
figure.dir <- "/Users/sm3466/Dropbox (YSE)/Research/RUSTMapper/FIGURES"

setwd(data.dir)

load("RF_MODELFIT_Results_DAYMET.RDATA")


rf.invading.5$terms
rf.invading.10$terms
rf.invading.20$terms

rf.established.5$terms
rf.established.10$terms
rf.established.20$terms

# Model correlations:
           
eval <- function( model,train, test, label){

  oob.5 <- model$err.rate[500,1] *100
  auc.invading.5  = performance( prediction(predict(model, type = "prob")[,2], train$RUST), "auc")
  auc.5 <- auc.invading.5@y.values
  train$MODEL <- predict(model, train, type = "prob") [,2]
  # Accuracy:
  train$MODEL.c[train$MODEL >= 0.50] <- 1
  train$MODEL.c[train$MODEL < 0.50] <- 0
  accuracy.train.5 <- (length(train$MODEL.c[ train $MODEL.c == 1 & train $RUST ==1]) + length(train$MODEL.c[ train$MODEL.c == 0 & train$RUST ==0]) )/ length(train $RUST) *100
  
  
  test$MODEL <- predict(model, test, type = "prob") [,2]
  # Accuracy:
  test$MODEL.c[test$MODEL >= 0.50] <- 1
  test$MODEL.c[test$MODEL < 0.50] <- 0
  accuracy.test.5 <- (length(test$MODEL.c[ test $MODEL.c == 1 & test $RUST ==1]) + length(test$MODEL.c[ test$MODEL.c == 0 & test$RUST ==0]) )/ length(test $RUST) *100
  
out <- data.frame( OOB= oob.5,
                   AUC = auc.5[[1]], 
                   Accuracy.train = accuracy.train.5, 
                   Accuracy.test = accuracy.test.5,
                   row.names= label)
  return(out)
  
}

inv.5.val <- eval(model = rf.invading.5,
                  train = train.5.invading,
                  test = test.5.invading,
                  label = "INV5")

est.5.val <- eval(model = rf.established.5,
                  train = train.5.established,
                  test = test.5.established,
                  label="EST5")

inv.10.val <- eval(model = rf.invading.10,
                  train = train.10.invading,
                  test = test.10.invading,
                  label = "INV10")

est.10.val <- eval(model = rf.established.10,
                  train = train.10.established,
                  test = test.10.established,
                  label="EST10")

inv.20.val <- eval(model = rf.invading.20,
                  train = train.20.invading,
                  test = test.20.invading,
                  label = "INV20")

est.20.val <- eval(model = rf.established.20,
                  train = train.20.established,
                  test = test.20.established,
                  label="EST20")


validation <- rbind(inv.5.val , inv.10.val, inv.20.val,
      est.5.val , est.10.val, est.20.val) %>% t() %>% round(2)

write.csv(validation, paste(data.dir,"/MODEL_Validation.csv", sep=""))

