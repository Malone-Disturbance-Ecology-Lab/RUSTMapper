# Model Resultslibrary(randomForest)
library(tidyverse)
library(ggplot2)
library(gridExtra)

rm(list=ls())

load("/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/Final Scripts/RF_MODELFIT_Results_DAYMET.RDATA")

# invading .5 ####
rf.invading.5$terms
auc.invading.5  = performance( prediction(predict(rf.invading.5, type = "prob")[,2], train.5.invading$RUST), "auc")
auc.invading.5@y.values

test.5.invading$MODEL <- predict(rf.invading.5, test.5.invading, type = "prob") [,2]
# Accuracy:
test.5.invading$MODEL.c[test.5.invading$MODEL >= 0.50] <- 1
test.5.invading$MODEL.c[test.5.invading$MODEL < 0.50] <- 0
(length(test.5.invading$MODEL.c[ test.5.invading $MODEL.c == 1 & test.5.invading $RUST ==1]) + length(test.5.invading$MODEL.c[ test.5.invading$MODEL.c == 0 & test.5.invading$RUST ==0]) )/ length(test.5.invading $RUST) *100

#Invading.10 #####
rf.invading.10$terms
auc.invading.10  = performance( prediction(predict(rf.invading.10, type = "prob")[,2], train.10.invading$RUST), "auc")
auc.invading.10@y.values

test.10.invading$MODEL <- predict(rf.invading.10, test.10.invading, type = "prob") [,2]

test.10.invading$MODEL.c[test.10.invading$MODEL >= 0.50] <- 1
test.10.invading$MODEL.c[test.10.invading$MODEL < 0.50] <- 0

#Invading.20 #####
rf.invading.20$terms
auc.invading.20  = performance( prediction(predict(rf.invading.20, type = "prob")[,2], train.20.invading$RUST), "auc")
auc.invading.20@y.values

test.20.invading$MODEL <- predict(rf.invading.20, test.20.invading, type = "prob") [,2]

test.20.invading$MODEL.c[test.20.invading$MODEL >= 0.50] <- 1
test.20.invading$MODEL.c[test.20.invading$MODEL < 0.50] <- 0

(length(test.20.invading$MODEL.c[ test.20.invading$MODEL.c == 1 & test.20.invading$RUST ==1]) + length(test.20.invading$MODEL.c[ test.20.invading$MODEL.c == 0 & test.20.invading$RUST ==0]) )/ length(test.20.invading$RUST) *100


# established .5 ####
rf.established.5$terms
auc.established.5  = performance( prediction(predict(rf.established.5, type = "prob")[,2], train.5.established$RUST), "auc")
auc.established.5@y.values

test.5.established$MODEL <- predict(rf.established.5, test.5.established, type = "prob") [,2]
# Accuracy:
test.5.established$MODEL.c[test.5.established$MODEL >= 0.50] <- 1
test.5.established$MODEL.c[test.5.established$MODEL < 0.50] <- 0
(length(test.5.established$MODEL.c[ test.5.established $MODEL.c == 1 & test.5.established $RUST ==1]) + length(test.5.established$MODEL.c[ test.5.established$MODEL.c == 0 & test.5.established$RUST ==0]) )/ length(test.5.established $RUST) *100

#established.10 #####
rf.established.10$terms
auc.established.10  = performance( prediction(predict(rf.established.10, type = "prob")[,2], train.10.established$RUST), "auc")
auc.established.10@y.values

test.10.established$MODEL <- predict(rf.established.10, test.10.established, type = "prob") [,2]

test.10.established$MODEL.c[test.10.established$MODEL >= 0.50] <- 1
test.10.established$MODEL.c[test.10.established$MODEL < 0.50] <- 0

(length(test.10.established$MODEL.c[ test.10.established$MODEL.c == 1 & test.10.established$RUST ==1]) + length(test.10.established$MODEL.c[ test.10.established$MODEL.c == 0 & test.10.established$RUST ==0]) )/ length(test.10.established$RUST) *100


#established.20 #####
rf.established.20$terms
auc.established.20  = performance( prediction(predict(rf.established.20, type = "prob")[,2], train.20.established$RUST), "auc")
auc.established.20@y.values

test.20.established$MODEL <- predict(rf.established.20, test.20.established, type = "prob") [,2]

test.20.established$MODEL.c[test.20.established$MODEL >= 0.50] <- 1
test.20.established$MODEL.c[test.20.established$MODEL < 0.50] <- 0

(length(test.20.established$MODEL.c[ test.20.established$MODEL.c == 1 & test.20.established$RUST ==1]) + length(test.20.established$MODEL.c[ test.20.established$MODEL.c == 0 & test.20.established$RUST ==0]) )/ length(test.20.established$RUST) *100
