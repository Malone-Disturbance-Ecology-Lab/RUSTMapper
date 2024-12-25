# Random forest models (10/2025):

rm(list=ls())

library(randomForest)
library(ROCR) 
library(VSURF)
library(corrplot)
library(dplyr)
library(sf)

##### 5 Years All: #####
load(file = '/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/Final Scripts/WPBR_plots_DAYMET.RDATA')

load("/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/Final Scripts/WPBR_Variable_Selction_5_DAYMET.RData")


data.5 <- data.5years %>% dplyr::filter( RUST <= 1)

# All data models 
all.5 <- data.5 %>% dplyr::select(c(RUST,vars.m.all.5)) %>% na.omit()

sample <- sample(c(TRUE, FALSE), nrow(all.5), replace=TRUE, prob=c(0.7,0.3))
train.5 <- all.5[sample, ]
test.5 <- all.5[!sample, ]

rf.all.5 <- randomForest(factor(RUST) ~ .,
                          data=train.5 , do.trace =TRUE,   importance=TRUE)

rf.all.5

# calculate AUC : # 1. Area under curve:  It tells how much model is capable of distinguishing between classes. Higher the AUC, better the model is at predicting 0s as 0s and 1s as 1s.
auc.all.5  = performance( prediction(predict(rf.all.5, type = "prob")[,2], train.5$RUST), "auc")
auc.all.5@y.values

test.5$MODEL <- predict(rf.all.5, test.5, type = "prob") [,2]

test.5$MODEL.c[test.5$MODEL >= 0.50] <- 1
test.5$MODEL.c[test.5$MODEL < 0.50] <- 0

(length(test.5$MODEL.c[ test.5$MODEL.c == 1 & test.5$RUST ==1]) + length(test.5$MODEL.c[ test.5$MODEL.c == 0 & test.5$RUST ==0]) )/ length(test.5$RUST) *100

save(vars.m.all.5, all.5, train.5 , test.5, rf.all.5 , auc.all.5, data.5,
     file="/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/Final Scripts/RF_MODELFIT_Results_DAYMET.RDATA")

##### 5 Years Invading: #####
invading.5 <- data.5 %>% filter(WPBR_stat == "Invading")

# All data models 
vars.m.invading.5 

years.5.invading <- invading.5 %>% dplyr::select(c(RUST,vars.m.invading.5 )) %>% na.omit()

sample <- sample(c(TRUE, FALSE), nrow(years.5.invading ), replace=TRUE, prob=c(0.7,0.3))
train.5.invading   <- years.5.invading[sample, ]
test.5.invading    <- years.5.invading[!sample, ]

rf.invading.5 <- randomForest(factor(RUST) ~ . ,
                         data=train.5.invading  , do.trace =TRUE,   importance=TRUE)

rf.invading.5

auc.invading.5  = performance( prediction(predict(rf.invading.5, type = "prob")[,2], train.5.invading$RUST), "auc")
auc.invading.5@y.values

test.5.invading$MODEL <- predict(rf.invading.5, test.5.invading, type = "prob") [,2]

test.5.invading$MODEL.c[test.5.invading$MODEL >= 0.50] <- 1
test.5.invading$MODEL.c[test.5.invading$MODEL < 0.50] <- 0

(length(test.5.invading$MODEL.c[ test.5.invading $MODEL.c == 1 & test.5.invading $RUST ==1]) + length(test.5.invading$MODEL.c[ test.5.invading$MODEL.c == 0 & test.5.invading$RUST ==0]) )/ length(test.5.invading $RUST) *100


save(vars.m.all.5, all.5, train.5 , test.5, rf.all.5 , auc.all.5, data.5,
     vars.m.invading.5, invading.5, train.5.invading , test.5.invading, rf.invading.5 , auc.invading.5,
     file="/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/Final Scripts/RF_MODELFIT_Results_DAYMET.RDATA")

##### 5 Years Established: #####

established.5 <- data.5 %>% filter( WPBR_stat == "Established")

vars.m.established.5 

years.5.established <- established.5 %>% dplyr::select(c(RUST, vars.m.established.5)) %>%na.omit()

sample <- sample(c(TRUE, FALSE), nrow(years.5.established ), replace=TRUE, prob=c(0.7,0.3))
train.5.established   <- years.5.established[sample, ]
test.5.established   <- years.5.established[!sample, ]

rf.established.5 <- randomForest(factor(RUST) ~ .,
                              data=train.5.established  , do.trace =TRUE,   importance=TRUE)

rf.established.5 

auc.established.5  = performance( prediction(predict(rf.established.5, type = "prob")[,2], train.5.established$RUST), "auc")
auc.established.5@y.values

test.5.established$MODEL <- predict(rf.established.5, test.5.established, type = "prob") [,2]

test.5.established$MODEL.c[test.5.established$MODEL >= 0.50] <- 1
test.5.established$MODEL.c[test.5.established$MODEL < 0.50] <- 0

(length(test.5.established$MODEL.c[ test.5.established $MODEL.c == 1 & test.5.established $RUST ==1]) + length(test.5.established$MODEL.c[ test.5.established$MODEL.c == 0 & test.5.established$RUST ==0]) )/ length(test.5.established $RUST) *100

save(vars.m.all.5, all.5, train.5 , test.5, rf.all.5 , auc.all.5, data.5,
     vars.m.invading.5, invading.5, train.5.invading , test.5.invading, rf.invading.5 , auc.invading.5,
     vars.m.established.5, established.5, train.5.established , test.5.established, rf.established.5 , auc.established.5, 
     file="/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/Final Scripts/RF_MODELFIT_Results_DAYMET.RDATA")

##### 10 Years All: #####
load(file = "/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/Final Scripts/WPBR_Variable_Selction_10_DAYMET.RData")

data.10n <- data.10years %>% filter(RUST < 2) 
# All data models 
vars.m.all.10 

all.10 <- data.10years %>% dplyr::select(c(RUST, vars.m.all.10) ) %>% filter( RUST <=1)

sample <- sample(c(TRUE, FALSE), nrow(all.10), replace=TRUE, prob=c(0.7,0.3))
train.10   <- all.10[sample, ]
test.10    <- all.10[!sample, ]

rf.all.10 <-  randomForest(factor(RUST) ~ . , data=train.10, do.trace =TRUE,   importance=TRUE)

rf.all.10

auc.all.10  = performance( prediction(predict(rf.all.10, type = "prob")[,2], train.10$RUST), "auc")
auc.all.10@y.values

test.10$MODEL <- predict(rf.all.10, test.10, type = "prob") [,2]

test.10$MODEL.c[test.10$MODEL >= 0.50] <- 1
test.10$MODEL.c[test.10$MODEL < 0.50] <- 0

(length(test.10$MODEL.c[ test.10 $MODEL.c == 1 & test.10 $RUST ==1]) + length(test.10$MODEL.c[ test.10$MODEL.c == 0 & test.10$RUST ==0]) )/ length(test.10 $RUST) *100

save(vars.m.all.5, all.5, train.5 , test.5, rf.all.5 , auc.all.5, data.5years,
     vars.m.invading.5, invading.5, train.5.invading , test.5.invading, rf.invading.5 , auc.invading.5,
     vars.m.established.5, established.5, train.5.established , test.5.established, rf.established.5 , auc.established.5, 
     vars.m.all.10, all.10, train.10 , test.10, rf.all.10 , auc.all.10, data.10years,
     file="/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/Final Scripts/RF_MODELFIT_Results_DAYMET.RDATA")

##### 10 Years Invading: #####
invading.10 <- data.10n %>% filter(WPBR_stat == "Invading")

# All data models 
vars.m.invading.10 
years.10.invading <- invading.10 %>% dplyr::select(c(RUST,vars.m.invading.10)) %>% na.omit()

sample <- sample(c(TRUE, FALSE), nrow(years.10.invading ), replace=TRUE, prob=c(0.7,0.3))
train.10.invading   <- years.10.invading[sample, ]
test.10.invading    <- years.10.invading[!sample, ]

rf.invading.10 <- randomForest(factor(RUST) ~ .,
                               data=train.10.invading  , do.trace =TRUE,   importance=TRUE)

rf.invading.10 

imp.10.invading <- varImpPlot(rf.invading.10) %>% as.data.frame() 
imp.10.invading$varnames <- rownames(imp.10.invading) # row names to column
rownames(imp.10.invading) <- NULL  


auc.invading.10  = performance( prediction(predict(rf.invading.10, type = "prob")[,2], train.10.invading$RUST), "auc")
auc.invading.10@y.values

test.10.invading$MODEL <- predict(rf.invading.10, test.10.invading, type = "prob") [,2]

test.10.invading$MODEL.c[test.10.invading$MODEL >= 0.50] <- 1
test.10.invading$MODEL.c[test.10.invading$MODEL < 0.50] <- 0

(length(test.10.invading$MODEL.c[ test.10.invading$MODEL.c == 1 & test.10.invading$RUST ==1]) + length(test.10.invading$MODEL.c[ test.10.invading$MODEL.c == 0 & test.10.invading$RUST ==0]) )/ length(test.10.invading$RUST) *100

save(vars.m.all.5, all.5, train.5 , test.5, rf.all.5 , auc.all.5, data.5,
     vars.m.invading.5, invading.5, train.5.invading , test.5.invading, rf.invading.5 , auc.invading.5,
     vars.m.established.5, established.5, train.5.established , test.5.established, rf.established.5 , auc.established.5,
     vars.m.all.10, all.10, train.10 , test.10, rf.all.10 , auc.all.10, data.10years,
     vars.m.invading.10, invading.10, train.10.invading , test.10.invading, rf.invading.10 , auc.invading.10,
     file="/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/Final Scripts/RF_MODELFIT_Results_DAYMET.RDATA")

##### 10 Years Established: #####
established.10 <- data.10n %>% filter(WPBR_stat == "Established")

# All data models 
vars.m.established.10 

years.10.established <- established.10 %>% dplyr::select(RUST, vars.m.established.10) %>%na.omit()

sample <- sample(c(TRUE, FALSE), nrow(years.10.established ), replace=TRUE, prob=c(0.7,0.3))
train.10.established   <- years.10.established[sample, ]
test.10.established   <- years.10.established[!sample, ]

rf.established.10 <- randomForest(factor(RUST) ~ .,data=train.10.established  , do.trace =TRUE,   importance=TRUE)

rf.established.10 

auc.established.10  = performance( prediction(predict(rf.established.10, type = "prob")[,2], train.10.established$RUST), "auc")
auc.established.10@y.values

test.10.established$MODEL <- predict(rf.established.10, test.10.established, type = "prob") [,2]

test.10.established$MODEL.c[test.10.established$MODEL >= 0.50] <- 1
test.10.established$MODEL.c[test.10.established$MODEL < 0.50] <- 0

(length(test.10.established$MODEL.c[ test.10.established$MODEL.c == 1 & test.10.established$RUST ==1]) + length(test.10.established$MODEL.c[ test.10.established$MODEL.c == 0 & test.10.established$RUST ==0]) )/ length(test.10.established$RUST) *100

save(vars.m.all.5, all.5, train.5 , test.5, rf.all.5 , auc.all.5, data.5years,
     vars.m.invading.5, invading.5, train.5.invading , test.5.invading, rf.invading.5 , auc.invading.5,
     vars.m.established.5, established.5, train.5.established , test.5.established, rf.established.5 , auc.established.5, 
     vars.m.all.10, all.10, train.10 , test.10, rf.all.10 , auc.all.10, data.10years,
     vars.m.invading.10, invading.10, train.10.invading , test.10.invading, rf.invading.10 , auc.invading.10,
     vars.m.established.10, established.10, train.10.established , test.10.established, rf.established.10 , auc.established.10, 
     file="/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/Final Scripts/RF_MODELFIT_Results_DAYMET.RDATA")

##### 20 Years All: #####
load(file ="/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/Final Scripts/WPBR_Variable_Selction_20_DAYMET.RData")

data.20 <- data.20years %>% filter( RUST < 2) 

# All data models 
vars.m.all.20 

all.20 <- data.20  %>% dplyr::select(RUST, vars.m.all.20) %>%na.omit()

sample <- sample(c(TRUE, FALSE), nrow(all.20), replace=TRUE, prob=c(0.7,0.3))
train.20   <- all.20[sample, ]
test.20    <- all.20[!sample, ]

rf.all.20 <- randomForest(factor(RUST) ~  ., data=train.20 , do.trace =TRUE,   importance=TRUE)

rf.all.20

auc.all.20  = performance( prediction(predict(rf.all.20, type = "prob")[,2], train.20$RUST), "auc")
auc.all.20@y.values

test.20$MODEL <- predict( rf.all.20, test.20, type = "prob")[,2]

test.20$MODEL.c[test.20$MODEL >= 0.50] <- 1
test.20$MODEL.c[test.20$MODEL < 0.50] <- 0

(length(test.20$MODEL.c[ test.20$MODEL.c == 1 & test.20$RUST ==1]) + length(test.20$MODEL.c[ test.20$MODEL.c == 0 & test.20$RUST ==0]) )/ length(test.20$RUST) *100

save(vars.m.all.5, all.5, train.5 , test.5, rf.all.5 , auc.all.5, data.5years,
     vars.m.invading.5, invading.5, train.5.invading , test.5.invading, rf.invading.5 , auc.invading.5,
     vars.m.established.5, established.5, train.5.established , test.5.established, rf.established.5 , auc.established.5, 
     vars.m.all.10, all.10, train.10 , test.10, rf.all.10 , auc.all.10, data.10years,
     vars.m.invading.10, invading.10, train.10.invading , test.10.invading, rf.invading.10 , auc.invading.10,
     vars.m.established.10, established.10, train.10.established , test.10.established, rf.established.10 , auc.established.10, 
     
     vars.m.all.20, all.20, train.20 , test.20, rf.all.20 , auc.all.20, data.20years,
     file="/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/Final Scripts/RF_MODELFIT_Results_DAYMET.RDATA")

##### 20 Years Invading: #####
invading.20 <- data.20years %>% dplyr::filter(WPBR_stat == "Invading",
                                              RUST <=1)

# Invading data models 
vars.m.invading.20 

years.20.invading <- invading.20 %>% dplyr::select(RUST, vars.m.invading.20) %>% na.omit()

sample <- sample(c(TRUE, FALSE), nrow(years.20.invading ), replace=TRUE, prob=c(0.7,0.3))
train.20.invading   <- years.20.invading[sample, ]
test.20.invading    <- years.20.invading[!sample, ]

rf.invading.20 <- randomForest(factor(RUST) ~ .,data=train.20.invading  , do.trace =TRUE,   importance=TRUE)

rf.invading.20

auc.invading.20  = performance( prediction(predict(rf.invading.20, type = "prob")[,2], train.20.invading$RUST), "auc")
auc.invading.20@y.values

test.20.invading$MODEL <- predict( rf.invading.20, test.20.invading, type = "prob")[,2]

test.20.invading$MODEL.c[test.20.invading$MODEL >= 0.50] <- 1
test.20.invading$MODEL.c[test.20.invading$MODEL < 0.50] <- 0

(length(test.20.invading$MODEL.c[ test.20.invading$MODEL.c == 1 & test.20.invading$RUST ==1]) + length(test.20.invading$MODEL.c[ test.20.invading$MODEL.c == 0 & test.20.invading$RUST ==0]) )/ length(test.20.invading$RUST) *100

save(vars.m.all.5, all.5, train.5 , test.5, rf.all.5 , auc.all.5, data.5years,
     vars.m.invading.5, invading.5, train.5.invading , test.5.invading, rf.invading.5 , auc.invading.5,
     vars.m.established.5, established.5, train.5.established , test.5.established, rf.established.5 , auc.established.5, 
     vars.m.all.10, all.10, train.10 , test.10, rf.all.10 , auc.all.10, data.10years,
     vars.m.invading.10, invading.10, train.10.invading , test.10.invading, rf.invading.10 , auc.invading.10,
     vars.m.established.10, established.10, train.10.established , test.10.established, rf.established.10 , auc.established.10, 
     
     vars.m.all.20, all.20, train.20 , test.20, rf.all.20 , auc.all.20, data.20years,
     vars.m.invading.20, invading.20, train.20.invading , test.20.invading, rf.invading.20 , auc.invading.20,
     file="/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/Final Scripts/RF_MODELFIT_Results_DAYMET.RDATA")

##### 20 Years Established: #####

established.20 <- data.20 %>% filter(WPBR_stat == "Established")

# All data models 
vars.m.established.20 
years.20.established <- established.20 %>% dplyr::select(RUST,vars.m.established.20) %>% na.omit()

sample <- sample(c(TRUE, FALSE), nrow(years.20.established ), replace=TRUE, prob=c(0.7,0.3))
train.20.established   <- years.20.established[sample, ]
test.20.established   <- years.20.established[!sample, ]

rf.established.20 <- randomForest(factor(RUST) ~  .,data=train.20.established  , do.trace =TRUE,   importance=TRUE)

rf.established.20 

auc.established.20  = performance( prediction(predict(rf.established.20, type = "prob")[,2], train.20.established$RUST), "auc")
auc.established.20@y.values

test.20.established$MODEL <- predict( rf.established.20, test.20.established, type = "prob")[,2]

test.20.established$MODEL.c[test.20.established$MODEL >= 0.50] <- 1
test.20.established$MODEL.c[test.20.established$MODEL < 0.50] <- 0

(length(test.20.established$MODEL.c[ test.20.established$MODEL.c == 1 & test.20.established$RUST ==1]) + length(test.20.established$MODEL.c[ test.20.established$MODEL.c == 0 & test.20.established$RUST ==0]) )/ length(test.20.established$RUST) *100

save(vars.m.all.5, all.5, train.5 , test.5, rf.all.5 , auc.all.5, data.5years,
     vars.m.invading.5, invading.5, train.5.invading , test.5.invading, rf.invading.5 , auc.invading.5,
     vars.m.established.5, established.5, train.5.established , test.5.established, rf.established.5 , auc.established.5, 
     vars.m.all.10, all.10, train.10 , test.10, rf.all.10 , auc.all.10, data.10years,
     vars.m.invading.10, invading.10, train.10.invading , test.10.invading, rf.invading.10 , auc.invading.10,
     vars.m.established.10, established.10, train.10.established , test.10.established, rf.established.10 , auc.established.10, 
     
     vars.m.all.20, all.20, train.20 , test.20, rf.all.20 , auc.all.20, data.20years,
     vars.m.invading.20, invading.20, train.20.invading , test.20.invading, rf.invading.20 , auc.invading.20,
     vars.m.established.20, established.20, train.20.established , test.20.established, rf.established.20 , auc.established.20, 
     file="/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/Final Scripts/RF_MODELFIT_Results_DAYMET.RDATA")

# Load Models:
rm(list=ls())

load("/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/Final Scripts/RF_MODELFIT_Results_DAYMET.RDATA")

#EOF