# Random forest models (04/2023):
library(randomForest) # models
library(ROCR) # calculate AUC : # 1. Area under curve:  It tells how much model is capable of distinguishing between classes. Higher the AUC, better the model is at predicting 0s as 0s and 1s as 1s.
library(VSURF)
library(corrplot)
library(tidyverse)

rm(list=ls())

##### 5 Years All: #####
load(file = "/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/WPBR_Variable_Selction_5.RData")
load(file='/Users/sm3466/Dropbox (YSE)/Research/WPBR/WPBR_plots_climate.RDATA')

data.5 <- data.5years %>% filter( RUST <=1)

names(data.5)
# All data models 
vars.m.all.5n <- c("RUST", "H_Zone" ,  
                   "Tmin_Spring" , "Tmin_Summer", "Tmin_Fall",
                   "Tmax_Spring" , "Tmax_Summer", "Tmax_Fall",
                   "Tmean_Spring" , "Tmean_Summer", "Tmean_Fall",
                   "PRCP_Spring" , "PRCP_Summer", "PRCP_Fall", 
                   "VPD_Fall" , "VPD_Summer","VPD_Fall",
                   "VPDmax_Spring","VPDmax_Summer","VPDmax_Fall",
                   "RH_Spring", "RH_Summer", "RH_Fall",
                   "TPI","TRI", "StreamDen")


all.5 <- data.5[, vars.m.all.5n ] %>% na.omit()
sample <- sample(c(TRUE, FALSE), nrow(all.5), replace=TRUE, prob=c(0.7,0.3))
train.5   <- all.5[sample, ]
test.5    <- all.5[!sample, ]


rf.all.5 <- randomForest(factor(RUST) ~ .,
                          data=train.5 , do.trace =TRUE,   importance=TRUE)

rf.all.5
auc.all.5  = performance( prediction(predict(rf.all.5, type = "prob")[,2], train.5$RUST), "auc")
auc.all.5@y.values

test.5$MODEL <- predict(rf.all.5, test.5, type = "prob") [,2]

test.5$MODEL.c[test.5$MODEL >= 0.50] <- 1
test.5$MODEL.c[test.5$MODEL < 0.50] <- 0

(length(test.5$MODEL.c[ test.5$MODEL.c == 1 & test.5$RUST ==1]) + length(test.5$MODEL.c[ test.5$MODEL.c == 0 & test.5$RUST ==0]) )/ length(test.5$RUST) *100

save(vars.m.all.5n, all.5, train.5 , test.5, rf.all.5 , auc.all.5, data.5,
     file="/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/RF_MODELFIT_Results.RDATA")

##### 5 Years Invading: #####
invading.5 <- data.5n[which(data.5n$WPBR_stat == "Invading"),]

# All data models 
vars.m.invading.5 
vars.m.invading.5n <- c("RUST", "H_Zone" ,  "VPD_Fall", "VPDmax_Fall", "VPDmax_Summer", "Tmin_Fall","RH_Fall","PRCP_Spring" , "PRCP_Summer", "PRCP_Fall", "StreamDen")

years.5.invading <- invading.5[, vars.m.invading.5n] %>% na.omit()

sample <- sample(c(TRUE, FALSE), nrow(years.5.invading ), replace=TRUE, prob=c(0.7,0.3))
train.5.invading   <- years.5.invading[sample, ]
test.5.invading    <- years.5.invading[!sample, ]

rf.invading.5 <- randomForest(factor(RUST) ~    H_Zone +     
                                VPD_Fall+VPDmax_Fall+VPDmax_Summer+ Tmin_Fall+
                                RH_Fall + PRCP_Spring+PRCP_Summer+ PRCP_Fall+ StreamDen ,
                         data=train.5.invading  , do.trace =TRUE,   importance=TRUE)

rf.invading.5

auc.invading.5  = performance( prediction(predict(rf.invading.5, type = "prob")[,2], train.5.invading$RUST), "auc")
auc.invading.5@y.values

test.5.invading$MODEL <- predict(rf.invading.5, test.5.invading, type = "prob") [,2]

test.5.invading$MODEL.c[test.5.invading$MODEL >= 0.50] <- 1
test.5.invading$MODEL.c[test.5.invading$MODEL < 0.50] <- 0

(length(test.5.invading$MODEL.c[ test.5.invading $MODEL.c == 1 & test.5.invading $RUST ==1]) + length(test.5.invading$MODEL.c[ test.5.invading$MODEL.c == 0 & test.5.invading$RUST ==0]) )/ length(test.5.invading $RUST) *100


save(vars.m.all.5n, all.5, train.5 , test.5, rf.all.5 , auc.all.5, data.5n,
     vars.m.invading.5n, invading.5, train.5.invading , test.5.invading, rf.invading.5 , auc.invading.5,
     file="/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/RF_MODELFIT_Results.RDATA")

##### 5 Years Established: #####

established.5 <- data.5n[which(data.5n$WPBR_stat == "Established"),]

vars.m.established.5 
vars.m.established.5n <- c("RUST", "H_Zone" ,"StreamDen", "PRCP_Spring", "PRCP_Summer","PRCP_Fall", "VPD_Fall", "VPD_Summer", 'RH_Spring', 'RH_Summer', 'RH_Fall', 'Tmin_Summer', 'Tmin_Fall', 'Tmax_Fall', "StreamDen")

years.5.established <- established.5[, vars.m.established.5n] %>%na.omit()

sample <- sample(c(TRUE, FALSE), nrow(years.5.established ), replace=TRUE, prob=c(0.7,0.3))
train.5.established   <- years.5.established[sample, ]
test.5.established   <- years.5.established[!sample, ]

rf.established.5 <- randomForest(factor(RUST) ~    H_Zone + StreamDen +  PRCP_Spring +  PRCP_Summer + PRCP_Fall +  VPD_Fall +  VPD_Summer + RH_Spring+ RH_Summer+ RH_Fall+ Tmin_Summer+ Tmin_Fall+ Tmax_Fall + StreamDen,
                              data=train.5.established  , do.trace =TRUE,   importance=TRUE)

rf.established.5 

auc.established.5  = performance( prediction(predict(rf.established.5, type = "prob")[,2], train.5.established$RUST), "auc")
auc.established.5@y.values

test.5.established$MODEL <- predict(rf.established.5, test.5.established, type = "prob") [,2]

test.5.established$MODEL.c[test.5.established$MODEL >= 0.50] <- 1
test.5.established$MODEL.c[test.5.established$MODEL < 0.50] <- 0

(length(test.5.established$MODEL.c[ test.5.established $MODEL.c == 1 & test.5.established $RUST ==1]) + length(test.5.established$MODEL.c[ test.5.established$MODEL.c == 0 & test.5.established$RUST ==0]) )/ length(test.5.established $RUST) *100

save(vars.m.all.5n, all.5, train.5 , test.5, rf.all.5 , auc.all.5, data.5n,
     vars.m.invading.5n, invading.5, train.5.invading , test.5.invading, rf.invading.5 , auc.invading.5,
     vars.m.established.5n, established.5, train.5.established , test.5.established, rf.established.5 , auc.established.5, 
     file="/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/RF_MODELFIT_Results.RDATA")

##### 10 Years All: #####
load(file = "/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/WPBR_Variable_Selction_10.RData")

data.10 <- read.csv ('/Users/sm3466/Dropbox (YSE)/Research/WPBR/WPBR_plots_10years.csv') 

data.10n <- data.10 %>% filter(is.na(GB) == TRUE, RUST < 2) %>% mutate(RUST <- as.factor(RUST),
                                                                       RIBES <- as.factor(RIBES))

data.10n$GB <- NULL

# consolidate variables:
data.10n$Tmin_Spring <- (data.10n$tmin.4 + data.10n$tmin.5) / 2
data.10n$Tmin_Summer <- (data.10n$tmin.6 + data.10n$tmin.7 + data.10n$tmin.8) / 3
data.10n$Tmin_Fall <- (data.10n$tmin.9 + data.10n$tmin.10) / 2

data.10n$Tmax_Spring <- (data.10n$tmax.4 + data.10n$tmax.5) / 2
data.10n$Tmax_Summer <- (data.10n$tmax.6 + data.10n$tmax.7 + data.10n$tmax.8) / 3
data.10n$Tmax_Fall <- (data.10n$tmax.9 + data.10n$tmax.10) / 2

data.10n$Tmean_Spring <- (data.10n$tmean.4 + data.10n$tmean.5) / 2
data.10n$Tmean_Summer <- (data.10n$tmean.6 + data.10n$tmean.7 + data.10n$tmean.8) / 3
data.10n$Tmean_Fall <- (data.10n$tmean.9 + data.10n$tmean.10) / 2

data.10n$PRCP_Spring <- (data.10n$prcp.4 + data.10n$prcp.5) / 2
data.10n$PRCP_Summer <- (data.10n$prcp.6 + data.10n$prcp.7 + data.10n$prcp.8) / 3
data.10n$PRCP_Fall <- (data.10n$prcp.9 + data.10n$prcp.10) / 2

data.10n$VPD_Spring <- (data.10n$vpd.4 + data.10n$vpd.5) / 2
data.10n$VPD_Summer <- (data.10n$vpd.6 + data.10n$vpd.7 + data.10n$vpd.8) / 3
data.10n$VPD_Fall <- (data.10n$vpd.9 + data.10n$vpd.10) / 2

data.10n$VPDmax_Spring <- (data.10n$vpdmax.4 + data.10n$vpdmax.5) / 2
data.10n$VPDmax_Summer <- (data.10n$vpdmax.6 + data.10n$vpdmax.7 + data.10n$vpdmax.8) / 3
data.10n$VPDmax_Fall <- (data.10n$vpdmax.9 + data.10n$vpdmax.10) / 2

data.10n$RH_Spring <- (data.10n$RH.4 + data.10n$RH.5) / 2
data.10n$RH_Summer <- (data.10n$RH.6 + data.10n$RH.7 + data.10n$RH.8) / 3
data.10n$RH_Fall <- (data.10n$RH.9 + data.10n$RH.10) / 2


data.10n$Tmean_Summer <- (data.10n$tmean.6 + data.10n$tmean.7 + data.10n$tmean.8) / 3
data.10n$H_Zone <-data.10n$Zone_easy

# All data models 
vars.m.all.10 

vars.m.all.10n <- c("RUST", "H_Zone" ,  "Tmin_Fall", "Tmin_Summer", "VPD_Fall","PRCP_Summer", "PRCP_Fall","VPDmax_Spring", "Tmin_Spring", "VPDmax_Summer", "PRCP_Spring","VPD_Spring", "StreamDen" )

all.10 <- data.10n[, vars.m.all.10n] %>%na.omit()

sample <- sample(c(TRUE, FALSE), nrow(all.10), replace=TRUE, prob=c(0.7,0.3))
train.10   <- all.10[sample, ]
test.10    <- all.10[!sample, ]

rf.all.10 <- randomForest(factor(RUST) ~    H_Zone + Tmin_Fall+ Tmin_Summer+ VPD_Fall+PRCP_Summer+ PRCP_Fall+VPDmax_Spring+ Tmin_Spring+ VPDmax_Summer+ PRCP_Spring+VPD_Spring+ StreamDen , data=train.10 , do.trace =TRUE,   importance=TRUE)

rf.all.10

auc.all.10  = performance( prediction(predict(rf.all.10, type = "prob")[,2], train.10$RUST), "auc")
auc.all.10@y.values

test.10$MODEL <- predict(rf.all.10, test.10, type = "prob") [,2]

test.10$MODEL.c[test.10$MODEL >= 0.50] <- 1
test.10$MODEL.c[test.10$MODEL < 0.50] <- 0

(length(test.10$MODEL.c[ test.10 $MODEL.c == 1 & test.10 $RUST ==1]) + length(test.10$MODEL.c[ test.10$MODEL.c == 0 & test.10$RUST ==0]) )/ length(test.10 $RUST) *100

save(vars.m.all.5n, all.5, train.5 , test.5, rf.all.5 , auc.all.5, data.5n,
     vars.m.invading.5n, invading.5, train.5.invading , test.5.invading, rf.invading.5 , auc.invading.5,
     vars.m.established.5n, established.5, train.5.established , test.5.established, rf.established.5 , auc.established.5, 
     
     vars.m.all.10n, all.10, train.10 , test.10, rf.all.10 , auc.all.10, data.10n,
     file="/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/RF_MODELFIT_Results.RDATA")

##### 10 Years Invading: #####
invading.10 <- data.10n[which(data.10n$WPBR_stat == "Invading"),]

# All data models 
vars.m.invading.10 
vars.m.invading.10n <- c("RUST", "H_Zone" ,  "PRCP_Spring", "VPD_Fall" , "PRCP_Fall" , "PRCP_Summer", "VPDmax_Fall", "VPD_Summer", "StreamDen")

years.10.invading <- invading.10[, vars.m.invading.10n] %>% na.omit()

sample <- sample(c(TRUE, FALSE), nrow(years.10.invading ), replace=TRUE, prob=c(0.7,0.3))
train.10.invading   <- years.10.invading[sample, ]
test.10.invading    <- years.10.invading[!sample, ]

rf.invading.10 <- randomForest(factor(RUST) ~    H_Zone +     
                                 PRCP_Spring+  VPD_Fall +  PRCP_Fall +  PRCP_Summer+  VPDmax_Fall+  VPD_Summer + StreamDen,
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

save(vars.m.all.5n, all.5, train.5 , test.5, rf.all.5 , auc.all.5, data.5n,
     vars.m.invading.5n, invading.5, train.5.invading , test.5.invading, rf.invading.5 , auc.invading.5,
     vars.m.established.5n, established.5, train.5.established , test.5.established, rf.established.5 , auc.established.5, 
     
     vars.m.all.10n, all.10, train.10 , test.10, rf.all.10 , auc.all.10, data.10n,
     vars.m.invading.10n, invading.10, train.10.invading , test.10.invading, rf.invading.10 , auc.invading.10,
     file="/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/RF_MODELFIT_Results.RDATA")

##### 10 Years Established: #####

established.10 <- data.10n[which(data.10n$WPBR_stat == "Established"),]

# All data models 
vars.m.established.10 
vars.m.established.10n <- c("RUST", "H_Zone" ,"StreamDen", "PRCP_Spring", "PRCP_Summer","PRCP_Fall", "VPD_Fall", "VPD_Spring", 'RH_Spring', 'RH_Summer', 'RH_Fall', 'Tmin_Summer', 'Tmax_Summer')


years.10.established <- established.10[, vars.m.established.10n] %>%na.omit()

sample <- sample(c(TRUE, FALSE), nrow(years.10.established ), replace=TRUE, prob=c(0.7,0.3))
train.10.established   <- years.10.established[sample, ]
test.10.established   <- years.10.established[!sample, ]

rf.established.10 <- randomForest(factor(RUST) ~    H_Zone + StreamDen +  PRCP_Spring+  PRCP_Summer+ PRCP_Fall+  VPD_Fall+  VPD_Spring+  RH_Spring+  RH_Summer+  RH_Fall+  Tmin_Summer+  Tmax_Summer,
                                  data=train.10.established  , do.trace =TRUE,   importance=TRUE)

rf.established.10 

auc.established.10  = performance( prediction(predict(rf.established.10, type = "prob")[,2], train.10.established$RUST), "auc")
auc.established.10@y.values

test.10.established$MODEL <- predict(rf.established.10, test.10.established, type = "prob") [,2]

test.10.established$MODEL.c[test.10.established$MODEL >= 0.50] <- 1
test.10.established$MODEL.c[test.10.established$MODEL < 0.50] <- 0

(length(test.10.established$MODEL.c[ test.10.established$MODEL.c == 1 & test.10.established$RUST ==1]) + length(test.10.established$MODEL.c[ test.10.established$MODEL.c == 0 & test.10.established$RUST ==0]) )/ length(test.10.established$RUST) *100

save(vars.m.all.5n, all.5, train.5 , test.5, rf.all.5 , auc.all.5, data.5n,
     vars.m.invading.5n, invading.5, train.5.invading , test.5.invading, rf.invading.5 , auc.invading.5,
     vars.m.established.5n, established.5, train.5.established , test.5.established, rf.established.5 , auc.established.5, 
     
     vars.m.all.10n, all.10, train.10 , test.10, rf.all.10 , auc.all.10, data.10n,
     vars.m.invading.10n, invading.10, train.10.invading , test.10.invading, rf.invading.10 , auc.invading.10,
     vars.m.established.10n, established.10, train.10.established , test.10.established, rf.established.10 , auc.established.10, 
     file="/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/RF_MODELFIT_Results.RDATA")

##### 20 Years All: #####
load(file = "/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/WPBR_Variable_Selction_20.RData")

data.20 <- read.csv ('/Users/sm3466/Dropbox (YSE)/Research/WPBR/WPBR_plots_20years.csv') 

data.20n <- data.20 %>% filter(is.na(GB) == TRUE, RUST < 2) %>% mutate(RUST <- as.factor(RUST),
                                                                       RIBES <- as.factor(RIBES))

data.20n$GB <- NULL

# consolidate variables:

data.20n$Tmin_Spring <- (data.20n$tmin.4 + data.20n$tmin.5) / 2
data.20n$Tmin_Summer <- (data.20n$tmin.6 + data.20n$tmin.7 + data.20n$tmin.8) / 3
data.20n$Tmin_Fall <- (data.20n$tmin.9 + data.20n$tmin.10) / 2

data.20n$Tmax_Spring <- (data.20n$tmax.4 + data.20n$tmax.5) / 2
data.20n$Tmax_Summer <- (data.20n$tmax.6 + data.20n$tmax.7 + data.20n$tmax.8) / 3
data.20n$Tmax_Fall <- (data.20n$tmax.9 + data.20n$tmax.10) / 2

data.20n$Tmean_Spring <- (data.20n$tmean.4 + data.20n$tmean.5) / 2
data.20n$Tmean_Summer <- (data.20n$tmean.6 + data.20n$tmean.7 + data.20n$tmean.8) / 3
data.20n$Tmean_Fall <- (data.20n$tmean.9 + data.20n$tmean.10) / 2

data.20n$PRCP_Spring <- (data.20n$prcp.4 + data.20n$prcp.5) / 2
data.20n$PRCP_Summer <- (data.20n$prcp.6 + data.20n$prcp.7 + data.20n$prcp.8) / 3
data.20n$PRCP_Fall <- (data.20n$prcp.9 + data.20n$prcp.10) / 2

data.20n$VPD_Spring <- (data.20n$vpd.4 + data.20n$vpd.5) / 2
data.20n$VPD_Summer <- (data.20n$vpd.6 + data.20n$vpd.7 + data.20n$vpd.8) / 3
data.20n$VPD_Fall <- (data.20n$vpd.9 + data.20n$vpd.10) / 2

data.20n$VPDmax_Spring <- (data.20n$vpdmax.4 + data.20n$vpdmax.5) / 2
data.20n$VPDmax_Summer <- (data.20n$vpdmax.6 + data.20n$vpdmax.7 + data.20n$vpdmax.8) / 3
data.20n$VPDmax_Fall <- (data.20n$vpdmax.9 + data.20n$vpdmax.10) / 2

data.20n$RH_Spring <- (data.20n$RH.4 + data.20n$RH.5) / 2
data.20n$RH_Summer <- (data.20n$RH.6 + data.20n$RH.7 + data.20n$RH.8) / 3
data.20n$RH_Fall <- (data.20n$RH.9 + data.20n$RH.10) / 2


data.20n$Tmean_Summer <- (data.20n$tmean.6 + data.20n$tmean.7 + data.20n$tmean.8) / 3


data.20n$H_Zone <- data.20n$Zone_easy

# All data models 
vars.m.all.20 

vars.m.all.20n <- c("RUST", "H_Zone" , "PRCP_Summer", "PRCP_Fall" ,"PRCP_Spring",
                    "Tmin_Fall", "RH_Fall", "RH_Summer", "VPD_Fall", "VPD_Summer", "VPDmax_Spring", "VPDmax_Fall", "StreamDen")

all.20 <- data.20n[, vars.m.all.20n] %>%na.omit()

sample <- sample(c(TRUE, FALSE), nrow(all.20), replace=TRUE, prob=c(0.7,0.3))
train.20   <- all.20[sample, ]
test.20    <- all.20[!sample, ]


rf.all.20 <- randomForest(factor(RUST) ~    H_Zone + Tmin_Fall+ PRCP_Summer+ PRCP_Fall +PRCP_Spring+ RH_Fall+ RH_Summer+ VPD_Fall+ VPD_Summer+ VPDmax_Spring+ VPDmax_Fall+ StreamDen, data=train.20 , do.trace =TRUE,   importance=TRUE)

rf.all.20

auc.all.20  = performance( prediction(predict(rf.all.20, type = "prob")[,2], train.20$RUST), "auc")
auc.all.20@y.values

test.20$MODEL <- predict( rf.all.20, test.20, type = "prob")[,2]

test.20$MODEL.c[test.20$MODEL >= 0.50] <- 1
test.20$MODEL.c[test.20$MODEL < 0.50] <- 0

(length(test.20$MODEL.c[ test.20$MODEL.c == 1 & test.20$RUST ==1]) + length(test.20$MODEL.c[ test.20$MODEL.c == 0 & test.20$RUST ==0]) )/ length(test.20$RUST) *100

save(vars.m.all.5n, all.5, train.5 , test.5, rf.all.5 , auc.all.5, data.5n,
     vars.m.invading.5n, invading.5, train.5.invading , test.5.invading, rf.invading.5 , auc.invading.5,
     vars.m.established.5n, established.5, train.5.established , test.5.established, rf.established.5 , auc.established.5, 
     
     vars.m.all.10n, all.10, train.10 , test.10, rf.all.10 , auc.all.10, data.10n,
     vars.m.invading.10n, invading.10, train.10.invading , test.10.invading, rf.invading.10 , auc.invading.10,
     vars.m.established.10n, established.10, train.10.established , test.10.established, rf.established.10 , auc.established.10, 
     
     vars.m.all.20n, all.20, train.20 , test.20, rf.all.20 , auc.all.20, data.20n,
     file="/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/RF_MODELFIT_Results.RDATA")

##### 20 Years Invading: #####
invading.20 <- data.20n[which(data.20n$WPBR_stat == "Invading"),]

# Invading data models 
vars.m.invading.20 
vars.m.invading.20n <- c("RUST", "H_Zone" ,  "PRCP_Spring", "PRCP_Fall" , "PRCP_Summer",
                         "RH_Summer", "Tmin_Spring", "VPD_Summer", "VPD_Spring", "Tmin_Fall", "VPD_Fall", "StreamDen")

years.20.invading <- invading.20[, vars.m.invading.20n] %>% na.omit()

sample <- sample(c(TRUE, FALSE), nrow(years.20.invading ), replace=TRUE, prob=c(0.7,0.3))
train.20.invading   <- years.20.invading[sample, ]
test.20.invading    <- years.20.invading[!sample, ]

rf.invading.20 <- randomForest(factor(RUST) ~    H_Zone +     
                                 PRCP_Spring+ PRCP_Fall + PRCP_Summer+
                                 RH_Summer+ Tmin_Spring+ VPD_Summer+ VPD_Spring+ Tmin_Fall+ VPD_Fall+ StreamDen,
                               data=train.20.invading  , do.trace =TRUE,   importance=TRUE)

rf.invading.20

auc.invading.20  = performance( prediction(predict(rf.invading.20, type = "prob")[,2], train.20.invading$RUST), "auc")
auc.invading.20@y.values

test.20.invading$MODEL <- predict( rf.invading.20, test.20.invading, type = "prob")[,2]

test.20.invading$MODEL.c[test.20.invading$MODEL >= 0.50] <- 1
test.20.invading$MODEL.c[test.20.invading$MODEL < 0.50] <- 0

(length(test.20.invading$MODEL.c[ test.20.invading$MODEL.c == 1 & test.20.invading$RUST ==1]) + length(test.20.invading$MODEL.c[ test.20.invading$MODEL.c == 0 & test.20.invading$RUST ==0]) )/ length(test.20.invading$RUST) *100

save(vars.m.all.5n, all.5, train.5 , test.5, rf.all.5 , auc.all.5, data.5n,
     vars.m.invading.5n, invading.5, train.5.invading , test.5.invading, rf.invading.5 , auc.invading.5,
     vars.m.established.5n, established.5, train.5.established , test.5.established, rf.established.5 , auc.established.5, 
     
     vars.m.all.10n, all.10, train.10 , test.10, rf.all.10 , auc.all.10, data.10n,
     vars.m.invading.10n, invading.10, train.10.invading , test.10.invading, rf.invading.10 , auc.invading.10,
     vars.m.established.10n, established.10, train.10.established , test.10.established, rf.established.10 , auc.established.10, 
     
     vars.m.all.20n, all.20, train.20 , test.20, rf.all.20 , auc.all.20, data.20n,
     vars.m.invading.20n, invading.20, train.20.invading , test.20.invading, rf.invading.20 , auc.invading.20,
     file="/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/RF_MODELFIT_Results.RDATA")

##### 20 Years Established: #####

established.20 <- data.20n[which(data.20n$WPBR_stat == "Established"),]

# All data models 
vars.m.established.20 

vars.m.established.20n <- c("RUST", "H_Zone" , "PRCP_Spring", "PRCP_Summer","PRCP_Fall", 'RH_Summer','RH_Fall',"VPD_Spring","VPD_Fall",'Tmin_Spring', "StreamDen")


years.20.established <- established.20[, vars.m.established.20n] %>%na.omit()

sample <- sample(c(TRUE, FALSE), nrow(years.20.established ), replace=TRUE, prob=c(0.7,0.3))
train.20.established   <- years.20.established[sample, ]
test.20.established   <- years.20.established[!sample, ]

rf.established.20 <- randomForest(factor(RUST) ~    H_Zone + PRCP_Spring + PRCP_Summer + PRCP_Fall + RH_Summer+ RH_Fall +VPD_Spring+ VPD_Fall+ Tmin_Spring+ StreamDen,
                                  data=train.20.established  , do.trace =TRUE,   importance=TRUE)

rf.established.20 

auc.established.20  = performance( prediction(predict(rf.established.20, type = "prob")[,2], train.20.established$RUST), "auc")
auc.established.20@y.values

test.20.established$MODEL <- predict( rf.established.20, test.20.established, type = "prob")[,2]

test.20.established$MODEL.c[test.20.established$MODEL >= 0.50] <- 1
test.20.established$MODEL.c[test.20.established$MODEL < 0.50] <- 0

(length(test.20.established$MODEL.c[ test.20.established$MODEL.c == 1 & test.20.established$RUST ==1]) + length(test.20.established$MODEL.c[ test.20.established$MODEL.c == 0 & test.20.established$RUST ==0]) )/ length(test.20.established$RUST) *100

save(vars.m.all.5n, all.5, train.5 , test.5, rf.all.5 , auc.all.5, data.5n,
     vars.m.invading.5n, invading.5, train.5.invading , test.5.invading, rf.invading.5 , auc.invading.5,
     vars.m.established.5n, established.5, train.5.established , test.5.established, rf.established.5 , auc.established.5, 
     
     vars.m.all.10n, all.10, train.10 , test.10, rf.all.10 , auc.all.10, data.10n,
     vars.m.invading.10n, invading.10, train.10.invading , test.10.invading, rf.invading.10 , auc.invading.10,
     vars.m.established.10n, established.10, train.10.established , test.10.established, rf.established.10 , auc.established.10, 
     
     vars.m.all.20n, all.20, train.20 , test.20, rf.all.20 , auc.all.20, data.20n,
     vars.m.invading.20n, invading.20, train.20.invading , test.20.invading, rf.invading.20 , auc.invading.20,
     vars.m.established.20n, established.20, train.20.established , test.20.established, rf.established.20 , auc.established.20, 
     file="/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/RF_MODELFIT_Results.RDATA")

# Load Models:
rm(list=ls())

load("/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/RF_MODELFIT_Results.RDATA")

importance(rf.invading.10)
