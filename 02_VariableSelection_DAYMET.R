#STEP2_Variable Selection:

library( VSURF)
library(dplyr)
library(tidyverse)
library(parallel)
# https://www.rdocumentation.org/packages/VSURF/versions/1.1.0/topics/VSURF

load('/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/Final Scripts/WPBR_plots_DAYMET.RDATA')


###### 5 year model fit ####
data.5 <- data.5years %>% filter( RUST <=1)

# All data models 
vars.m.all.5n <- c("RUST", "H_Zone" , "WPBR_stat",
                   "Tmin_Spring" , "Tmin_Summer", "Tmin_Fall",
                   "Tmax_Spring" , "Tmax_Summer", "Tmax_Fall",
                   "PRCP_Spring" , "PRCP_Summer", "PRCP_Fall", 
                   "VPD_Fall" , "VPD_Summer","VPD_Fall",
                   "VPDmax_Spring","VPDmax_Summer","VPDmax_Fall",
                   "RH_Spring", "RH_Summer", "RH_Fall",
                   "TPI","TRI", "StreamDen")


data.5n <- data.5 %>% dplyr::select(vars.m.all.5n) %>% filter(RUST < 2) %>% na.omit()

data.5n$RUST <- data.5n$RUST %>% as.factor

established <- data.5n[which(data.5n$WPBR_stat == "Established"),]
invading <- data.5n[which(data.5n$WPBR_stat == "Invading"),]

# All
# Fit vars in two groups climate and site
names(data.5n)
m.all.5n.site <- VSURF(data.5n[, c(2, 4:23)], data.5n[, 1], ntree = 500,
                   RFimplem = "randomForest", 
                   clusterType = "PSOCK", 
                   verbose = TRUE,
                   ncores = detectCores() - 2, parallel= TRUE)

vars.m.all.5 <-unique(names( data.5n[, c(2, 4:23)])[ m.all.5n.site$varselect.pred])

# Invading
m.invading.5 <- VSURF(invading[, c(2, 4:23)], invading[, 1], ntree = 1000,
                      RFimplem = "randomForest", 
                      clusterType = "PSOCK", 
                      verbose = TRUE,
                      ncores = detectCores() - 2, parallel= TRUE)

vars.m.invading.5 <- names( invading[, c(2, 4:23)]) [m.invading.5$varselect.interp] # Used .interp 

# Established
# Fit vars in two groups climate and site
m.established.5 <- VSURF(established[, c(2, 4:23)], established[, 1], ntree = 1000,
                         RFimplem = "randomForest", 
                         clusterType = "PSOCK", 
                         verbose = TRUE,
                         ncores = detectCores() - 2, parallel= TRUE)

vars.m.established.5 <-names( established[, c(2, 4:23)]) [m.established.5$varselect.pred]

# Three steps variable selection procedure based on random forests for supervised classification and regression problems. First step ("thresholding step") is dedicated to eliminate irrelevant variables from the dataset. Second step ("interpretation step") aims to select all variables related to the re- sponse for interpretation purpose. Third step ("prediction step") refines the selection by eliminating redundancy in the set of variables selected by the second step, for prediction purpose.

save(m.all.5n.site, vars.m.all.5, 
     m.invading.5,vars.m.invading.5,
     m.established.5, vars.m.established.5,
     file = "/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/Final Scripts/WPBR_Variable_Selction_5_DAYMET.RData")

###### 10 year model fit ####
data.10 <- data.10years %>% filter( RUST <=1)

names(data.10)
# All data models 
vars.m.all.10n <- c("RUST", "H_Zone" , "WPBR_stat",
                   "Tmin_Spring" , "Tmin_Summer", "Tmin_Fall",
                   "Tmax_Spring" , "Tmax_Summer", "Tmax_Fall",
                   "PRCP_Spring" , "PRCP_Summer", "PRCP_Fall", 
                   "VPD_Fall" , "VPD_Summer","VPD_Fall",
                   "VPDmax_Spring","VPDmax_Summer","VPDmax_Fall",
                   "RH_Spring", "RH_Summer", "RH_Fall",
                   "TPI","TRI", "StreamDen")


data.10n <- data.10 %>% dplyr::select(vars.m.all.10n) %>% filter(RUST < 2) %>% na.omit()

data.10n$RUST <- data.10n$RUST %>% as.factor

established <- data.10n[which(data.10n$WPBR_stat == "Established"),]
invading <- data.10n[which(data.10n$WPBR_stat == "Invading"),]

# All
m.all.10n.site <- VSURF(data.10n[, c(2, 4:23)], data.10n[, 1], ntree = 1000,
                       RFimplem = "randomForest", 
                       clusterType = "PSOCK", 
                       verbose = TRUE,
                       ncores = detectCores() - 2, parallel= TRUE)

vars.m.all.10 <-unique(names( data.10n[, c(2, 4:23)])[ m.all.10n.site$varselect.pred])

# Invading
# Fit vars in two groups climate and site

names(invading)
m.invading.10 <- VSURF(invading[, c(2, 4:23)], invading[, 1], ntree = 1000,
                      RFimplem = "randomForest", 
                      clusterType = "PSOCK", 
                      verbose = TRUE,
                      ncores = detectCores() - 2, parallel= TRUE)

vars.m.invading.10 <- names( invading[, c(2, 4:23)]) [m.invading.10$varselect.pred]

# Established
# Fit vars in two groups climate and site
m.established.10 <- VSURF(established[, c(2, 4:23)], established[, 1], ntree = 1000,
                         RFimplem = "randomForest", 
                         clusterType = "PSOCK", 
                         verbose = TRUE,
                         ncores = detectCores() - 2, parallel= TRUE)

vars.m.established.10 <-names( established[, c(2, 4:23)]) [m.established.10$varselect.pred]

# Three steps variable selection procedure based on random forests for supervised classification and regression problems. First step ("thresholding step") is dedicated to eliminate irrelevant variables from the dataset. Second step ("interpretation step") aims to select all variables related to the re- sponse for interpretation purpose. Third step ("prediction step") refines the selection by eliminating redundancy in the set of variables selected by the second step, for prediction purpose.

save(m.all.10n.site, vars.m.all.10, 
     m.invading.10,vars.m.invading.10,
     m.established.10, vars.m.established.10,
     file = "/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/Final Scripts/WPBR_Variable_Selction_10_DAYMET.RData")
###### 20 year model fit ####
data.20 <- data.20years %>% filter( RUST <=1)

names(data.20)
# All data models 
vars.m.all.20n <- c("RUST", "H_Zone" , "WPBR_stat",
                   "Tmin_Spring" , "Tmin_Summer", "Tmin_Fall",
                   "Tmax_Spring" , "Tmax_Summer", "Tmax_Fall",
                   "PRCP_Spring" , "PRCP_Summer", "PRCP_Fall", 
                   "VPD_Fall" , "VPD_Summer","VPD_Fall",
                   "VPDmax_Spring","VPDmax_Summer","VPDmax_Fall",
                   "RH_Spring", "RH_Summer", "RH_Fall",
                   "TPI","TRI", "StreamDen")


data.20n <- data.20 %>% dplyr::select(vars.m.all.20n) %>% filter(RUST < 2) %>% na.omit()

data.20n$RUST <- data.20n$RUST %>% as.factor

established <- data.20n[which(data.20n$WPBR_stat == "Established"),]
invading <- data.20n[which(data.20n$WPBR_stat == "Invading"),]

# All
# Fit vars in two groups climate and site

m.all.20n.site <- VSURF(data.20n[, c(2, 4:23)], data.20n[, 1], ntree = 2000,
                       RFimplem = "randomForest", 
                       clusterType = "PSOCK", 
                       verbose = TRUE,
                       ncores = detectCores() - 2, parallel= TRUE)

vars.m.all.20 <-unique(names( data.20n[, c(2, 4:23)])[ m.all.20n.site$varselect.pred])

# Invading
# Fit vars in two groups climate and site

names(invading)
m.invading.20 <- VSURF(invading[, c(2, 4:23)], invading[, 1], ntree = 1000,
                      RFimplem = "randomForest", 
                      clusterType = "PSOCK", 
                      verbose = TRUE,
                      ncores = detectCores() - 2, parallel= TRUE)

vars.m.invading.20 <- names( invading[, c(2, 4:23)]) [m.invading.20$varselect.pred]

# Established
# Fit vars in two groups climate and site
m.established.20 <- VSURF(established[, c(2, 4:23)], established[, 1], ntree = 1000,
                         RFimplem = "randomForest", 
                         clusterType = "PSOCK", 
                         verbose = TRUE,
                         ncores = detectCores() - 2, parallel= TRUE)

vars.m.established.20 <-names( established[, c(2, 4:23)]) [m.established.20$varselect.pred]

# Three steps variable selection procedure based on random forests for supervised classification and regression problems. First step ("thresholding step") is dedicated to eliminate irrelevant variables from the dataset. Second step ("interpretation step") aims to select all variables related to the re- sponse for interpretation purpose. Third step ("prediction step") refines the selection by eliminating redundancy in the set of variables selected by the second step, for prediction purpose.

save(m.all.20n.site, vars.m.all.20, 
     m.invading.20,vars.m.invading.20,
     m.established.20, vars.m.established.20,
     file = "/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/Final Scripts/WPBR_Variable_Selction_20_DAYMET.RData")
# EOF

