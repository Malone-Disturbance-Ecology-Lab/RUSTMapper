# Model Projections ####
# Layers for Spatial projection

rm(list=ls())


library(dplyr)
library(sf)
library(AOI)
library(terra)
library(randomForest)

data.dir <- "/Volumes/MaloneLab/Research/RUSTMAPPER"
figure.dir <- "/Users/sm3466/Dropbox (YSE)/Research/RUSTMapper/FIGURES"
setwd(data.dir)

# Project Models #####
# Import Climate Layers 2000-2099+ makes the AOI:
source( '/Users/sm3466/Dropbox (YSE)/Research/RUSTMapper/01_PrepareClimateLayersForUSE.R')

# Imports the models
setwd(data.dir)
load("RF_MODELFIT_Results_DAYMET.RDATA")

# Preparing stream and Hardinesszone layers:
streamDen <- rast('StreamDensity_WestUSA_1km.tif')[[5]] %>% terra::project(RH_Spring) %>% terra::resample(RH_Spring, method="bilinear")
streamDen[is.na(streamDen)] <- 0
names(streamDen) <- "StreamDen"

H_ZONE <- rast("H_Zone_1980-2099.tif") %>% terra::project(RH_Spring) %>% terra::resample(RH_Spring, method="bilinear")

tpi <- rast('tpi_2021.tif')%>% terra::project(RH_Spring) %>% terra::resample(RH_Spring, method="bilinear")
tri <-rast('tri_2021.tif') %>% terra::project(RH_Spring) %>% terra::resample(RH_Spring, method="bilinear")

# Functions
invading.projections <- function(Data){
  Raster.invading.5.2030 <- terra::predict( Data, rf.invading.5,  na.rm=T, cpkgs="randomForest", type="prob")
  Raster.invading.10.2030 <- terra::predict( Data, rf.invading.10,  na.rm=T, cpkgs="randomForest", type="prob")
  Raster.invading.20.2030 <- terra::predict( Data, rf.invading.20,  na.rm=T, cpkgs="randomForest", type="prob")
  
  ensemble.invading.2030 <- mean(Raster.invading.5.2030$X1,  Raster.invading.10.2030$X1, Raster.invading.20.2030$X1)
  return( ensemble.invading.2030)
}
established.projections <- function(Data){
  
  
  
  Raster.established.5.2030 <- terra::predict( Data, rf.established.5,  na.rm=T, cpkgs="randomForest", type="prob")
  Raster.established.10.2030 <- terra::predict( Data, rf.established.10,  na.rm=T, cpkgs="randomForest", type="prob")
  Raster.established.20.2030 <- terra::predict( Data, rf.established.20,  na.rm=T, cpkgs="randomForest", type="prob")
  
  ensemble.establshed.2030 <- mean(Raster.established.5.2030$X1,  Raster.established.10.2030$X1, Raster.established.20.2030$X1)
  
  return( ensemble.establshed.2030)
}

# Get time from object
time <- time(H_ZONE) %>% as.numeric

# Ensemble Estimates: ####

for( i in 1:length(time)){
  
  print(i)
  
  Data <- c( PRCP_Spring[[i]], PRCP_Summer[[i]], PRCP_Fall[[i]],
             Tmin_Spring[[i]], Tmin_Summer[[i]], Tmin_Fall[[i]],
             Tmax_Spring[[i]], Tmax_Summer[[i]], Tmax_Fall[[i]],
             Tmean_Spring[[i]], Tmean_Summer[[i]], Tmean_Fall[[i]],
             VPD_Spring[[i]], VPD_Summer[[i]], VPD_Fall[[i]],
             VPDmax_Spring[[i]], VPDmax_Summer[[i]], VPDmax_Fall[[i]],
             RH_Spring[[i]], RH_Summer[[i]], RH_Fall[[i]], streamDen,H_ZONE[[i]],tri, tpi)
  
  list.names <- c( 'PRCP_Spring', 'PRCP_Summer', 'PRCP_Fall',
                   'Tmin_Spring', 'Tmin_Summer', 'Tmin_Fall',
                   'Tmax_Spring', 'Tmax_Summer', 'Tmax_Fall',
                   'Tmean_Spring', 'Tmean_Summer', 'Tmean_Fall',
                   'VPD_Spring', 'VPD_Summer', 'VPD_Fall',
                   'VPDmax_Spring', 'VPDmax_Summer', 'VPDmax_Fall',
                   'RH_Spring', 'RH_Summer', 'RH_Fall', 'StreamDen','H_Zone', 'TPI', 'TRI')
  
  names(Data) <- list.names
  
  inv.ensemble <- invading.projections(Data)
  est.ensemble <- established.projections(Data)
  
  time(inv.ensemble) <- time(time[i])
  time(est.ensemble) <- time(time[i])
  setwd(paste(data.dir,"/Current_RF_Projections",sep=""))
  
  writeRaster( inv.ensemble ,paste('inv.ensemble',  time[i], ".tiff" ,sep="_" ) , overwrite=T)
  writeRaster( est.ensemble ,paste('est.ensemble',  time[i], ".tiff" ,sep="_" ) , overwrite=T)
}

# Must calculate the SD of the ensemble: ####

invading.projections.sd <- function(Data){
  Raster.invading.5.2030 <- terra::predict( Data, rf.invading.5,  na.rm=T, cpkgs="randomForest", type="prob")
  Raster.invading.10.2030 <- terra::predict( Data, rf.invading.10,  na.rm=T, cpkgs="randomForest", type="prob")
  Raster.invading.20.2030 <- terra::predict( Data, rf.invading.20,  na.rm=T, cpkgs="randomForest", type="prob")
  
  ensemble.invading.2030 <- stdev(Raster.invading.5.2030$X1,  Raster.invading.10.2030$X1, Raster.invading.20.2030$X1) 
  
  return( ensemble.invading.2030)
}
established.projections.sd <- function(Data){
  
  Raster.established.5.2030 <- terra::predict( Data, rf.established.5,  na.rm=T, cpkgs="randomForest", type="prob")
  Raster.established.10.2030 <- terra::predict( Data, rf.established.10,  na.rm=T, cpkgs="randomForest", type="prob")
  Raster.established.20.2030 <- terra::predict( Data, rf.established.20,  na.rm=T, cpkgs="randomForest", type="prob")
  
  ensemble.establshed.2030 <-  stdev(Raster.established.5.2030$X1,  Raster.established.10.2030$X1, Raster.established.20.2030$X1)
  
  return( ensemble.establshed.2030)
}

for( i in 1:length(time[1:44])){
  
  print(i)
  
  Data <- c( PRCP_Spring[[i]], PRCP_Summer[[i]], PRCP_Fall[[i]],
             Tmin_Spring[[i]], Tmin_Summer[[i]], Tmin_Fall[[i]],
             Tmax_Spring[[i]], Tmax_Summer[[i]], Tmax_Fall[[i]],
             Tmean_Spring[[i]], Tmean_Summer[[i]], Tmean_Fall[[i]],
             VPD_Spring[[i]], VPD_Summer[[i]], VPD_Fall[[i]],
             VPDmax_Spring[[i]], VPDmax_Summer[[i]], VPDmax_Fall[[i]],
             RH_Spring[[i]], RH_Summer[[i]], RH_Fall[[i]], streamDen,H_ZONE[[i]], tpi, tri)
  
  list.names <- c( 'PRCP_Spring', 'PRCP_Summer', 'PRCP_Fall',
                   'Tmin_Spring', 'Tmin_Summer', 'Tmin_Fall',
                   'Tmax_Spring', 'Tmax_Summer', 'Tmax_Fall',
                   'Tmean_Spring', 'Tmean_Summer', 'Tmean_Fall',
                   'VPD_Spring', 'VPD_Summer', 'VPD_Fall',
                   'VPDmax_Spring', 'VPDmax_Summer', 'VPDmax_Fall',
                   'RH_Spring', 'RH_Summer', 'RH_Fall', 'StreamDen','H_Zone','TPI', 'TRI')
  
  names(Data) <- list.names
  
  inv.ensemble <- invading.projections.sd(Data)
  est.ensemble <- established.projections.sd(Data)
  
  time(inv.ensemble) <- time(time[i])
  time(est.ensemble) <- time(time[i])
  setwd(paste(data.dir,"/Current_RF_Projections/SD",sep=""))
  
  writeRaster( inv.ensemble ,paste('inv.ensemble.sd',  time[i], ".tiff" ,sep="_" ) , overwrite=T)
  writeRaster( est.ensemble ,paste('est.ensemble.sd',  time[i], ".tiff" ,sep="_" ) , overwrite=T)
  
}
  # Remove all .json files that stand in your way.
setwd(paste(data.dir,"/Current_RF_Projections",sep=""))
  file.remove(list.files(pattern = "*.json"))

  setwd(paste(data.dir,"/Current_RF_Projections/SD",sep=""))
  file.remove(list.files(pattern = "*.json"))
  
# Ensemble Layers for use: #####
  
setwd(data.dir)
load("Final_ShapeFiles.RDATA")
  
# Create a List of the files you want to import
files.est <- list.files(path=paste(data.dir,"/Current_RF_Projections",sep=""), pattern = "est")

files.inv <- list.files(path=paste(data.dir,"/Current_RF_Projections",sep=""), pattern = "inv")
  
  # Create a List of the files you want to import
files.est.sd <- list.files(path=paste(data.dir,"/Current_RF_Projections/SD",sep=""), pattern = "est")
  
files.inv.sd <- list.files(path=paste(data.dir,"/Current_RF_Projections/SD",sep=""), pattern = "inv")
  
  setwd(paste(data.dir,"/Current_RF_Projections",sep=""))
  
  ensemble.est <- rast(files.est ) %>% mask(wp) %>% crop(AOI)
  ensemble.inv <- rast(files.inv ) %>% mask(wp.s) %>% crop(AOI)  
  
  setwd(paste(data.dir,"/Current_RF_Projections/SD",sep=""))
  ensemble.est.sd  <- rast(files.est.sd ) %>% mask(wp) %>% crop(AOI)
  ensemble.inv.sd  <- rast(files.inv.sd  )%>% mask(wp.s) %>% crop(AOI)
  
  # Add the time and names into these layers.
  terra::time(ensemble.est) <- c(seq(1980, 2023, 1),seq(2030, 2099, 1))
  names(ensemble.est) <- paste("est", c(seq(1980, 2023, 1),seq(2030, 2099, 1)), sep = "-")
  
  terra::time(ensemble.inv) <- c(seq(1980, 2023, 1),seq(2030, 2099, 1))
  names(ensemble.inv) <- paste("inv", c(seq(1980, 2023, 1),seq(2030, 2099, 1)), sep = "-")
  
  terra::time(ensemble.est.sd) <- seq(1980, 2023, 1)
  names(ensemble.est.sd) <- paste("est.sd", seq(1980, 2023, 1), sep = "-")
  
  terra::time(ensemble.inv.sd) <- seq(1980, 2023, 1)
  names(ensemble.inv.sd) <- paste("inv.sd", seq(1980, 2023, 1), sep = "-")
  
  rm( files.est,
      files.inv, 
      files.inv.sd, files.est.sd)
  
  writeRaster(ensemble.est, paste(data.dir,"/Ensemble_1980-2099_EST.tif", sep=""), overwrite=TRUE)
  writeRaster(ensemble.inv, paste(data.dir,"/Ensemble_1980-2099_INV.tif",sep=""), overwrite=TRUE)
  
  writeRaster(ensemble.est.sd, paste(data.dir,"/Ensemble_SD_1980-2099_EST.tif", sep=""), overwrite=TRUE)
  writeRaster(ensemble.inv.sd, paste(data.dir,"/Ensemble_SD_1980-2099_INV.tif",sep=""), overwrite=TRUE)
  
