# Model Projections ####
# Layers for Spatial projection

rm(list=ls())


library(dplyr)
library(sf)
library(AOI)
library(terra)
library(randomForest)

# Import Climate Layers 2000-2099+ makes the AOI:
source( '/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/Final Scripts/01_PrepareClimateLayersForUSE.R')

# Imports the models
load("/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/Final Scripts/RF_MODELFIT_Results_DAYMET.RDATA")

# Preparing stream and Hardinesszone layers:
streamDen <- rast('~/Dropbox (YSE)/Research/WPBR/StreamDensity_WestUSA_1km.tif')[[5]] %>% terra::project(RH_Spring) %>% terra::resample(RH_Spring, method="bilinear")
streamDen[is.na(streamDen)] <- 0
names(streamDen) <- "StreamDen"

H_ZONE <- rast("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/WPBR/NewData/Spatial/H_Zone_1980-2099.tif") %>% terra::project(RH_Spring) %>% terra::resample(RH_Spring, method="bilinear")

tpi <- rast('/Users/sm3466/Dropbox (YSE)/Research/WPBR/Shapefiles/tpi_2021.tif')%>% terra::project(RH_Spring) %>% terra::resample(RH_Spring, method="bilinear")
tri <-rast('/Users/sm3466/Dropbox (YSE)/Research/WPBR/Shapefiles/tri_2021.tif') %>% terra::project(RH_Spring) %>% terra::resample(RH_Spring, method="bilinear")

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
                   'RH_Spring', 'RH_Summer', 'RH_Fall', 'StreamDen','H_ZONE', 'TPI', 'TRI')
  
  names(Data) <- list.names
  
  inv.ensemble <- invading.projections(Data)
  est.ensemble <- established.projections(Data)
  
  time(inv.ensemble) <- time(time[i])
  time(est.ensemble) <- time(time[i])
  setwd("/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/Spatial/Current_RF_Projections")
  
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
                   'RH_Spring', 'RH_Summer', 'RH_Fall', 'StreamDen','H_ZONE','TPI', 'TRI')
  
  names(Data) <- list.names
  
  inv.ensemble <- invading.projections.sd(Data)
  est.ensemble <- established.projections.sd(Data)
  
  time(inv.ensemble) <- time(time[i])
  time(est.ensemble) <- time(time[i])
  setwd("/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/Spatial/Current_RF_Projections/SD")
  
  writeRaster( inv.ensemble ,paste('inv.ensemble.sd',  time[i], ".tiff" ,sep="_" ) , overwrite=T)
  writeRaster( est.ensemble ,paste('est.ensemble.sd',  time[i], ".tiff" ,sep="_" ) , overwrite=T)
  
}
  # Remove all .json files that stand in your way.
  setwd('/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/Spatial/Current_RF_Projections')
  file.remove(list.files(pattern = "*.json"))
  
  setwd('/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/Spatial/Current_RF_Projections/SD')
  file.remove(list.files(pattern = "*.json"))
