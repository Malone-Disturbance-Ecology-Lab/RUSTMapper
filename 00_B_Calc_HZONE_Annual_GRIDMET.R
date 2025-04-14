library(tidyverse)
library(sf)
library(AOI)
library(terra)

rm(list=ls())
# Imports the climate change data:
AOI = aoi_get(state = c("CO", "WA", "OR", "CA", "MT", "ID", "UT", "AZ", "NV", "WY", "NM", "TX"))

data.dir <- "/Volumes/MaloneLab/Research/RUSTMAPPER"
figure.dir <- "/Users/sm3466/Dropbox (YSE)/Research/RUSTMapper/FIGURES"
setwd(data.dir)

source( '/Users/sm3466/Dropbox (YSE)/Research/RUSTMapper/01_PrepareClimateLayersForUSE.R')

mydir <- paste( data.dir,"/GRIDMET_CURRENT",sep="")

delfiles <- dir(path=mydir , pattern="*json")
file.remove(file.path(mydir, delfiles))

source( '/Users/sm3466/Dropbox (YSE)/Research/RUSTMapper/00_FUNCTION_ComplieClimteRaters.R')

template <- climate.import.gridmet(pattern = "Tmin_Winter")
Tmin <- c(climate.import.gridmet(pattern = "Tmin_Winter") , climate.import.future(pattern = "Tmin_Summer") %>% resample(template ))


H_Zone <- read.csv(paste(data.dir,'/HardinessZones.csv', sep =""))

CALC_H_ZONE <- function( TMIN, H_ZONE){
  H_Zone_rast <- Tmin
  H_Zone_rast[H_Zone_rast > -100] <- NA
  H_Zone_rast[ Tmin <= H_Zone$Temp_upper[H_Zone$H_Zone == 1] ] <- 1
  H_Zone_rast[ Tmin <= H_Zone$Temp_upper[H_Zone$H_Zone == 2] & Tmin >= H_Zone$Temp_Lower[H_Zone$H_Zone == 2]] <- 2
  H_Zone_rast[ Tmin <= H_Zone$Temp_upper[H_Zone$H_Zone == 3] & Tmin >= H_Zone$Temp_Lower[H_Zone$H_Zone == 3]] <- 3
  H_Zone_rast[ Tmin <= H_Zone$Temp_upper[H_Zone$H_Zone == 4] & Tmin >= H_Zone$Temp_Lower[H_Zone$H_Zone == 4]] <- 4
  H_Zone_rast[ Tmin <= H_Zone$Temp_upper[H_Zone$H_Zone == 5] & Tmin >= H_Zone$Temp_Lower[H_Zone$H_Zone == 5]] <- 5
  H_Zone_rast[ Tmin <= H_Zone$Temp_upper[H_Zone$H_Zone == 6] & Tmin >= H_Zone$Temp_Lower[H_Zone$H_Zone == 6]] <- 6
  H_Zone_rast[ Tmin <= H_Zone$Temp_upper[H_Zone$H_Zone == 7] & Tmin >= H_Zone$Temp_Lower[H_Zone$H_Zone == 7]] <- 7
  H_Zone_rast[ Tmin <= H_Zone$Temp_upper[H_Zone$H_Zone == 8] & Tmin >= H_Zone$Temp_Lower[H_Zone$H_Zone == 8]] <- 8
  H_Zone_rast[ Tmin <= H_Zone$Temp_upper[H_Zone$H_Zone == 9] & Tmin >= H_Zone$Temp_Lower[H_Zone$H_Zone == 9]] <- 9
  H_Zone_rast[ Tmin <= H_Zone$Temp_upper[H_Zone$H_Zone == 10] & Tmin >= H_Zone$Temp_Lower[H_Zone$H_Zone == 10]] <- 10
  H_Zone_rast[ Tmin <= H_Zone$Temp_upper[H_Zone$H_Zone == 11] & Tmin >= H_Zone$Temp_Lower[H_Zone$H_Zone == 11]] <- 11
  H_Zone_rast[ Tmin <= H_Zone$Temp_upper[H_Zone$H_Zone == 12] & Tmin >= H_Zone$Temp_Lower[H_Zone$H_Zone == 12]] <- 12
  H_Zone_rast[  Tmin >= H_Zone$Temp_Lower[H_Zone$H_Zone == 13] ] <- 13
  return(H_Zone_rast )
}

H_Zone_annual <- CALC_H_ZONE(TMIN=TMIN, H_ZONE= H_Zone)

Tmin.mean <- mean( Tmin, na.rm=T)

H_Zone_Tmin.mean <- CALC_H_ZONE(TMIN=Tmin, H_ZONE= H_Zone) %>% focal( w=9, fun=mean, na.policy="only", na.rm=T) %>% round(1)

H_Zone_annual.filled <- terra::cover( H_Zone_annual, H_Zone_Tmin.mean)


writeRaster(H_Zone_annual.filled,paste(data.dir,"/H_Zone_1980-2099.tif", sep =""),overwrite=T)

