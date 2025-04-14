# Create Function to compile Climate Data
library(tidyverse)
library(terra)
library(AOI)

climate.import.future <- function( pattern){
  # FUTRUE
  setwd(paste(data.dir,"/ClimateChangeProjections", sep=""))
  
  data.2030 <- rast(list.files(".", pattern=pattern) )
  AOI = sf::st_transform( AOI, crs(data.2030))
  terra::time(data.2030 ) <- seq(2030,2099)
  data.2030 <- mask(crop(data.2030, AOI), AOI)
  
  total.data <- data.2030 %>% terra::project("+proj=lcc +lat_0=42.5 +lon_0=-100 +lat_1=25 +lat_2=60 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
  
  return(total.data)
  
  
}

climate.import.normals <- function( pattern){
  setwd(paste(data.dir,"/DAYMET/NORMAL", sep=""))
  data.current <- terra::rast(list.files(".", pattern=pattern) )
  AOI = sf::st_transform( AOI, crs(data.current))
  terra::time(data.current ) <- seq(2000,2020)
  data.current <- mask(crop(data.current, AOI), AOI)
  units(data.current) <- pattern
  return(data.current)
}

climate.import.gridmet <- function( pattern){
  # FUTRUE
  setwd(paste(data.dir,"/GRIDMET_CURRENT", sep=""))
  data.2030 <- rast(list.files(".", pattern=pattern) )
  AOI = sf::st_transform( AOI, crs(data.2030))
  terra::time(data.2030 ) <- seq(1980,2023)
  data.2030 <- mask(crop(data.2030, AOI), AOI)
  
  total.data <- data.2030 %>% terra::project("+proj=lcc +lat_0=42.5 +lon_0=-100 +lat_1=25 +lat_2=60 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
  
  return(total.data)
}
