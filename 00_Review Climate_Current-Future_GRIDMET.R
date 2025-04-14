
# Compare Current and Future Climate Conditions of the Landscape

library(tidyverse)
library(sf)
library(AOI)

# Explore data to determine the limits of model development:
# Look at the climate change data for the locations:

data.dir <- "/Volumes/MaloneLab/Research/RUSTMAPPER"
setwd(data.dir)

load("RF_MODELFIT_Results_DAYMET.RDATA")

AOI = aoi_get(state = c("CO", "WA", "OR", "CA", "MT", "ID", "UT", "AZ", "NV", "WY", "NM", "TX"))

# Imports the climate change data:
source( '00_FUNCTION_ComplieClimteRaters.R')

# Imports current and future climate layers
template <- climate.import.gridmet(pattern = "PRCP_Fall")

# Future years need to be adjusted by 5
PRCP_Fall <- c(climate.import.gridmet(pattern = "PRCP_Fall"), climate.import.future(pattern = "PRCP_Fall") %>% resample(template ))
PRCP_Spring <- c(climate.import.gridmet(pattern = "PRCP_Spring"), climate.import.future(pattern = "PRCP_Spring") %>% resample(template ))
PRCP_Summer <- c(climate.import.gridmet(pattern = "PRCP_Summer"), climate.import.future(pattern = "PRCP_Summer") %>% resample(template ))

PRCP_Fall <- c(PRCP_Fall[[1:44]], PRCP_Fall[[45:114]]/5)
PRCP_Summer <- c(PRCP_Summer[[1:44]], PRCP_Summer[[45:114]]/5)
PRCP_Spring <- c(PRCP_Spring[[1:44]], PRCP_Spring[[45:114]]/5)

Tmin_Fall <- c(climate.import.gridmet(pattern = "Tmin_Fall") , climate.import.future(pattern = "Tmin_Fall") %>% resample(template ))
Tmin_Spring <- c(climate.import.gridmet(pattern = "Tmin_Spring") , climate.import.future(pattern = "Tmin_Spring") %>% resample(template ))
Tmin_Summer <- c(climate.import.gridmet(pattern = "Tmin_Summer") , climate.import.future(pattern = "Tmin_Summer") %>% resample(template ))

Tmax_Fall <- c(climate.import.gridmet(pattern = "Tmax_Fall") , climate.import.future(pattern = "Tmax_Fall") %>% resample(template ))
Tmax_Spring <- c(climate.import.gridmet(pattern = "Tmax_Spring"), climate.import.future(pattern = "Tmax_Spring") %>% resample(template ))
Tmax_Summer <- c(climate.import.gridmet(pattern = "Tmax_Summer"), climate.import.future(pattern = "Tmax_Summer") %>% resample(template ))

VPD_Fall <- c(climate.import.gridmet(pattern = "VPD_fall"), climate.import.future(pattern = "VPD_fall") %>% resample(template ))
VPD_Spring <- c(climate.import.gridmet(pattern = "VPD_Spring"), climate.import.future(pattern = "VPD_Spring") %>% resample(template ))
VPD_Summer <- c(climate.import.gridmet(pattern = "VPD_Summer"), climate.import.future(pattern = "VPD_Summer") %>% resample(template ))

VPDMAX_Fall <- c(climate.import.gridmet(pattern = "VPDMAX_fall") , climate.import.future(pattern = "VPDMAX_fall") %>% resample(template ))
VPDMAX_Spring <- c(climate.import.gridmet(pattern = "VPDMAX_Spring") , climate.import.future(pattern = "VPDMAX_Spring") %>% resample(template ))
VPDMAX_Summer <- c(climate.import.gridmet(pattern = "VPDMAX_Summer") , climate.import.future(pattern = "VPDMAX_Summer") %>% resample(template ))

RH_Fall <- c(climate.import.gridmet(pattern = "RH.Fall") , climate.import.future(pattern = "RH.Fall") %>% resample(template ))
RH_Spring <- c(climate.import.gridmet(pattern = "RH.Spring") , climate.import.future(pattern = "RH.Spring") %>% resample(template))
RH_Summer <- c(climate.import.gridmet(pattern = "RH.Summer") , climate.import.future(pattern = "RH.Summer") %>% resample(template))

library(sf)
names(data.5years)
data.5n.sf <- st_as_sf(data.5years, coords = c('POINT_X', 'POINT_Y'), crs = 4326)


# Create Mean layers
future.mean <- function( raster, label){
  raster.n <- raster[[45:114]] %>% mean(na.rm=T)
  names(raster.n) <- label
  return( raster.n)
}
current.mean <- function( raster, label){
  raster.n <- raster[[1:44]] %>% mean(na.rm=T)
  names(raster.n) <- label
  return( raster.n)
}

Tmin_Spring.mean <- Tmin_Spring %>% future.mean( label ="Tmin_Spring")
Tmin_Summer.mean <- Tmin_Summer %>% future.mean( label ="Tmin_Summer")
Tmin_Fall.mean <- Tmin_Fall %>% future.mean( label ="Tmin_Fall")

Tmax_Spring.mean <- Tmax_Spring %>% future.mean( label ="Tmax_Spring")
Tmax_Summer.mean <- Tmax_Summer %>% future.mean( label ="Tmax_Summer")
Tmax_Fall.mean <- Tmax_Fall %>% future.mean( label ="Tmax_Fall")

Tmax_Spring.mean <- Tmax_Spring %>% future.mean( label ="Tmax_Spring")
Tmax_Summer.mean <- Tmax_Summer %>% future.mean( label ="Tmax_Summer")
Tmax_Fall.mean <- Tmax_Fall %>% future.mean( label ="Tmax_Fall")

PRCP_Spring.mean <- PRCP_Spring %>% future.mean( label ="PRCP_Spring")
PRCP_Summer.mean <- PRCP_Summer %>% future.mean( label ="PRCP_Summer")
PRCP_Fall.mean <- PRCP_Fall %>% future.mean( label ="PRCP_Fall")

VPD_Spring.mean <- VPD_Spring %>% future.mean( label ="VPD_Spring")
VPD_Summer.mean <- VPD_Summer %>% future.mean( label ="VPD_Summer")
VPD_Fall.mean <- VPD_Fall %>% future.mean( label ="VPD_Fall")

VPDmax_Spring.mean <- VPDMAX_Spring %>% future.mean( label ="VPDmax_Spring")
VPDmax_Summer.mean <- VPDMAX_Summer %>% future.mean( label ="VPDmax_Summer")
VPDmax_Fall.mean <- VPDMAX_Fall %>% future.mean( label ="VPDmax_Fall")

RH_Spring.mean <- RH_Spring %>% future.mean( label ="RH_Spring")
RH_Summer.mean <- RH_Summer %>% future.mean( label ="RH_Summer")
RH_Fall.mean <- RH_Fall %>% future.mean( label ="RH_Fall")

future.mean.climate <- c(Tmin_Spring.mean,Tmin_Summer.mean,Tmin_Fall.mean,
                         Tmax_Spring.mean,Tmax_Summer.mean,Tmax_Fall.mean,
                         Tmax_Spring.mean,Tmax_Summer.mean,Tmax_Fall.mean,
                         PRCP_Spring.mean,PRCP_Summer.mean,PRCP_Fall.mean,
                         VPD_Spring.mean,VPD_Summer.mean,VPD_Fall.mean,
                         VPDmax_Spring.mean,VPDmax_Summer.mean,VPDmax_Fall.mean,
                         RH_Spring.mean,RH_Summer.mean,RH_Fall.mean)

Tmin_Spring.cmean <- Tmin_Spring %>% current.mean( label ="Tmin_Spring")
Tmin_Summer.cmean <- Tmin_Summer %>% current.mean( label ="Tmin_Summer")
Tmin_Fall.cmean <- Tmin_Fall %>% current.mean( label ="Tmin_Fall")

Tmax_Spring.cmean <- Tmax_Spring %>% current.mean( label ="Tmax_Spring")
Tmax_Summer.cmean <- Tmax_Summer %>% current.mean( label ="Tmax_Summer")
Tmax_Fall.cmean <- Tmax_Fall %>% current.mean( label ="Tmax_Fall")

Tmax_Spring.cmean <- Tmax_Spring %>% current.mean( label ="Tmax_Spring")
Tmax_Summer.cmean <- Tmax_Summer %>% current.mean( label ="Tmax_Summer")
Tmax_Fall.cmean <- Tmax_Fall %>% current.mean( label ="Tmax_Fall")

PRCP_Spring.cmean <- PRCP_Spring %>% current.mean( label ="PRCP_Spring")
PRCP_Summer.cmean <- PRCP_Summer %>% current.mean( label ="PRCP_Summer")
PRCP_Fall.cmean <- PRCP_Fall %>% current.mean( label ="PRCP_Fall")

VPD_Spring.cmean <- VPD_Spring %>% current.mean( label ="VPD_Spring")
VPD_Summer.cmean <- VPD_Summer %>% current.mean( label ="VPD_Summer")
VPD_Fall.cmean <- VPD_Fall %>% current.mean( label ="VPD_Fall")

VPDmax_Spring.cmean <- VPDMAX_Spring %>% current.mean( label ="VPDmax_Spring")
VPDmax_Summer.cmean <- VPDMAX_Summer %>% current.mean( label ="VPDmax_Summer")
VPDmax_Fall.cmean <- VPDMAX_Fall %>% current.mean( label ="VPDmax_Fall")

RH_Spring.cmean <- RH_Spring %>% current.mean( label ="RH_Spring")
RH_Summer.cmean <- RH_Summer %>% current.mean( label ="RH_Summer")
RH_Fall.cmean <- RH_Fall %>% current.mean( label ="RH_Fall")

current.mean.climate <- c(Tmin_Spring.cmean,Tmin_Summer.cmean,Tmin_Fall.cmean,
                         Tmax_Spring.cmean,Tmax_Summer.cmean,Tmax_Fall.cmean,
                         Tmax_Spring.cmean,Tmax_Summer.cmean,Tmax_Fall.cmean,
                         PRCP_Spring.cmean,PRCP_Summer.cmean,PRCP_Fall.cmean,
                         VPD_Spring.cmean,VPD_Summer.cmean,VPD_Fall.cmean,
                         VPDmax_Spring.cmean,VPDmax_Summer.cmean,VPDmax_Fall.cmean,
                         RH_Spring.cmean,RH_Summer.cmean,RH_Fall.cmean)

data.5n.sf <- st_transform(data.5n.sf, '+proj=lcc +lat_0=42.5 +lon_0=-100 +lat_1=25 +lat_2=60 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs')
data.5n.sf.future <- raster::extract(future.mean.climate, data.5n.sf)
data.5n.sf.current <- raster::extract(current.mean.climate, data.5n.sf)

ggplot() + geom_density(aes(data.5n.sf.current$Tmin_Spring)) +
  geom_density(aes(data.5n.sf.future$Tmin_Spring), col="red")

ggplot() + geom_density(aes(data.5n.sf.current$Tmin_Summer)) +
  geom_density(aes(data.5n.sf.future$Tmin_Summer), col="red")

ggplot() + geom_density(aes(data.5n.sf.current$Tmin_Fall)) +
  geom_density(aes(data.5n.sf.future$Tmin_Fall), col="red")

ggplot() + geom_density(aes(data.5n.sf.current$Tmax_Spring)) +
  geom_density(aes(data.5n.sf.future$Tmax_Spring), col="red")

ggplot() + geom_density(aes(data.5n.sf.current$Tmax_Summer)) +
  geom_density(aes(data.5n.sf.future$Tmax_Summer), col="red")

ggplot() + geom_density(aes(data.5n.sf.current$Tmax_Fall)) +
  geom_density(aes(data.5n.sf.future$Tmax_Fall), col="red")

ggplot() + geom_density(aes(data.5n.sf.current$Tmax_Spring)) +
  geom_density(aes(data.5n.sf.future$Tmin_Spring), col="red")

ggplot() + geom_density(aes(data.5n.sf.current$Tmax_Summer)) +
  geom_density(aes(data.5n.sf.future$Tmin_Summer), col="red")

ggplot() + geom_density(aes(data.5n.sf.current$Tmax_Fall)) +
  geom_density(aes(data.5n.sf.future$Tmin_Fall), col="red")

ggplot() + geom_density(aes(data.5n.sf.current$PRCP_Spring)) +
  geom_density(aes(data.5n.sf.future$PRCP_Spring), col="red")

ggplot() + geom_density(aes(data.5n.sf.current$PRCP_Summer)) +
  geom_density(aes(data.5n.sf.future$PRCP_Summer), col="red")

ggplot() + geom_density(aes(data.5n.sf.current$PRCP_Fall)) +
  geom_density(aes(data.5n.sf.future$PRCP_Fall), col="red")

ggplot() + geom_density(aes(data.5n.sf.current$VPD_Spring)) +
  geom_density(aes(data.5n.sf.future$VPD_Spring), col="red")

ggplot() + geom_density(aes(data.5n.sf.current$VPD_Summer)) +
  geom_density(aes(data.5n.sf.future$VPD_Summer), col="red")

ggplot() + geom_density(aes(data.5n.sf.current$VPD_Fall)) +
  geom_density(aes(data.5n.sf.future$VPD_Fall), col="red")

ggplot() + geom_density(aes(data.5n.sf.current$RH_Spring)) +
  geom_density(aes(data.5n.sf.future$RH_Spring), col="red")

ggplot() + geom_density(aes(data.5n.sf.current$RH_Summer)) +
  geom_density(aes(data.5n.sf.future$RH_Summer), col="red")

ggplot() + geom_density(aes(data.5n.sf.current$RH_Fall)) +
  geom_density(aes(data.5n.sf.future$RH_Fall), col="red")

