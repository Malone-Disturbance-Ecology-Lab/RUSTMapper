# Prepare Climate layers for use:

library(tidyverse)
library(sf)
library(AOI)

mydir <- "/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/Spatial/GRIDMET_CURRENT"

delfiles <- dir(path=mydir , pattern="*json")
file.remove(file.path(mydir, delfiles))


# Imports the climate change data:
source( '/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/Final Scripts/00_FUNCTION_ComplieClimteRaters.R')

# Imports current and future climate layers
template <- climate.import.gridmet(pattern = "PRCP_Fall")

# Future years need to be adjusted by 5
PRCP_Fall <- c(climate.import.gridmet(pattern = "PRCP_Fall"), climate.import.future(pattern = "PRCP_Fall") %>% resample(template ))
PRCP_Spring <- c(climate.import.gridmet(pattern = "PRCP_Spring"), climate.import.future(pattern = "PRCP_Spring") %>% resample(template ))
PRCP_Summer <- c(climate.import.gridmet(pattern = "PRCP_Summer"), climate.import.future(pattern = "PRCP_Summer") %>% resample(template ))

PRCP_Fall <- c(PRCP_Fall[[1:44]], PRCP_Fall[[45:114]]/5 )# Need to adjust for the 5 models which are all summed together
PRCP_Spring <- c(PRCP_Spring[[1:44]], PRCP_Spring[[45:114]]/5 ) # Need to adjust for the 5 models which are all summed together
PRCP_Summer <- c(PRCP_Summer[[1:44]], PRCP_Summer[[45:114]]/5 ) # Need to adjust for the 5 models which are all summed together

Tmean_Fall <- c(climate.import.gridmet(pattern = "Tmean_Fall") , climate.import.future(pattern = "Tmean_Fall") %>% resample(template ))
Tmean_Spring <- c(climate.import.gridmet(pattern = "Tmean_Spring") , climate.import.future(pattern = "Tmean_Spring") %>% resample(template ))
Tmean_Summer <- c(climate.import.gridmet(pattern = "Tmean_Summer"), climate.import.future(pattern = "Tmean_Summer") %>% resample(template ))

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

#VPDMAX_Fall[[22:91]] <- VPDMAX_Fall[[22:91]]*1000

RH_Fall <- c(climate.import.gridmet(pattern = "RH.Fall") , climate.import.future(pattern = "RH.Fall") %>% resample(template ))
RH_Spring <- c(climate.import.gridmet(pattern = "RH.Spring") , climate.import.future(pattern = "RH.Spring") %>% resample(template))
RH_Summer <- c(climate.import.gridmet(pattern = "RH.Summer") , climate.import.future(pattern = "RH.Summer") %>% resample(template))

AOI <- AOI %>% st_transform( crs(PRCP_Spring))

PRCP_Spring <- mask(crop(PRCP_Spring, AOI), AOI)
PRCP_Summer <- mask(crop(PRCP_Summer, AOI), AOI)
PRCP_Fall  <- mask(crop(PRCP_Fall, AOI), AOI)

Tmin_Spring  <- mask(crop(Tmin_Spring, AOI), AOI)
Tmin_Summer <- mask(crop(Tmin_Summer, AOI), AOI)
Tmin_Fall  <- mask(crop(Tmin_Fall, AOI), AOI)

Tmax_Spring  <- mask(crop(Tmax_Spring, AOI), AOI)
Tmax_Summer <- mask(crop(Tmax_Summer, AOI), AOI)
Tmax_Fall  <- mask(crop(Tmax_Fall, AOI), AOI)

Tmean_Spring  <- mask(crop(Tmean_Spring, AOI), AOI)
Tmean_Summer <- mask(crop(Tmean_Summer, AOI), AOI)
Tmean_Fall  <- mask(crop(Tmean_Fall, AOI), AOI)

VPD_Spring  <- mask(crop(VPD_Spring, AOI), AOI)
VPD_Summer <- mask(crop(VPD_Summer, AOI), AOI)
VPD_Fall  <- mask(crop(VPD_Fall, AOI), AOI)

VPDmax_Spring  <- mask(crop(VPDMAX_Spring, AOI), AOI)
VPDmax_Summer <- mask(crop(VPDMAX_Summer, AOI), AOI)
VPDmax_Fall  <- mask(crop(VPDMAX_Fall, AOI), AOI)

RH_Spring  <- mask(crop(RH_Spring, AOI), AOI)
RH_Summer <- mask(crop(RH_Summer, AOI), AOI)
RH_Fall  <- mask(crop(RH_Fall, AOI), AOI)
