rm(list=ls())

# build new data files and download Daymet data:

# Import and build analysis file from raw csv files provided by PIs
library(dplyr)
library(gtools)
library(rgdal)
library(sf)
library(AOI)
library(terra)

load( "/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/04042023/Final_ShapeFiles.RDATA")

AOI = aoi_get(state = c("CO", "WA", "OR", "CA", "MT", "ID", "UT", "AZ", "NV", "WY", "NM", "TX"))

# Base file Development from PI Files: ####

# Import WPBR data files:

wpbr <- st_read("/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/04042023",  "WWETAC_040423")

# Extract STREAM Density:

stream <- rast('~/Dropbox (YSE)/Research/WPBR/StreamDensity_WestUSA.tif')
streamDen <-stream/91.79378
streamDen[is.na(streamDen)] <- 0

wpbr <- st_transform(wpbr, crs(stream)) # must change proj for elev tool

plot(streamDen)
plot(wpbr$geometry, add=T)
wpbr$streamDen <- terra::extract( streamDen, wpbr)[,2]

# Add an indicator for the greatbasin to this file:
wp.gb <- wp.gb %>% st_as_sf %>%  st_transform( crs(wpbr))
wp.gb$GB <- 1

wpbr.gb <- st_intersection(wpbr, wp.gb)
wpbr.nogb <- st_difference(wpbr, wp.gb)
wpbr.nogb$GB <- 0 
wpbr.sf <- rbind(wpbr.gb, wpbr.nogb) %>% st_as_sf()

# Looks at states:
AOI = st_transform( AOI, st_crs(wpbr.sf))

wpbr$WPBR_stat <- as.factor(wpbr$WPBR_stat)

# get topography information:
wpbr.sf <- st_transform(wpbr.sf, '+proj=longlat +datum=WGS84 +no_defs') # must change proj for elev tool

elev <- rast('/Users/sm3466/Dropbox (YSE)/Research/WPBR/Shapefiles/DEM_2021.tif' )
tpi <- rast('/Users/sm3466/Dropbox (YSE)/Research/WPBR/Shapefiles/tpi_2021.tif')
tri <-rast('/Users/sm3466/Dropbox (YSE)/Research/WPBR/Shapefiles/tri_2021.tif')

wpbr.sf$ID <- seq.int(nrow(wpbr.sf))
wpbr.sf$TPI <- terra::extract( tpi, wpbr.sf, method='simple',df=TRUE)[,2]
wpbr.sf$TRI <- terra::extract( tri, wpbr.sf, method='simple',df=TRUE)[,2]

wpbr.sf <- wpbr.sf %>% st_transform(crs(wp.gb))%>% distinct(.keep_all = TRUE) 

save(wpbr.sf, file='/Users/sm3466/Dropbox (YSE)/Research/WPBR/WPBR_plots2023.RDATA')

H_Zone <- rast("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/WPBR/NewData/Spatial/H_Zone_1980-2099.tif") %>% 
  terra::project( crs(wp.gb))

source("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/WPBR/NewData/Final Scripts/Flow.PrepareClimateLayersForUSE.R")

#########

load( '/Users/sm3466/Dropbox (YSE)/Research/WPBR/WPBR_plots2023.RDATA')
AOI = aoi_get(state = c("CO", "WA", "OR", "CA", "MT", "ID", "UT", "AZ", "NV", "WY", "NM", "TX"))
summary(wpbr.sf$YEAR_LAST)
data <- wpbr.sf  %>% st_transform( st_crs(AOI))
data$YEAR_LAST <- as.numeric(data$YEAR_LAST)
data <- data %>% filter(YEAR_LAST > 0 )

# Create a DF to store the data:
subsample <- data %>% distinct( ) # some points are lost because they have the same latlong

raster.sub <- function(raster, round, period ){
  sub.raster <- min(raster[[ time(raster) <= subsample$YEAR_LAST[[i]]
                             &  time(raster) >= subsample$YEAR_LAST[[i]] - period ]], na.rm=T) 
  
  data <- terra::extract(x= sub.raster, y=subsample[i])[,2] %>% round(0)
  return(data)
}

# 5 Year Climate summaries
for(i in 1: length( subsample$YEAR_LAST) ){
  try(print(i), silent = T)
  try(subsample$H_Zone[i] <- raster.sub(raster= H_Zone, round=0, period=4 ), silent = T)
  try(subsample$Tmin_Spring[i] <- raster.sub(raster= Tmin_Spring, round=2, period=4 ), silent = T)
  try(subsample$Tmin_Summer[i] <- raster.sub(raster= Tmin_Summer, round=2, period=4 ), silent = T)
  try(subsample$Tmin_Fall[i] <- raster.sub(raster= Tmin_Fall, round=2, period=4 ), silent = T)
  try(subsample$Tmax_Spring[i] <- raster.sub(raster= Tmax_Spring, round=2, period=4 ), silent = T)
  try(subsample$Tmax_Summer[i] <- raster.sub(raster= Tmax_Summer, round=2, period=4 ), silent = T)
  try(subsample$Tmax_Fall[i] <- raster.sub(raster= Tmax_Fall, round=2, period=4 ), silent = T)
try(subsample$Tmean_Spring[i] <- raster.sub(raster= Tmean_Spring, round=2, period=4 ), silent = T)
try(subsample$Tmean_Summer[i] <- raster.sub(raster= Tmean_Summer, round=2, period=4 ), silent = T)
try(subsample$Tmean_Fall[i] <- raster.sub(raster= Tmean_Fall, round=2, period=4 ), silent = T)
try(subsample$PRCP_Spring[i] <- raster.sub(raster= PRCP_Spring, round=2, period=4 ), silent = T)
try(subsample$PRCP_Summer[i] <- raster.sub(raster= PRCP_Summer, round=2, period=4 ), silent = T)
try(subsample$PRCP_Fall[i] <- raster.sub(raster= PRCP_Fall, round=2, period=4 ), silent = T)
try(subsample$VPD_Spring[i] <- raster.sub(raster= VPD_Spring, round=2, period=4 ), silent = T)
try(subsample$VPD_Summer[i] <- raster.sub(raster= VPD_Summer, round=2, period=4 ), silent = T)
try(subsample$VPD_Fall[i] <- raster.sub(raster= VPD_Fall, round=2, period=4 ), silent = T)
try(subsample$VPDmax_Spring[i] <- raster.sub(raster= VPDmax_Spring, round=2, period=4 ), silent = T)
try(subsample$VPDmax_Summer[i] <- raster.sub(raster= VPDmax_Summer, round=2, period=4 ), silent = T)
try(subsample$VPDmax_Fall[i] <- raster.sub(raster= VPDmax_Fall, round=2, period=4 ), silent = T)
try(subsample$RH_Spring[i] <- raster.sub(raster= RH_Spring, round=2, period=4 ), silent = T)
try(subsample$RH_Summer[i] <- raster.sub(raster= RH_Summer, round=2, period=4 ), silent = T)
try(subsample$RH_Fall[i] <- raster.sub(raster= RH_Fall, round=2, period=4 ), silent = T)
}

data.5years <- subsample %>%  as.data.frame
#________________________________________________________________-________________________________
#________________________________________________________________-________________________________
#________________________________________________________________-________________________________

# 10 year climate summaries
subsample <- data %>% distinct( )

for(i in 1: length( subsample$YEAR_LAST) ){
  try(print(i), silent = T)
  try(subsample$H_Zone[i] <- raster.sub(raster= H_Zone, round=0, period=9 ), silent = T)
  try(subsample$Tmin_Spring[i] <- raster.sub(raster= Tmin_Spring, round=2, period=9 ), silent = T)
  try(subsample$Tmin_Summer[i] <- raster.sub(raster= Tmin_Summer, round=2, period=9 ), silent = T)
  try(subsample$Tmin_Fall[i] <- raster.sub(raster= Tmin_Fall, round=2, period=9 ), silent = T)
  try(subsample$Tmax_Spring[i] <- raster.sub(raster= Tmax_Spring, round=2, period=9 ), silent = T)
  try(subsample$Tmax_Summer[i] <- raster.sub(raster= Tmax_Summer, round=2, period=9 ), silent = T)
  try(subsample$Tmax_Fall[i] <- raster.sub(raster= Tmax_Fall, round=2, period=9 ), silent = T)
  try(subsample$Tmean_Spring[i] <- raster.sub(raster= Tmean_Spring, round=2, period=9 ), silent = T)
  try(subsample$Tmean_Summer[i] <- raster.sub(raster= Tmean_Summer, round=2, period=9 ), silent = T)
  try(subsample$Tmean_Fall[i] <- raster.sub(raster= Tmean_Fall, round=2, period=9 ), silent = T)
  try(subsample$PRCP_Spring[i] <- raster.sub(raster= PRCP_Spring, round=2, period=9 ), silent = T)
  try(subsample$PRCP_Summer[i] <- raster.sub(raster= PRCP_Summer, round=2, period=9 ), silent = T)
  try(subsample$PRCP_Fall[i] <- raster.sub(raster= PRCP_Fall, round=2, period=9 ), silent = T)
  try(subsample$VPD_Spring[i] <- raster.sub(raster= VPD_Spring, round=2, period=9 ), silent = T)
  try(subsample$VPD_Summer[i] <- raster.sub(raster= VPD_Summer, round=2, period=9 ), silent = T)
  try(subsample$VPD_Fall[i] <- raster.sub(raster= VPD_Fall, round=2, period=9 ), silent = T)
  try(subsample$VPDmax_Spring[i] <- raster.sub(raster= VPDmax_Spring, round=2, period=9 ), silent = T)
  try(subsample$VPDmax_Summer[i] <- raster.sub(raster= VPDmax_Summer, round=2, period=9 ), silent = T)
  try(subsample$VPDmax_Fall[i] <- raster.sub(raster= VPDmax_Fall, round=2, period=9 ), silent = T)
  try(subsample$RH_Spring[i] <- raster.sub(raster= RH_Spring, round=2, period=9 ), silent = T)
  try(subsample$RH_Summer[i] <- raster.sub(raster= RH_Summer, round=2, period=9 ), silent = T)
  try(subsample$RH_Fall[i] <- raster.sub(raster= RH_Fall, round=2, period=9 ), silent = T)
}

data.10years <-subsample  %>%  as.data.frame
#________________________________________________________________-________________________________
#________________________________________________________________-________________________________
#________________________________________________________________-________________________________

# 20 year climate summaries
# Merge subsample with data...

for(i in 1: length( subsample$YEAR_LAST) ){
  try(print(i), silent = T)
  try(subsample$H_Zone[i] <- raster.sub(raster= H_Zone, round=0, period=19 ), silent = T)
  try(subsample$Tmin_Spring[i] <- raster.sub(raster= Tmin_Spring, round=2, period=19 ), silent = T)
  try(subsample$Tmin_Summer[i] <- raster.sub(raster= Tmin_Summer, round=2, period=19 ), silent = T)
  try(subsample$Tmin_Fall[i] <- raster.sub(raster= Tmin_Fall, round=2, period=19 ), silent = T)
  try(subsample$Tmax_Spring[i] <- raster.sub(raster= Tmax_Spring, round=2, period=19 ), silent = T)
  try(subsample$Tmax_Summer[i] <- raster.sub(raster= Tmax_Summer, round=2, period=19 ), silent = T)
  try(subsample$Tmax_Fall[i] <- raster.sub(raster= Tmax_Fall, round=2, period=19 ), silent = T)
  try(subsample$Tmean_Spring[i] <- raster.sub(raster= Tmean_Spring, round=2, period=19 ), silent = T)
  try(subsample$Tmean_Summer[i] <- raster.sub(raster= Tmean_Summer, round=2, period=19 ), silent = T)
  try(subsample$Tmean_Fall[i] <- raster.sub(raster= Tmean_Fall, round=2, period=19 ), silent = T)
  try(subsample$PRCP_Spring[i] <- raster.sub(raster= PRCP_Spring, round=2, period=19 ), silent = T)
  try(subsample$PRCP_Summer[i] <- raster.sub(raster= PRCP_Summer, round=2, period=19 ), silent = T)
  try(subsample$PRCP_Fall[i] <- raster.sub(raster= PRCP_Fall, round=2, period=19 ), silent = T)
  try(subsample$VPD_Spring[i] <- raster.sub(raster= VPD_Spring, round=2, period=19 ), silent = T)
  try(subsample$VPD_Summer[i] <- raster.sub(raster= VPD_Summer, round=2, period=19 ), silent = T)
  try(subsample$VPD_Fall[i] <- raster.sub(raster= VPD_Fall, round=2, period=19 ), silent = T)
  try(subsample$VPDmax_Spring[i] <- raster.sub(raster= VPDmax_Spring, round=2, period=19 ), silent = T)
  try(subsample$VPDmax_Summer[i] <- raster.sub(raster= VPDmax_Summer, round=2, period=19 ), silent = T)
  try(subsample$VPDmax_Fall[i] <- raster.sub(raster= VPDmax_Fall, round=2, period=19 ), silent = T)
  try(subsample$RH_Spring[i] <- raster.sub(raster= RH_Spring, round=2, period=19 ), silent = T)
  try(subsample$RH_Summer[i] <- raster.sub(raster= RH_Summer, round=2, period=19 ), silent = T)
  try(subsample$RH_Fall[i] <- raster.sub(raster= RH_Fall, round=2, period=19 ), silent = T)
}
data.20years <-subsample %>%  as.data.frame

save(data.5years, data.10years, data.20years ,file='/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/Final Scripts/WPBR_plots_GRIDMET.RDATA')

