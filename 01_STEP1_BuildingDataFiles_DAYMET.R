rm(list=ls())

# build new data files and download daymet data:
# Import and build analysis file from raw csv files provided by PIs
library(dplyr)
library(gtools)
library(rgdal)
library(sf)
library(AOI)
library(terra)
library(tidyverse)

data.dir <- "/Volumes/MaloneLab/Research/RUSTMAPPER"
setwd(data.dir)
load( "Final_ShapeFiles.RDATA")

AOI = aoi_get(state = c("CO", "WA", "OR", "CA", "MT", "ID", "UT", "AZ", "NV", "WY", "NM", "TX"))

# Base file Development from PI Files: ####

# Import WPBR data files:
projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
wpbr <- read.csv("WWETAC_040423_CorrectedOriginal withNewData_040425.csv") %>%
  mutate( LAT = POINT_X, LON = POINT_Y) %>% st_as_sf(coords = c("POINT_X", "POINT_Y"), crs = projcrs)

ggplot() + geom_sf(data= AOI) + geom_sf(data =wpbr )

# Extract STREAM Density:

stream <- rast('StreamDensity_WestUSA_1km.tif')
streamDen <-stream$Density
streamDen[is.na(streamDen)] <- 0

wpbr <- st_transform(wpbr, crs(stream)) # must change proj for elev tool
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

elev <- rast('DEM_2021.tif')
tpi <- rast('tpi_2021.tif')
tri <-rast('tri_2021.tif')

wpbr.sf$ID <- seq.int(nrow(wpbr.sf))
wpbr.sf$TPI <- terra::extract( tpi, wpbr.sf, method='simple',df=TRUE)[,2]
wpbr.sf$TRI <- terra::extract( tri, wpbr.sf, method='simple',df=TRUE)[,2]

wpbr.sf <- wpbr.sf %>% st_transform(crs(wp.gb))%>% distinct(.keep_all = TRUE) 

save(wpbr.sf, file='WPBR_plots2023.RDATA')

CALC_H_ZONE <- function(Tmin){
  H_Zone <- read.csv( 'HardinessZones.csv')
  Tmin <- Tmin %>% round(1)
  H_Zone.calc <- NA
  H_Zone.calc[ Tmin <= H_Zone$Temp_upper[H_Zone$H_Zone == 1] ] <- 1
  H_Zone.calc[ Tmin <= H_Zone$Temp_upper[H_Zone$H_Zone == 2] & Tmin >= H_Zone$Temp_Lower[H_Zone$H_Zone == 2]] <- 2
  H_Zone.calc[ Tmin <= H_Zone$Temp_upper[H_Zone$H_Zone == 3] & Tmin >= H_Zone$Temp_Lower[H_Zone$H_Zone == 3]] <- 3
  H_Zone.calc[ Tmin <= H_Zone$Temp_upper[H_Zone$H_Zone == 4] & Tmin >= H_Zone$Temp_Lower[H_Zone$H_Zone == 4]] <- 4
  H_Zone.calc[ Tmin <= H_Zone$Temp_upper[H_Zone$H_Zone == 5] & Tmin >= H_Zone$Temp_Lower[H_Zone$H_Zone == 5]] <- 5
  H_Zone.calc[ Tmin <= H_Zone$Temp_upper[H_Zone$H_Zone == 6] & Tmin >= H_Zone$Temp_Lower[H_Zone$H_Zone == 6]] <- 6
  H_Zone.calc[ Tmin <= H_Zone$Temp_upper[H_Zone$H_Zone == 7] & Tmin >= H_Zone$Temp_Lower[H_Zone$H_Zone == 7]] <- 7
  H_Zone.calc[ Tmin <= H_Zone$Temp_upper[H_Zone$H_Zone == 8] & Tmin >= H_Zone$Temp_Lower[H_Zone$H_Zone == 8]] <- 8
  H_Zone.calc[ Tmin <= H_Zone$Temp_upper[H_Zone$H_Zone == 9] & Tmin >= H_Zone$Temp_Lower[H_Zone$H_Zone == 9]] <- 9
  H_Zone.calc[ Tmin <= H_Zone$Temp_upper[H_Zone$H_Zone == 10] & Tmin >= H_Zone$Temp_Lower[H_Zone$H_Zone == 10]] <- 10
  H_Zone.calc[ Tmin <= H_Zone$Temp_upper[H_Zone$H_Zone == 11] & Tmin >= H_Zone$Temp_Lower[H_Zone$H_Zone == 11]] <- 11
  H_Zone.calc[ Tmin <= H_Zone$Temp_upper[H_Zone$H_Zone == 12] & Tmin >= H_Zone$Temp_Lower[H_Zone$H_Zone == 12]] <- 12
  H_Zone.calc[  Tmin >= H_Zone$Temp_Lower[H_Zone$H_Zone == 13] ] <- 13
  return(H_Zone.calc )
}
#########

load( 'WPBR_plots2023.RDATA')
AOI = aoi_get(state = c("CO", "WA", "OR", "CA", "MT", "ID", "UT", "AZ", "NV", "WY", "NM", "TX"))
summary(wpbr.sf$YEAR_LAST)
data <- wpbr.sf  %>% st_transform( st_crs(AOI))
data$YEAR_LAST <- as.numeric(data$YEAR_LAST)
data <- data %>% filter(YEAR_LAST > 0, YEAR_LAST < 2025 )

# Create a DF to store the data:
subsample <- data %>% distinct( ) %>% as.data.frame(xy=T)

# some points are lost because they have the same latlong
subsample %>% names()
subsample$POINT_Y <- subsample$LON
subsample$POINT_X <- subsample$LAT

########## Climate Date ##########
#________________________________________________________________-________________________________
#________________________________________________________________-________________________________
#________________________________________________________________-________________________________
library(daymetr)
library(lubridate)
library(epitools)
library(dplyr)

# 5 Year Climate summaries
subsample <- subsample %>% filter (YEAR_LAST > 1985 ) # Daymet starts in 1980
subsample.temp <- subsample
subsample %>% names()

for(i in 6170: length( subsample$YEAR_LAST) ){
  print(i)
  data1 <- download_daymet(site = "SparkleSparkle",
                           lat =  subsample$POINT_Y[i],
                           lon = subsample$POINT_X[i],
                           start = subsample$YEAR_LAST[i] - 4 ,
                           end = subsample$YEAR_LAST[i],
                           internal = TRUE)
  
  df <- as.data.frame(data1$data)
  
  df$date <- paste( df$year, df$yday, sep="-")
  df$date <- as.Date(df$date, format="%Y-%j")
  df$month <- as.numeric(format(df$date, format="%m"))
  df$yday <- as.numeric(format(df$date, format="%j"))
  
  df <- df %>% mutate(
    tmean = (0.606 * tmax..deg.c.) + 0.394 *tmin..deg.c.,
    Svp = 610.8* exp((17.27 *tmean) / (237.3 + tmean)),
    RH = (vp..Pa./Svp)*100)
  
  # Make the seasonal DF
  df.Spring <-df %>% filter(month >= 4,
                             month <= 5 ) %>% reframe(
                               PRCP_Spring = sum(prcp..mm.day.)/5,
                               Tmax_Spring = max(tmax..deg.c.) ,
                               Tmin_Spring = min(tmin..deg.c.) ,
                               VPD_Spring = mean(vp..Pa.),
                               VPDmax_Spring = max(vp..Pa.),
                               RH_Spring = mean(RH))
  
  df.Summer <-df %>% filter (month >= 6,
                             month <= 8 ) %>% reframe(
                               PRCP_Summer = sum(prcp..mm.day.)/5,
                               Tmax_Summer = max(tmax..deg.c.) ,
                               Tmin_Summer = min(tmin..deg.c.) ,
                               VPD_Summer = mean(vp..Pa.),
                               VPDmax_Summer = max(vp..Pa.),
                               RH_Summer = mean(RH))
  
  df.Fall <-df %>% filter (month >= 9,
                             month <= 10 ) %>% reframe(
                               PRCP_Fall = sum(prcp..mm.day.)/5,
                               Tmax_Fall = max(tmax..deg.c.) ,
                               Tmin_Fall = min(tmin..deg.c.) ,
                               VPD_Fall = mean(vp..Pa.),
                               VPDmax_Fall = max(vp..Pa.),
                               RH_Fall = mean(RH))
  
  df.Winter <-df %>% filter (month >= 1,
                           month <= 2 ) %>% reframe(
                             Tmin_Winter = min(tmin..deg.c.))
  
  # Add to then data to the sample file for the location
  subsample$PRCP_Spring[i] <- df.Spring$PRCP_Spring
  subsample$Tmin_Spring[i] <- df.Spring$Tmin_Spring
  subsample$Tmax_Spring[i] <- df.Spring$Tmax_Spring
  subsample$Tmean_Spring[i] <- df.Spring$Tmean_Spring
  subsample$VPD_Spring[i] <- df.Spring$VPD_Spring
  subsample$VPDmax_Spring[i] <- df.Spring$VPDmax_Spring
  subsample$RH_Spring[i] <- df.Spring$RH_Spring
  
  subsample$PRCP_Summer[i] <- df.Summer$PRCP_Summer
  subsample$Tmin_Summer[i] <- df.Summer$Tmin_Summer
  subsample$Tmax_Summer[i] <- df.Summer$Tmax_Summer
  subsample$Tmean_Summer[i] <- df.Summer$Tmean_Summer
  subsample$VPD_Summer[i] <- df.Summer$VPD_Summer
  subsample$VPDmax_Summer[i] <- df.Summer$VPDmax_Summer
  subsample$RH_Summer[i] <- df.Summer$RH_Summer
  
  subsample$PRCP_Fall[i] <- df.Fall$PRCP_Fall
  subsample$Tmin_Fall[i] <- df.Fall$Tmin_Fall
  subsample$Tmax_Fall[i] <- df.Fall$Tmax_Fall
  subsample$Tmean_Fall[i] <- df.Fall$Tmean_Fall
  subsample$VPD_Fall[i] <- df.Fall$VPD_Fall
  subsample$VPDmax_Fall[i] <- df.Fall$VPDmax_Fall
  subsample$RH_Fall[i] <- df.Fall$RH_Fall
  
  subsample$Tmin_Winter[i] <- df.Winter$Tmin_Winter
  rm(data1, df.Spring, df.Summer, df.Fall, df.Winter )
}

# Merge subsample with data...
data.5years <- subsample
data.5years$Tmin_Winter[ is.na(data.5years$H_Zone)]
data.5years$H_Zone <- CALC_H_ZONE(Tmin=data.5years$Tmin_Winter)

#________________________________________________________________-________________________________
#________________________________________________________________-________________________________
#________________________________________________________________-________________________________

# 10 year climate summaries ####
subsample <- subsample.temp %>% filter(YEAR_LAST > 1989 ) # must reduce dataset due to DAYMET Limitations:

for(i in 1: length( subsample$YEAR_LAST) ){
  print(i)
  data1 <- download_daymet(site = "SparkleSparkle",
                           lat =  subsample$POINT_Y[i],
                           lon = subsample$POINT_X[i],
                           start = subsample$YEAR_LAST[i] - 9 ,
                           end = subsample$YEAR_LAST[i],
                           internal = TRUE)
  
  df <- as.data.frame(data1$data)
  
  df$date <- paste( df$year, df$yday, sep="-")
  df$date <- as.Date(df$date, format="%Y-%j")
  df$month <- as.numeric(format(df$date, format="%m"))
  df$yday <- as.numeric(format(df$date, format="%j"))
  
  df <- df %>% mutate(
    tmean = (0.606 * tmax..deg.c.) + 0.394 *tmin..deg.c.,
    Svp = 610.8* exp((17.27 *tmean) / (237.3 + tmean)),
    RH = (vp..Pa./Svp)*100)
  
  # Make the seasonal DF
  df.Spring <-df %>% filter(month >= 4,
                            month <= 5 ) %>% reframe(
                              PRCP_Spring = sum(prcp..mm.day.)/10,
                              Tmax_Spring = max(tmax..deg.c.) ,
                              Tmin_Spring = min(tmin..deg.c.) ,
                              VPD_Spring = mean(vp..Pa.),
                              VPDmax_Spring = max(vp..Pa.),
                              RH_Spring = mean(RH))
  
  df.Summer <-df %>% filter (month >= 6,
                             month <= 8 ) %>% reframe(
                               PRCP_Summer = sum(prcp..mm.day.)/10,
                               Tmax_Summer = max(tmax..deg.c.) ,
                               Tmin_Summer = min(tmin..deg.c.) ,
                               VPD_Summer = mean(vp..Pa.),
                               VPDmax_Summer = max(vp..Pa.),
                               RH_Summer = mean(RH))
  
  df.Fall <-df %>% filter (month >= 9,
                           month <= 10 ) %>% reframe(
                             PRCP_Fall = sum(prcp..mm.day.)/10,
                             Tmax_Fall = max(tmax..deg.c.) ,
                             Tmin_Fall = min(tmin..deg.c.) ,
                             VPD_Fall = mean(vp..Pa.),
                             VPDmax_Fall = max(vp..Pa.),
                             RH_Fall = mean(RH))
  
  df.Winter <-df %>% filter (month >= 1,
                             month <= 2 ) %>% reframe(
                               Tmin_Winter = min(tmin..deg.c.))
  
  # Add to then data to the sample file for the location
  subsample$PRCP_Spring[i] <- df.Spring$PRCP_Spring
  subsample$Tmin_Spring[i] <- df.Spring$Tmin_Spring
  subsample$Tmax_Spring[i] <- df.Spring$Tmax_Spring
  subsample$Tmean_Spring[i] <- df.Spring$Tmean_Spring
  subsample$VPD_Spring[i] <- df.Spring$VPD_Spring
  subsample$VPDmax_Spring[i] <- df.Spring$VPDmax_Spring
  subsample$RH_Spring[i] <- df.Spring$RH_Spring
  
  subsample$PRCP_Summer[i] <- df.Summer$PRCP_Summer
  subsample$Tmin_Summer[i] <- df.Summer$Tmin_Summer
  subsample$Tmax_Summer[i] <- df.Summer$Tmax_Summer
  subsample$Tmean_Summer[i] <- df.Summer$Tmean_Summer
  subsample$VPD_Summer[i] <- df.Summer$VPD_Summer
  subsample$VPDmax_Summer[i] <- df.Summer$VPDmax_Summer
  subsample$RH_Summer[i] <- df.Summer$RH_Summer
  
  subsample$PRCP_Fall[i] <- df.Fall$PRCP_Fall
  subsample$Tmin_Fall[i] <- df.Fall$Tmin_Fall
  subsample$Tmax_Fall[i] <- df.Fall$Tmax_Fall
  subsample$Tmean_Fall[i] <- df.Fall$Tmean_Fall
  subsample$VPD_Fall[i] <- df.Fall$VPD_Fall
  subsample$VPDmax_Fall[i] <- df.Fall$VPDmax_Fall
  subsample$RH_Fall[i] <- df.Fall$RH_Fall
  
  subsample$Tmin_Winter[i] <- df.Winter$Tmin_Winter
  rm(data1, df.Spring, df.Summer, df.Fall, df.Winter )
}

# Merge subsample with data...
data.10years <- subsample
data.10years$H_Zone <- CALC_H_ZONE(Tmin=data.10years$Tmin_Winter)

data.10years$Tmin_Winter[ is.na(data.10years$H_Zone)]

# 20 year climate summaries ####
subsample <- subsample.temp %>% filter(YEAR_LAST > 1999 ) # must reduce dataset due to DAYMET Limitations:

for(i in 1: length( subsample$YEAR_LAST) ){
  print(i)
  data1 <- download_daymet(site = "SparkleSparkle",
                           lat =  subsample$POINT_Y[i],
                           lon = subsample$POINT_X[i],
                           start = subsample$YEAR_LAST[i] - 19 ,
                           end = subsample$YEAR_LAST[i],
                           internal = TRUE)
  
  df <- as.data.frame(data1$data)
  
  df$date <- paste( df$year, df$yday, sep="-")
  df$date <- as.Date(df$date, format="%Y-%j")
  df$month <- as.numeric(format(df$date, format="%m"))
  df$yday <- as.numeric(format(df$date, format="%j"))
  
  df <- df %>% mutate(
    tmean = (0.606 * tmax..deg.c.) + 0.394 *tmin..deg.c.,
    Svp = 620.8* exp((17.27 *tmean) / (237.3 + tmean)),
    RH = (vp..Pa./Svp)*200)
  
  # Make the seasonal DF
  df.Spring <-df %>% filter(month >= 4,
                            month <= 5 ) %>% reframe(
                              PRCP_Spring = sum(prcp..mm.day.)/20,
                              Tmax_Spring = max(tmax..deg.c.) ,
                              Tmin_Spring = min(tmin..deg.c.) ,
                              VPD_Spring = mean(vp..Pa.),
                              VPDmax_Spring = max(vp..Pa.),
                              RH_Spring = mean(RH))
  
  df.Summer <-df %>% filter (month >= 6,
                             month <= 8 ) %>% reframe(
                               PRCP_Summer = sum(prcp..mm.day.)/20,
                               Tmax_Summer = max(tmax..deg.c.) ,
                               Tmin_Summer = min(tmin..deg.c.) ,
                               VPD_Summer = mean(vp..Pa.),
                               VPDmax_Summer = max(vp..Pa.),
                               RH_Summer = mean(RH))
  
  df.Fall <-df %>% filter (month >= 9,
                           month <= 10 ) %>% reframe(
                             PRCP_Fall = sum(prcp..mm.day.)/20,
                             Tmax_Fall = max(tmax..deg.c.) ,
                             Tmin_Fall = min(tmin..deg.c.) ,
                             VPD_Fall = mean(vp..Pa.),
                             VPDmax_Fall = max(vp..Pa.),
                             RH_Fall = mean(RH))
  
  df.Winter <-df %>% filter (month >= 1,
                             month <= 2 ) %>% reframe(
                               Tmin_Winter = min(tmin..deg.c.))
  
  # Add to then data to the sample file for the location
  subsample$PRCP_Spring[i] <- df.Spring$PRCP_Spring
  subsample$Tmin_Spring[i] <- df.Spring$Tmin_Spring
  subsample$Tmax_Spring[i] <- df.Spring$Tmax_Spring
  subsample$Tmean_Spring[i] <- df.Spring$Tmean_Spring
  subsample$VPD_Spring[i] <- df.Spring$VPD_Spring
  subsample$VPDmax_Spring[i] <- df.Spring$VPDmax_Spring
  subsample$RH_Spring[i] <- df.Spring$RH_Spring
  
  subsample$PRCP_Summer[i] <- df.Summer$PRCP_Summer
  subsample$Tmin_Summer[i] <- df.Summer$Tmin_Summer
  subsample$Tmax_Summer[i] <- df.Summer$Tmax_Summer
  subsample$Tmean_Summer[i] <- df.Summer$Tmean_Summer
  subsample$VPD_Summer[i] <- df.Summer$VPD_Summer
  subsample$VPDmax_Summer[i] <- df.Summer$VPDmax_Summer
  subsample$RH_Summer[i] <- df.Summer$RH_Summer
  
  subsample$PRCP_Fall[i] <- df.Fall$PRCP_Fall
  subsample$Tmin_Fall[i] <- df.Fall$Tmin_Fall
  subsample$Tmax_Fall[i] <- df.Fall$Tmax_Fall
  subsample$Tmean_Fall[i] <- df.Fall$Tmean_Fall
  subsample$VPD_Fall[i] <- df.Fall$VPD_Fall
  subsample$VPDmax_Fall[i] <- df.Fall$VPDmax_Fall
  subsample$RH_Fall[i] <- df.Fall$RH_Fall
  
  subsample$Tmin_Winter[i] <- df.Winter$Tmin_Winter
  rm(data1, df.Spring, df.Summer, df.Fall, df.Winter )
}

# Merge subsample with data...
data.20years <- subsample
data.20years$H_Zone <- CALC_H_ZONE(Tmin=data.20years$Tmin_Winter)

data.20years$Tmin_Winter[ is.na(data.20years$H_Zone)]

save(wpbr, data.5years, data.10years, data.20years ,file='WPBR_plots_DAYMET.RDATA')



message(" Next RUN 02_VariableSelection_DAYMET")
