# STEP7 CHANGE over time:

rm(list=ls())

#  https://pjbartlein.github.io/REarthSysSci/rasterVis01.html
library(viridis)
library(AOI)
library(sf)
library(RColorBrewer)
library(terra)

library(ggplot2)
library(tidyterra)
library(egg) # adds tags to facets
library(gridExtra)
library("cowplot")
library(ggpubr)

# Load data:
data.dir <- "/Volumes/MaloneLab/Research/RUSTMAPPER"
setwd(data.dir)

ensemble.est <- rast( paste(data.dir,"/Ensemble_1980-2099_EST.tif", sep=""))
ensemble.inv <- rast( paste(data.dir,"/Ensemble_1980-2099_INV.tif", sep=""))

load("Final_ShapeFiles.RDATA")

source("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/RUSTMapper/07_Functions_regional_summary_TABLES_ALL.R" )

# Southern Rockies
library(zoo)
regional.trends.df <- function(raster.inv, raster.est,  shp){
  # Crop and mask stacks:
  raster.inv.c <- terra::crop(raster.inv, shp) %>% terra::mask(shp)
  raster.est.c <- terra::crop(raster.est, shp) %>% terra::mask(shp)
  
  inv.df <- as.data.frame(global( raster.inv.c, 'mean', na.rm=T)) %>% dplyr::mutate( inv=mean, year=time( raster.inv.c)) %>% dplyr::select(inv, year)
  
  est.df <- as.data.frame(global( raster.est.c, 'mean', na.rm=T)) %>% dplyr::mutate( est=mean, year=time( raster.est.c)) %>% dplyr::select(est, year)
  
  final.df <- inv.df %>% full_join( est.df, by="year")
  
  return(final.df )}

cc.trends.wp <- regional.trends.df( ensemble.inv, ensemble.est, wp %>% st_as_sf) %>% rename( wp.inv=inv, wp.est=est)
cc.trends.sr <- regional.trends.df( ensemble.inv, ensemble.est, wp.sr %>% st_as_sf)%>% rename( sr.inv=inv, sr.est=est)
cc.trends.gb <- regional.trends.df( ensemble.inv, ensemble.est, wp.gb %>% st_as_sf)%>% rename( gb.inv=inv, gb.est=est)
cc.trends.ssn <- regional.trends.df( ensemble.inv, ensemble.est, wp.ssn %>% st_as_sf)%>% rename( ssn.inv=inv, ssn.est=est)
cc.trends.sw <- regional.trends.df( ensemble.inv, ensemble.est, wp.sw %>% st_as_sf)%>% rename( sw.inv=inv, sw.est=est)
cc.trends.gye <- regional.trends.df( ensemble.inv, ensemble.est, wp.gye %>% st_as_sf)%>% rename( gye.inv=inv, gye.est=est)
cc.trends.cce <- regional.trends.df( ensemble.inv, ensemble.est, wp.cce %>% st_as_sf)%>% rename( cce.inv=inv, cce.est=est)
cc.trends.pnw <- regional.trends.df( ensemble.inv, ensemble.est, wp.pnw %>% st_as_sf)%>% rename( pnw.inv=inv, pnw.est=est)

cc.trends.regional <- cc.trends.wp %>% 
  full_join( cc.trends.sr, by="year")%>% 
  full_join( cc.trends.gb, by="year")%>% 
  full_join( cc.trends.sw, by="year") %>% 
  full_join( cc.trends.ssn, by="year")%>% 
  full_join( cc.trends.gye, by="year")%>% 
  full_join( cc.trends.cce, by="year")%>% 
  full_join( cc.trends.pnw, by="year")

rm(cc.trends.sr,  cc.trends.gb , cc.trends.ssn, cc.trends.sw,
   cc.trends.gye,cc.trends.cce, cc.trends.pnw, cc.trends.wp )

### Trends by species: PIBA, PIAL, PIFL, PILO,PIST, PIAR

trends.PIBA <- regional.trends.df(ensemble.inv, ensemble.est, PIBA)[,1:3] %>% rename(PIBA.inv = inv, PIBA.est=est)
trends.PIAL <- regional.trends.df(ensemble.inv, ensemble.est, PIAL)[,1:3] %>% rename(PIAL.inv = inv, PIAL.est=est)
trends.PIFL <- regional.trends.df(ensemble.inv, ensemble.est, PIFL)[,1:3] %>% rename(PIFL.inv = inv, PIFL.est=est)
trends.PILO <- regional.trends.df(ensemble.inv, ensemble.est, PILO)[,1:3] %>% rename(PILO.inv = inv, PILO.est=est)
trends.PIST <- regional.trends.df(ensemble.inv, ensemble.est, PIST)[,1:3] %>% rename(PIST.inv = inv, PIST.est=est)
trends.PIAR <- regional.trends.df(ensemble.inv, ensemble.est, PIAR)[,1:3] %>% rename(PIAR.inv = inv, PIAR.est=est)

cc.trends.species <- trends.PIBA %>% full_join(trends.PIAL, by='year' )%>%
  full_join(trends.PIFL, by='year' )%>%
  full_join(trends.PILO, by='year' )%>%
  full_join(trends.PIST, by='year' )%>%
  full_join(trends.PIAR, by='year' )

rm(trends.PIAL, trends.PIFL,trends.PIBA, trends.PILO,
   trends.PIST, trends.PIAR)


# Export Files: ####
save( cc.trends.regional, cc.trends.species, 
      file= paste(data.dir,'/CCTRENDS_Regions_Species.RDATA', sep=""))


# EOF