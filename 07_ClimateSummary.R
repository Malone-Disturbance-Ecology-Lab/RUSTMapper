# Explore Trends in Climate- Produce Summary Tables:

rm(list=ls())

library(terra )
library(sf)
library(tidyverse)
library(AOI)

# Data: ####
AOI = aoi_get(state = c("CO", "WA", "OR", "CA", "MT", "ID", "UT", "AZ", "NV", "WY", "NM", "TX"))

source( '/Users/sm3466/Dropbox (YSE)/Research/RUSTMapper/01_PrepareClimateLayersForUSE.R')

data.dir <- "/Volumes/MaloneLab/Research/RUSTMAPPER"
figure.dir <- "/Users/sm3466/Dropbox (YSE)/Research/RUSTMapper/FIGURES"
setwd(data.dir)

ensemble.est <- rast(  paste(data.dir,"/Ensemble_1980-2099_EST.tif", sep=""))
ensemble.inv <- rast(paste(data.dir,"/Ensemble_1980-2099_INV.tif",sep=""))

load("Final_ShapeFiles.RDATA")
load('CCTRENDS_Regions_Species.RDATA')



# FUNCTIONS : ####
layer.extract <- function( sf, raster){
  test <- terra::extract( raster, sf , ID=F)  %>% as.data.frame
  names(test ) <- time(raster)
  test.2 <- test %>%  colMeans %>% as.data.frame 
  return(test.2)
}

location.mean <- function(sf, region, layers){
  
  Climate.Trends <-data.frame(sf=region )
  
  Climate.Trends$PRCP.Spring <- layer.extract( sf=sf, raster = PRCP_Spring[[layers]])[,1] %>% mean
  Climate.Trends$PRCP.Fall <- layer.extract( sf=sf, raster = PRCP_Fall[[layers]])[,1] %>% mean
  Climate.Trends$PRCP.Summer <- layer.extract( sf=sf, raster = PRCP_Summer[[layers]])[,1] %>% mean
  
  Climate.Trends$Tmax.Spring <- layer.extract( sf=sf, raster = Tmax_Spring[[layers]])[,1] %>% max
  Climate.Trends$Tmax.Fall <- layer.extract( sf=sf, raster = Tmax_Fall[[layers]])[,1] %>% max
  Climate.Trends$Tmax.Summer <- layer.extract( sf=sf, raster = Tmax_Summer[[layers]])[,1] %>% max
  
  Climate.Trends$Tmin.Spring <- layer.extract( sf=sf, raster = Tmin_Spring[[layers]])[,1] %>% min
  Climate.Trends$Tmin.Fall <- layer.extract( sf=sf, raster = Tmin_Fall[[layers]])[,1] %>% min
  Climate.Trends$Tmin.Summer <- layer.extract( sf=sf, raster = Tmin_Summer[[layers]])[,1] %>% min
  
  Climate.Trends$Tmean.Spring <- layer.extract( sf=sf, raster = Tmean_Spring[[layers]])[,1] %>% mean
  Climate.Trends$Tmean.Fall <- layer.extract( sf=sf, raster = Tmean_Fall[[layers]])[,1] %>% mean
  Climate.Trends$Tmean.Summer <- layer.extract( sf=sf, raster = Tmean_Summer[[layers]])[,1] %>% mean
  
  Climate.Trends$VPD.Spring <- layer.extract( sf=sf, raster = VPD_Spring[[layers]])[,1] %>% mean
  Climate.Trends$VPD.Fall <- layer.extract( sf=sf, raster = VPD_Fall[[layers]])[,1] %>% mean
  Climate.Trends$VPD.Summer <- layer.extract( sf=sf, raster = VPD_Summer[[layers]])[,1] %>% mean
  
  Climate.Trends$VPDmax.Spring <- layer.extract( sf=sf, raster = VPDmax_Spring[[layers]])[,1] %>% max
  Climate.Trends$VPDmax.Fall <- layer.extract( sf=sf, raster = VPDmax_Fall[[layers]])[,1] %>% max
  Climate.Trends$VPDmax.Summer <- layer.extract( sf=sf, raster = VPDmax_Summer[[layers]])[,1] %>% max
  
  Climate.Trends$RH.Spring <- layer.extract( sf=sf, raster = RH_Spring[[layers]])[,1] %>% mean
  Climate.Trends$RH.Fall <- layer.extract( sf=sf, raster = RH_Fall[[layers]])[,1] %>% mean
  Climate.Trends$RH.Summer <- layer.extract( sf=sf, raster = RH_Summer[[layers]])[,1] %>% mean
  # FINSH ADDING THE OTHER CLIMATE LAYERS!!!!!!
  return( Climate.Trends)
}

# Make a regional table!!!!
Table.Summary <- function( layers, layer.extract,location.mean ){
  
  
  t2.cce <- location.mean(sf = wp.cce, region= "CCE", layers = layers) 
  t2.gb <- location.mean(sf = wp.gb, region= "GB", layers = layers)
  t2.gye <- location.mean(sf = wp.gye, region= "GYE", layers = layers)
  t2.pnw <- location.mean(sf = wp.pnw, region= "PNW", layers = layers)
  t2.sr <- location.mean(sf = wp.sr, region= "SR", layers = layers)
  t2.ssn <- location.mean(sf = wp.ssn, region= "SSN", layers = layers)
  t2.sw <- location.mean(sf = wp.sw, region= "SW", layers = layers)
  
  table2 <- rbind(t2.cce, t2.gb, t2.gye, t2.pnw, t2.sr, t2.ssn, t2.sw) %>% t %>% as.data.frame 
  return( table2)
}

Current <- Table.Summary(layers = 1:44, layer.extract = layer.extract,location.mean =location.mean)

# Write Out the table to the Figures file
write.csv(Current  , paste(data.dir,"/08_Table_Current-Climate.csv", sep=""))

# Summary for the future

Table.Summary2 <- function( layers, layer.extract,location.mean ){
  location.mean <- function(sf, region, layers){
    
    Climate.Trends <-data.frame(sf=region )
    
    Climate.Trends$PRCP.Spring <- layer.extract( sf=sf, raster = PRCP_Spring[[layers]])[,1] %>% mean
    Climate.Trends$PRCP.Fall <- layer.extract( sf=sf, raster = PRCP_Fall[[layers]])[,1] %>% mean
    Climate.Trends$PRCP.Summer <- layer.extract( sf=sf, raster = PRCP_Summer[[layers]])[,1] %>% mean
    
    Climate.Trends$Tmax.Spring <- layer.extract( sf=sf, raster = Tmax_Spring[[layers]])[,1] %>% max
    Climate.Trends$Tmax.Fall <- layer.extract( sf=sf, raster = Tmax_Fall[[layers]])[,1] %>% max
    Climate.Trends$Tmax.Summer <- layer.extract( sf=sf, raster = Tmax_Summer[[layers]])[,1] %>% max
    
    Climate.Trends$Tmin.Spring <- layer.extract( sf=sf, raster = Tmin_Spring[[layers]])[,1] %>% mean
    Climate.Trends$Tmin.Fall <- layer.extract( sf=sf, raster = Tmin_Fall[[layers]])[,1] %>% mean
    Climate.Trends$Tmin.Summer <- layer.extract( sf=sf, raster = Tmin_Summer[[layers]])[,1] %>% mean
    
    Climate.Trends$Tmean.Spring <- layer.extract( sf=sf, raster = Tmean_Spring[[layers]])[,1] %>% mean
    Climate.Trends$Tmean.Fall <- layer.extract( sf=sf, raster = Tmean_Fall[[layers]])[,1] %>% mean
    Climate.Trends$Tmean.Summer <- layer.extract( sf=sf, raster = Tmean_Summer[[layers]])[,1] %>% mean
    
    Climate.Trends$VPD.Spring <- layer.extract( sf=sf, raster = VPD_Spring[[layers]])[,1] %>% mean
    Climate.Trends$VPD.Fall <- layer.extract( sf=sf, raster = VPD_Fall[[layers]])[,1] %>% mean
    Climate.Trends$VPD.Summer <- layer.extract( sf=sf, raster = VPD_Summer[[layers]])[,1] %>% mean
    
    Climate.Trends$VPDmax.Spring <- layer.extract( sf=sf, raster = VPDmax_Spring[[layers]])[,1] %>% mean
    Climate.Trends$VPDmax.Fall <- layer.extract( sf=sf, raster = VPDmax_Fall[[layers]])[,1] %>% mean
    Climate.Trends$VPDmax.Summer <- layer.extract( sf=sf, raster = VPDmax_Summer[[layers]])[,1] %>% mean
    
    Climate.Trends$RH.Spring <- layer.extract( sf=sf, raster = RH_Spring[[layers]])[,1] %>% mean
    Climate.Trends$RH.Fall <- layer.extract( sf=sf, raster = RH_Fall[[layers]])[,1] %>% mean
    Climate.Trends$RH.Summer <- layer.extract( sf=sf, raster = RH_Summer[[layers]])[,1] %>% mean
    # FINSH ADDING THE OTHER CLIMATE LAYERS!!!!!!
    return( Climate.Trends)
  }
  
  t2.cce <- location.mean(sf = wp.cce, region= "CCE", layers = layers) 
  t2.gb <- location.mean(sf = wp.gb, region= "GB", layers = layers)
  t2.gye <- location.mean(sf = wp.gye, region= "GYE", layers = layers)
  t2.pnw <- location.mean(sf = wp.pnw, region= "PNW", layers = layers)
  t2.sr <- location.mean(sf = wp.sr, region= "SR", layers = layers)
  t2.ssn <- location.mean(sf = wp.ssn, region= "SSN", layers = layers)
  t2.sw <- location.mean(sf = wp.sw, region= "SW", layers = layers)
  
  table2 <- rbind(t2.cce, t2.gb, t2.gye, t2.pnw, t2.sr, t2.ssn, t2.sw) %>% t %>% as.data.frame 
  return( table2)
}
Future <- Table.Summary2(layers = 45:114, layer.extract = layer.extract,location.mean =location.mean)

# Time series of the different layers to grapgh after scaling...
write.csv(Future, paste(data.dir,"/08_Table_Future-Climate.csv", sep=""))

# Delta:
location.mean.Delta <- function(sf, region, layer1, layer2){
  
  percentChanges <- function(raster){
    rasterPC <- ((raster[[layer1]] %>% mean - raster[[layer2]] %>% mean) / raster[[layer1]] %>% mean) *100 *-1
    return(rasterPC)
  }
  
  Climate.Trends <-data.frame(sf=region )
  
  Climate.Trends$PRCP.Spring <- layer.extract( sf=sf, raster =  percentChanges(PRCP_Spring) )[,1] %>% round(0)
  Climate.Trends$PRCP.Fall <- layer.extract( sf=sf, raster = percentChanges(PRCP_Fall))[,1] %>% round(0)
  Climate.Trends$PRCP.Summer <- layer.extract( sf=sf, raster = percentChanges(PRCP_Summer) )[,1] %>% round(0)
  
  Climate.Trends$Tmax.Spring <- layer.extract( sf=sf, raster = percentChanges(Tmax_Spring) )[,1]%>% round(0)
  Climate.Trends$Tmax.Fall <- layer.extract( sf=sf, raster = percentChanges(Tmax_Fall ))[,1]%>% round(0)
  Climate.Trends$Tmax.Summer <- layer.extract( sf=sf, raster = percentChanges(Tmax_Summer))[,1]%>% round(0)
  
  Climate.Trends$Tmin.Spring <- layer.extract( sf=sf, raster = percentChanges(Tmin_Spring) )[,1]%>% round(0)
  Climate.Trends$Tmin.Fall <- layer.extract( sf=sf, raster = percentChanges(Tmin_Fall) )[,1] %>% round(0)
  Climate.Trends$Tmin.Summer <- layer.extract( sf=sf, raster = percentChanges(Tmin_Summer) )[,1]%>% round(0)
  
  Climate.Trends$Tmean.Spring <- layer.extract( sf=sf, raster = percentChanges(Tmean_Spring) )[,1] %>% round(0)
  Climate.Trends$Tmean.Fall <- layer.extract( sf=sf, raster = percentChanges(Tmean_Fall) )[,1]%>% round(0)
  Climate.Trends$Tmean.Summer <- layer.extract( sf=sf, raster = percentChanges(Tmean_Summer) )[,1] %>% round(0)
  
  Climate.Trends$VPD.Spring <- layer.extract( sf=sf, raster = percentChanges(VPD_Spring ))[,1] %>% round(0)
  Climate.Trends$VPD.Fall <- layer.extract( sf=sf, raster = percentChanges(VPD_Fall ))[,1]%>% round(0)
  Climate.Trends$VPD.Summer <- layer.extract( sf=sf, raster = percentChanges(VPD_Summer ))[,1] %>% round(0)
  
  Climate.Trends$VPDmax.Spring <- layer.extract( sf=sf, raster = percentChanges(VPDmax_Spring) )[,1]%>% round(0)
  Climate.Trends$VPDmax.Fall <- layer.extract( sf=sf, raster =percentChanges( VPDmax_Fall ))[,1] %>% round(0)
  Climate.Trends$VPDmax.Summer <- layer.extract( sf=sf, raster = percentChanges(VPDmax_Summer) )[,1]%>% round(0)
  
  Climate.Trends$RH.Spring <- layer.extract( sf=sf, raster = percentChanges(RH_Spring ))[,1]%>% round(0)
  Climate.Trends$RH.Fall <- layer.extract( sf=sf, raster = percentChanges(RH_Fall ))[,1]%>% round(0)
  Climate.Trends$RH.Summer <- layer.extract( sf=sf, raster = percentChanges(RH_Summer) )[,1] %>% round(0)
  # FINSH ADDING THE OTHER CLIMATE LAYERS!!!!!!
  return( Climate.Trends)
}

t2.cce.d <- location.mean.Delta(sf = wp.cce, region= "CCE", layer1= 1:44, layer2= 45:114) 
t2.gb.d <- location.mean.Delta(sf = wp.gb, region= "GB", layer1= 1:44, layer2= 45:114)
t2.gye.d <- location.mean.Delta(sf = wp.gye, region= "GYE", layer1= 1:44, layer2= 45:114)
t2.pnw.d <- location.mean.Delta(sf = wp.pnw, region= "PNW", layer1= 1:44, layer2= 45:114)
t2.sr.d <- location.mean.Delta(sf = wp.sr, region= "SR", layer1= 1:44, layer2= 45:114)
t2.ssn.d <- location.mean.Delta(sf = wp.ssn, region= "SSN", layer1= 1:44, layer2= 45:114)
t2.sw.d <- location.mean.Delta(sf = wp.sw, region= "SW", layer1= 1:44, layer2= 45:114)

tableS.Delta <- rbind(t2.cce.d, t2.gb.d, t2.gye.d, t2.pnw.d, t2.sr.d, t2.ssn.d, t2.sw.d) %>% t %>% as.data.frame 

write.csv(tableS.Delta  , paste(data.dir, "/Table5_RegionalDelta.csv", sep =""))


