#REFUGIA

rm(list=ls())

#  https://pjbartlein.github.io/REarthSysSci/rasterVis01.html
library(viridis)
library(AOI)
library(sf)
library(RColorBrewer)
library(terra)
library(tidyverse)
library(ggplot2)
library(tidyterra)
library(egg) # adds tags to facets
library(gridExtra)
library("cowplot")
library(ggpubr)

# Import data: 
data.dir <- "/Volumes/MaloneLab/Research/RUSTMAPPER"
figure.dir <- "/Users/sm3466/Dropbox (YSE)/Research/RUSTMapper/FIGURES"

setwd(data.dir)

ensemble.est <- rast( paste(data.dir,"/Ensemble_1980-2099_EST.tif", sep=""))
ensemble.inv <- rast( paste(data.dir,"/Ensemble_1980-2099_INV.tif", sep=""))
load( "Final_ShapeFiles.RDATA")

# Contour:

ensemble.est <- terra::rast( paste(data.dir,"/Ensemble_1980-2099_EST.tif", sep=""))
ensemble.inv <- terra::rast( paste(data.dir,"/Ensemble_1980-2099_INV.tif", sep=""))


contour.rasters <- function( raster, threshold){
library(terra)  
  raster.filter <- raster
  raster.filter[ raster.filter < threshold] <- NA

  # Year sub
  new.raster <- rast()
  
  # For loop
  for( i in 1:nlyr( raster.filter )){
    print(i)
    r.1 <- raster.filter[[i]]
    r.1[ r.1 >= threshold] <- time(r.1)
    
    new.raster <- c( new.raster, r.1)
    
  }
  
  min.rast <- min(new.raster, na.rm=T)
  return(  min.rast )
}


min.rast.inv <- contour.rasters(raster = ensemble.inv , 
                                threshold = 0.5)

min.rast.est <- contour.rasters(raster = ensemble.est , 
                                threshold = 0.65)

library(ggplot2)
library(tidyterra)
library(AOI)

data.dir <- "/Volumes/MaloneLab/Research/RUSTMAPPER"
figure.dir <- "/Users/sm3466/Dropbox (YSE)/Research/RUSTMapper/FIGURES"
setwd(data.dir)

AOI = AOI::aoi_get(state = c("CO", "WA", "OR", "CA", "MT", "ID", "UT", "AZ", "NV", "WY", "NM"))
AOI <- st_transform( AOI, 4326)
load( "Final_ShapeFiles.RDATA")
load(file='WPBR_plots_DAYMET.RDATA')

cols <- c("darkblue","blue",  "magenta","goldenrod", "lightblue4", 
          "yellow", "cyan4", "deeppink","magenta4","sandybrown","springgreen", "green")


refugia.inv <- ggplot() + geom_sf(data=AOI , fill="white", color="gray", lwd=0.25) +
  geom_sf(data=wp.s , fill="white", col="black")+
  geom_spatraster_contour_filled(data = min.rast.inv, breaks = seq(1980, 2099, 10))  +
  scale_fill_manual(values=cols,labels = c("1980 - 1990", 
                                           "1991 - 2000",
                                           "2001 - 2010",
                                           "2011 - 2020",
                                           "2021 - 2030",
                                           "2031 - 2040",
                                           "2041 - 2050",
                                           "2051 - 2060",
                                           "2061 - 2070",
                                           "2071 - 2080",
                                           "2081 - 2090")) +
  theme_minimal()+
  guides(fill = guide_legend(title = "")) +theme(text = element_text(size = 20))


refugia.est <- ggplot() + geom_sf(data=AOI , fill="white", color="gray", lwd=0.25) +
  geom_sf(data=wp , fill="white", col="black")+
  geom_spatraster_contour_filled(data = min.rast.est, breaks = seq(1980, 2099, 10))  +
  scale_fill_manual(values=cols, labels = c("1980 - 1990", 
                                              "1991 - 2000",
                                              "2001 - 2010",
                                              "2011 - 2020",
                                              "2021 - 2030",
                                              "2031 - 2040",
                                              "2041 - 2050",
                                              "2051 - 2060",
                                              "2061 - 2070",
                                              "2071 - 2080",
                                              "2081 - 2090")) +
  theme_minimal() +
  guides(fill = guide_legend(title = "")) +theme(text = element_text(size = 20))
 
setwd(figure.dir)
png("08_REFUGIA_FIGURE.png", width = 600, height = 1000)
ggarrange(refugia.inv, refugia.est, labels=c("a", "b"), ncol=1, nrow=2, 
          common.legend = T,  font.label = list(size = 25, color = "black"))

dev.off()


# EOF
