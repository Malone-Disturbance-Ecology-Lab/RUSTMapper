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

refugia.inv <- ensemble.inv
refugia.inv[ensemble.inv >= 0.3] <- 0
refugia.inv[ensemble.inv < 0.3] <- 1

refugia.total.inv <- sum(refugia.inv, na.rm=T)
plot(refugia.total.inv)


Refugia.INV <- ggplot() + geom_sf(data=AOI, fill="black",  col="white",alpha=0.75) + geom_spatraster( data=refugia.total.inv, aes(fill = sum)) + scale_fill_gradient2(
  low = "magenta", 
  mid = "cyan", 
  high = "yellow", 
  midpoint =57, 
  na.value = "transparent", name="Lower Risk (Years)") + theme(text = element_text(size = 15),
                                                               panel.background = element_rect(fill='transparent'))

# Create the refugia shapefile:

refugia.inv.sf <- ensemble.inv
refugia.inv.sf [ensemble.inv >= 0.3] <- 0
refugia.inv.sf [ensemble.inv < 0.3] <- 1

refugia.total.inv.sf <- sum(refugia.inv.sf , na.rm=T)
refugia.total.inv.sf.final <- refugia.total.inv.sf
refugia.total.inv.sf.final[ refugia.total.inv.sf.final > 1 ] <- NA


inv.ref.shp = as.polygons(refugia.total.inv.sf.final) %>% st_as_sf()

# Refugia Established:

refugia.est.sf <- ensemble.est
refugia.est.sf[ensemble.est >= 0.5] <- NA
refugia.est.sf[ensemble.est < 0.5] <- 1

refugia.total.est.sf <- sum(refugia.est.sf, na.rm=T)
refugia.total.est.sf.final <-refugia.total.est.sf
refugia.total.est.sf.final[ refugia.total.est.sf > 1 ] <- NA

est.ref.shp = as.polygons(refugia.total.est.sf.final) %>% st_as_sf()


refugia.est <- ensemble.est
refugia.est[ensemble.est >= 0.5] <- 0
refugia.est[ensemble.est < 0.5] <- 1
refugia.total.est <- sum(refugia.est, na.rm=T)

Refugia.EST <- ggplot() + geom_sf(data=AOI, fill="black", col="white", alpha=0.75) + geom_spatraster( data=refugia.total.est, aes(fill = sum)) + scale_fill_gradient2(
  low = "magenta", 
  mid = "cyan", 
  high = "yellow",  
  midpoint =57, 
  na.value = "transparent", name="Lower Risk (Years)") + theme(text = element_text(size = 15),
                                                               panel.background = element_rect(fill='transparent'))

total <- sum(refugia.total.est.sf.final, refugia.total.inv.sf.final, na.rm=T)

total[ total < 2] <- NA

total.ref.shp = as.polygons(total) %>% st_as_sf()



write_sf(total.ref.shp ,paste(data.dir,'/Shapefiles/Refugia.shp', sep="")  )

write_sf(inv.ref.shp ,paste(data.dir,'/Shapefiles/Refugia_INV.shp', sep="") )

write_sf(est.ref.shp ,paste(data.dir,'/Shapefiles/Refugia_EST.shp', sep="") )

refugia.aoi <- read_sf(paste(data.dir,'/Shapefiles/Refugia_AOI.kml', sep=""))

wp <- wp %>% st_as_sf %>% st_transform(crs(AOI)) %>% st_intersection(AOI) %>% mutate(region='all') %>% st_make_valid() %>% st_as_sf %>% subset(select= region) %>%  group_by(region) %>% summarize() 

Total.refugia <- ggplot() + geom_sf(data=AOI, fill="white", col="grey", alpha=0.75) + 
  geom_sf(data=wp, fill="black", col="black") +
  geom_sf( data =inv.ref.shp, fill="red", col="red")  + 
  geom_sf( data =est.ref.shp, fill="yellow", col="yellow") +
  geom_sf( data = total.ref.shp, fill="cyan", col="cyan") +
  xlab("") + ylab("") + theme(text = element_text(size = 15),
                              panel.background = element_rect(fill='transparent'))


figure <- ggpubr::ggarrange(Total.refugia ,
                            ggpubr::ggarrange(Refugia.INV, Refugia.EST, ncol=2, nrow=1, 
                                              labels=c("b", "c"),
                                              common.legend = TRUE, legend="top"),
                            ncol=1, nrow=2, heights=c(1.25, 1), labels="a")


setwd(figure.dir)
png("08_REFUGIA_FIGURE.png", width = 600, height = 600)
Total.refugia 
figure
dev.off()


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



ggplot() + geom_sf(data=AOI , fill="white", color="grey", lwd=0.5) +
  geom_sf(data=wp.s , fill="red")+
  geom_spatraster_contour_filled(data = min.rast.inv, breaks = seq(1980, 2099, 5))  +
  theme_minimal()


ggplot() + geom_sf(data=AOI , fill="white", color="grey", lwd=0.5) +
  geom_sf(data=wp , fill="cyan")+
  geom_spatraster_contour_filled(data = min.rast.est, breaks = seq(1980, 2099, 10))  +
  theme_minimal()


# EOF
