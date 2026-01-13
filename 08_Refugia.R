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


load( "Final_ShapeFiles.RDATA")
load(file='WPBR_plots_DAYMET.RDATA')

AOI <- st_transform( AOI, crs(min.rast.est ))
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

library(ggpubr)

Final.refugia <- ggarrange( refugia.inv, refugia.est, common.legend = T, ncol=2, labels=c("a", "b"))

setwd(figure.dir)
ggsave(filename="08_REFUGIA_FIGUREA.png", plot=Final.refugia,  width = 8, height = 6, dpi = 300)




# Refugia
ensemble.est.mean <- ensemble.est %>% mean(na.rm=T)

refugia <- function( model.layers, years.thres, year){
  
  # Identify all area of interest
  model.layers.mean <- model.layers %>% mean
  model.layers.mean[ model.layers.mean > 0]<- 1
  
  # extract contour years
  
  refugia.layer <- years.thres
  refugia.layer[refugia.layer < year] <- NA
  refugia.layer.n <- refugia.layer + model.layers.mean
  refugia.layer.n[refugia.layer.n >= 1] <- 1
  
  return(refugia.layer.n)
}


refugia.est.2050 <- refugia(model.layers = ensemble.est, 
                            years.thres = min.rast.est, year=2050 ) %>% aggregate( fact=5,fun=max, expand=TRUE, na.rm=TRUE)

refugia.est.2075 <- refugia(model.layers = ensemble.est, 
                            years.thres = min.rast.est, year=2075 ) %>% aggregate( fact=5,fun=max, expand=TRUE, na.rm=TRUE)

refugia.est.2080 <- refugia(model.layers = ensemble.est, 
                            years.thres = min.rast.est, year=2080 ) %>% aggregate( fact=5,fun=max, expand=TRUE, na.rm=TRUE)

length( refugia.est.2080[ refugia.est.2050 ==1])
length( refugia.est.2080[ refugia.est.2075 ==1])
length( refugia.est.2080[ refugia.est.2080 ==1])

ensemble.est.mean %>% plot(col="grey")
refugia.est.2080 %>% sum( na.rm=T) %>% plot(add=T, col="red")

ensemble.inv.mean <- ensemble.inv %>% mean(na.rm=T)

refugia.inv.2050 <- refugia(model.layers = ensemble.inv, 
                            years.thres = min.rast.inv, year=2050 ) %>% aggregate( fact=5,fun=max, expand=TRUE, na.rm=TRUE)

refugia.inv.2075 <- refugia(model.layers = ensemble.inv, 
                            years.thres = min.rast.inv, year=2075 ) %>% aggregate( fact=5,fun=max, expand=TRUE, na.rm=TRUE)

refugia.inv.2080 <- refugia(model.layers = ensemble.inv, 
                            years.thres = min.rast.inv, year=2080 ) %>% aggregate( fact=5,fun=max, expand=TRUE, na.rm=TRUE)

length( refugia.inv.2080[ refugia.inv.2050 ==1])
length( refugia.inv.2080[ refugia.inv.2075 ==1])
length( refugia.inv.2080[ refugia.inv.2080 ==1])

          na.value = "transparent"
          

p.r.est.2050 <- ggplot() + geom_sf(data=AOI , fill="white", color="gray", lwd=0.25) +
  geom_sf(data=wp , fill="grey", col="grey") +
  geom_spatraster(data = refugia.est.2050 %>% as.factor, show.legend = F) +
  scale_fill_manual(values="red", na.value = "transparent",
                    labels = c("Refugia", ""))
  
p.r.est.2075 <- ggplot() + geom_sf(data=AOI , fill="white", color="gray", lwd=0.25) +
  geom_sf(data=wp , fill="grey", col="grey") +
  geom_spatraster(data = refugia.est.2075 %>% as.factor, show.legend = F) +  
  scale_fill_manual(values="red", na.value = "transparent",
                    labels = c("Refugia", ""))

p.r.est.2080 <- ggplot() + geom_sf(data=AOI , fill="white", color="gray", lwd=0.25) +
  geom_sf(data=wp , fill="grey", col="grey") +
  geom_spatraster(data = refugia.est.2080 %>% as.factor, show.legend = F) +
  scale_fill_manual(values="red", na.value = "transparent",
                    labels = c("Refugia", ""))

p.r.inv.2050 <- ggplot() + geom_sf(data=AOI , fill="white", color="gray", lwd=0.25) +
  geom_sf(data=wp.s , fill="grey", col="grey") +
  geom_spatraster(data = refugia.inv.2050 %>% as.factor, show.legend = F) +
  scale_fill_manual(values="red", na.value = "transparent",
                    labels = c("Refugia", ""))

p.r.inv.2075 <- ggplot() + geom_sf(data=AOI , fill="white", color="gray", lwd=0.25) +
  geom_sf(data=wp.s , fill="grey", col="grey") +
  geom_spatraster(data = refugia.inv.2075 %>% as.factor, show.legend = F) +
  scale_fill_manual(values="red", na.value = "transparent",
                    labels = c("Refugia", ""))

p.r.inv.2080 <- ggplot() + geom_sf(data=AOI , fill="white", color="gray", lwd=0.25) +
  geom_sf(data=wp.s , fill="grey", col="grey") +
  geom_spatraster(data = refugia.inv.2080 %>% as.factor, show.legend = F) +
  scale_fill_manual(values="red", na.value = "transparent",
                    labels = c("Refugia", ""))
  

contours <-ggpubr::ggarrange(refugia.inv, refugia.est, 
          labels=c("a", "b"), ncol=2, nrow=1, 
          common.legend = T,font.label = list(size = 25, color = "black"))


refugia.risk <- ggpubr::ggarrange(p.r.inv.2050, p.r.est.2050,
          p.r.inv.2075, p.r.est.2075,
          p.r.inv.2080, p.r.est.2080,
          labels=c("c", "d", 
                   "e", "f",  
                   "g", "h"), ncol=2, nrow=3, 
          font.label = list(size = 25, color = "black"),
          common.legend = T)



setwd(figure.dir)
png("08_REFUGIA_FIGUREB.png", width = 600, height = 1000)
refugia.risk
dev.off()

