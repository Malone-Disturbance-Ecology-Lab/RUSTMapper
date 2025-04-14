
# Create topography indices from Elevation:
#install.packages("remotes")
#remotes::install_github("jhollist/elevatr")

library(elevatr)
library(rgdal)
library(terra)
library(beepr)
library(spatialEco)


data.dir <- "/Volumes/MaloneLab/Research/RUSTMAPPER"
setwd(data.dir)

projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
wpbr <- read.csv("WWETAC_040423_CorrectedOriginal withNewData_040425.csv") %>%
  mutate( LAT = POINT_X, LON = POINT_Y) %>% st_as_sf(coords = c("POINT_X", "POINT_Y"), crs = projcrs)

elev <- get_elev_raster(wpbr, z= 9, clip='bbox',  serial = TRUE )
elev <-rast(elev)

# Uses the Spatial Eco package:
tpi <- tpi(elev, s = 9)
tri <- tri(elev) # This takes a long time

library(raster)
writeRaster(raster(elev),'DEM_2021.tif', format="GTiff" , overwrite=T  )
writeRaster(raster(tpi),'tpi_2021.tif', format="GTiff" , overwrite=T )
writeRaster(raster(tri),'tri_2021.tif', format="GTiff" , overwrite=T )

