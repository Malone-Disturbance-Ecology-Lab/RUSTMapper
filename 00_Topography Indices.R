
# Create topography indices from Elevation:
#install.packages("remotes")
#remotes::install_github("jhollist/elevatr")

library(elevatr)
library(rgdal)
library(terra)
library(beepr)
library(spatialEco)

wpbr <- readOGR(dsn="/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/04042023",  layer="WWETAC_040423")

elev <- get_elev_raster(wpbr, z= 9, clip='bbox',  serial = TRUE )
elev <-rast(elev)

# Uses the Spatial Eco package:
tpi <- tpi(elev, s = 9)
tri <- tri(elev) # This takes a long time

library(raster)
writeRaster(raster(elev),'/Users/sm3466/Dropbox (YSE)/Research/WPBR/Shapefiles/DEM_2021.tif', format="GTiff" , overwrite=T  )
writeRaster(raster(tpi),'/Users/sm3466/Dropbox (YSE)/Research/WPBR/Shapefiles/tpi_2021.tif', format="GTiff" , overwrite=T )
writeRaster(raster(tri),'/Users/sm3466/Dropbox (YSE)/Research/WPBR/Shapefiles/tri_2021.tif', format="GTiff" , overwrite=T )

