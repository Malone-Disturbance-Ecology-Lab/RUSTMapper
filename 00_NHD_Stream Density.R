# NHD data: https://apps.nationalmap.gov/downloader/#/


# Calculate Stream Density across the western US.
rm(list=ls())

library(AOI)
library(sf)
options(stringsAsFactors = FALSE)
library(tidyverse)
library(inborutils)
library(stars)
library(beepr)

#install.packages("inborutils", repos = c(inbo = "https://inbo.r-universe.dev",  CRAN = "https://cloud.r-project.org"))

setwd('/Volumes/MaloneLab/Research/StreamDensity/NHD')

st_layers('/Volumes/MaloneLab/Research/StreamDensity/NHD/NHD_H_Colorado_State_GDB/NHD_H_Colorado_State_GDB.gdb')


co <- st_read('/Volumes/MaloneLab/Research/StreamDensity/NHD/NHD_H_Colorado_State_GDB/NHD_H_Colorado_State_GDB.gdb',layer = "NHDFlowline")

az <- st_read("/Volumes/MaloneLab/Research/StreamDensity/NHD/NHD_H_Arizona_State_GDB/NHD_H_Arizona_State_GDB.gdb",layer = "NHDFlowline")
ca <- st_read("/Volumes/MaloneLab/Research/StreamDensity/NHD/NHD_H_California_State_GDB/NHD_H_California_State_GDB.gdb",layer = "NHDFlowline")
id <- st_read("/Volumes/MaloneLab/Research/StreamDensity/NHD/NHD_H_Idaho_State_GDB/NHD_H_Idaho_State_GDB.gdb",layer = "NHDFlowline")
mt <- st_read("/Volumes/MaloneLab/Research/StreamDensity/NHD/NHD_H_Montana_State_GDB/NHD_H_Montana_State_GDB.gdb",layer = "NHDFlowline")
nm <- st_read("/Volumes/MaloneLab/Research/StreamDensity/NHD/NHD_H_New_Mexico_State_GDB/NHD_H_New_Mexico_State_GDB.gdb",layer = "NHDFlowline")
or <- st_read("/Volumes/MaloneLab/Research/StreamDensity/NHD/NHD_H_Oregon_State_GDB/NHD_H_Oregon_State_GDB.gdb",layer = "NHDFlowline")
tx <- st_read("/Volumes/MaloneLab/Research/StreamDensity/NHD/NHD_H_Texas_State_GDB/NHD_H_Texas_State_GDB.gdb",layer = "NHDFlowline")
ut <- st_read("/Volumes/MaloneLab/Research/StreamDensity/NHD/NHD_H_Utah_State_GDB/NHD_H_Utah_State_GDB.gdb",layer = "NHDFlowline")
wa <- st_read("/Volumes/MaloneLab/Research/StreamDensity/NHD/NHD_H_Washington_State_GDB/NHD_H_Washington_State_GDB.gdb",layer = "NHDFlowline")
wy <- st_read("/Volumes/MaloneLab/Research/StreamDensity/NHD/NHD_H_Wyoming_State_GDB/NHD_H_Wyoming_State_GDB.gdb",layer = "NHDFlowline")
nv <- st_read("/Volumes/MaloneLab/Research/StreamDensity/NHD/NHD_H_Nevada_State_GDB/NHD_H_Nevada_State_GDB.gdb",layer = "NHDFlowline")

# Update geometry to remove ZM:

co <- st_zm( co , drop = TRUE, what = "ZM")
az<- st_zm( az , drop = TRUE, what = "ZM")
ca <- st_zm( ca , drop = TRUE, what = "ZM")
id <- st_zm( id , drop = TRUE, what = "ZM")
mt <- st_zm( mt , drop = TRUE, what = "ZM")
nm <- st_zm( nm , drop = TRUE, what = "ZM")
or <- st_zm( or , drop = TRUE, what = "ZM")
tx <- st_zm( tx , drop = TRUE, what = "ZM")
ut <- st_zm( ut , drop = TRUE, what = "ZM")
wa <- st_zm( wa , drop = TRUE, what = "ZM")
wy <- st_zm( wy , drop = TRUE, what = "ZM")
nv <- st_zm( nv , drop = TRUE, what = "ZM")

# Combine and develop a western US stream layer:

west <- rbind(co, az, ca, id, mt, nm, or, tx, ut, wa, wy, nv) # Best way to joining all states together. Would require the files to be the same. 

rm(co, az, ca, id, mt, nm, or, tx, ut, wa, wy, nv)

names(west) # get the columns to id distint geometry:
#west <- west %>% distinct(X,Y) # remove any duplicates from the dataset:

west = st_transform( west, "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

# Crop the simple feature:
AOI = aoi_get(state = c("CO", "WA", "OR", "CA", "MT", "ID", "UT", "AZ", "NV", "WY", "NM", "TX")) %>% st_transform( "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")


# Make grid
grid = st_as_stars(st_bbox(AOI), dx = 2500, dy = 2500)
grid = st_as_sf(grid)
grid$ID <- seq(1, length(grid$values), 1)

grid.intersect <- st_intersection(grid, west[,"lengthkm"]  ) # get streams in each gridcell
  
# Information on Density:
lot <- data.frame(tapply(st_length(grid.intersect)/1000, grid.intersect$ID,sum))
lot$ID <- as.numeric(rownames(lot)) # makes the ID numeric to match the ID in the grid
lot$StreamLength <- lot$tapply.st_length.grid.intersect..1000..grid.intersect.ID..sum. # adds stream density
lot$tapply.st_length.grid.intersect..1000..grid.intersect.ID..sum. <- NULL # Removes column

grid <- grid %>% left_join(lot, by="ID" )
names(grid)

STREAM <- st_rasterize( grid, st_as_stars(st_bbox(AOI), nx =2500, ny = 2500, values = NA_real_)) %>%  terra::rast()

STREAM$area <-terra::cellSize(STREAM, unit="km")

STREAM$Density <- STREAM$StreamLength_lyr.1 / STREAM$area

STREAM$Density %>% plot
STREAM$Density %>% hist

terra::writeRaster( STREAM, '~/Dropbox (YSE)/Research/WPBR/StreamDensity_WestUSA_1km.tif',overwrite=TRUE)

# Compare
