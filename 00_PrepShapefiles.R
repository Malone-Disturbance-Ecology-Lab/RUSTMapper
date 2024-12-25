# Prepare Shapefiles for use:

library(sf)
library(sp)
library(AOI)
library(terra)
library(tidyverse)

## Prepare shapefiles for use in other workflows:

AOI = aoi_get(state = c("CO", "WA", "OR", "CA", "MT", "ID", "UT", "AZ", "NV", "WY", "NM")) %>% st_transform(crs('+proj=lcc +lat_0=42.5 +lon_0=-100 +lat_1=25 +lat_2=60 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs ')) %>% st_as_sf

AOI.union = AOI %>% st_union %>% st_as_sf

invasion.regions <- read_sf("/Users/sm3466/Dropbox (YSE)/Research/WPBR/Shapefiles/WPBR_ECOGREGIONS_POLYS", "WPBR_INVASION_ECO_CLIPPED") %>% st_transform( crs(AOI)) %>% st_make_valid

load( "/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/04042023/ShapeFiles.RDATA")

wp <- wp %>% st_as_sf %>% st_transform(crs(AOI)) %>% st_intersection(AOI) %>% mutate(region='all') %>% st_make_valid() %>% st_as_sf %>% subset(select= region) %>%  group_by(region) %>% summarize() 

wp.cce <- wp.cce %>% st_as_sf %>% st_transform(crs(AOI)) %>% st_intersection(AOI) %>% mutate(region='cce') %>% st_make_valid() %>% st_as_sf %>% subset(select= region) %>%  group_by(region) %>% summarize() 

wp.gb <- wp.gb %>% st_as_sf %>% st_transform(crs(AOI)) %>% st_intersection(AOI) %>% mutate(region='gb') %>% st_make_valid() %>% st_as_sf %>% subset(select= region) %>%  group_by(region) %>% summarize() 

wp.gye <- wp.gye %>% st_as_sf %>% st_transform(crs(AOI)) %>% st_intersection(AOI) %>% mutate(region='gye') %>% st_make_valid() %>% st_as_sf %>% subset(select= region) %>%  group_by(region) %>% summarize() 

wp.pnw <- wp.pnw %>% st_as_sf %>% st_transform(crs(AOI)) %>% st_intersection(AOI) %>% mutate(region='pnw') %>% st_make_valid() %>% st_as_sf %>% subset(select= region) %>%  group_by(region) %>% summarize() 

wp.sr <- wp.sr %>% st_as_sf %>% st_transform(crs(AOI)) %>% st_intersection(AOI) %>% mutate(region='sr') %>% st_make_valid() %>% st_as_sf %>% subset(select= region) %>%  group_by(region) %>% summarize() 

wp.ssn <- wp.ssn %>% st_as_sf %>% st_transform(crs(AOI)) %>% st_intersection(AOI) %>% mutate(region='ssn') %>% st_make_valid() %>% st_as_sf %>% subset(select= region) %>%  group_by(region) %>% summarize() 

wp.sw <- wp.sw %>% st_as_sf %>% st_transform(crs(AOI)) %>% st_intersection(AOI) %>% mutate(region='sw') %>% st_make_valid() %>% st_as_sf %>% subset(select= region) %>%  group_by(region) %>% summarize() 


regions <- rbind( wp, wp.cce, wp.gb ,wp.gye, wp.pnw, wp.sr, wp.ssn, wp.sw) %>% st_make_valid()

regions <- regions[!st_is_empty(regions),drop=FALSE]

PIBA <- PIBA %>%  st_transform( crs(AOI)) %>% st_intersection(AOI) %>% st_make_valid
PIAL <-  PIAL %>%  st_transform( crs(AOI)) %>% st_intersection(AOI) %>% st_make_valid
PIFL <-  PIFL %>%  st_transform( crs(AOI)) %>% st_intersection(AOI) %>% st_make_valid
PILO <- PILO %>%   st_transform( crs(AOI)) %>% st_intersection(AOI) %>% st_make_valid
PIAR <-  PIAR %>%   st_transform( crs(AOI)) %>% st_intersection(AOI) %>% st_make_valid
PIST <-  PIST %>%   st_transform( crs(AOI)) %>% st_intersection(AOI) %>% st_make_valid

invasion.regions <- read_sf("/Users/sm3466/Dropbox (YSE)/Research/WPBR/Shapefiles/WPBR_ECOGREGIONS_POLYS", "WPBR_INVASION_ECO_CLIPPED") %>% st_transform( crs(AOI)) %>% st_make_valid

# Make invasion/ southern Species
wp.s <- wp %>% st_intersection(invasion.regions)
wp.cce.s <- wp.cce %>% st_intersection(invasion.regions)
wp.gb.s <- wp.gb %>% st_intersection(invasion.regions)
wp.gye.s <- wp.gye %>% st_intersection(invasion.regions)
wp.sr.s <- wp.sr %>% st_intersection(invasion.regions)
wp.ssn.s <- wp.ssn %>% st_intersection(invasion.regions)
wp.sw.s <- wp.sw %>% st_intersection(invasion.regions)

PIBA.s <- PIBA %>% st_intersection(invasion.regions)
PIAL.s <- PIAL %>%  st_intersection(invasion.regions)
PIFL.s <- PIFL %>% st_intersection(invasion.regions)
PILO.s <- PILO %>% st_intersection(invasion.regions)
PIAR.s <- PIAR %>% st_intersection(invasion.regions)
PIST.s <- PIST %>% st_intersection(invasion.regions)

# Save all files to one .Rdata Object for use in all other flow.r scripts (Tables and Maps)

wp.cce <- wp.cce %>% st_as_sf %>% st_transform(crs(AOI)) %>% st_intersection(AOI) %>% mutate(region='cce') %>% st_make_valid() %>% st_as_sf %>% subset(select= region) %>%  group_by(region) %>% summarize() 

wp.gb <- wp.gb %>% st_as_sf %>% st_transform(crs(AOI)) %>% st_intersection(AOI) %>% mutate(region='gb') %>% st_make_valid() %>% st_as_sf %>% subset(select= region) %>%  group_by(region) %>% summarize() 

wp.gye <- wp.gye %>% st_as_sf %>% st_transform(crs(AOI)) %>% st_intersection(AOI) %>% mutate(region='gye') %>% st_make_valid() %>% st_as_sf %>% subset(select= region) %>%  group_by(region) %>% summarize() 

wp.pnw <- wp.pnw %>% st_as_sf %>% st_transform(crs(AOI)) %>% st_intersection(AOI) %>% mutate(region='pnw') %>% st_make_valid() %>% st_as_sf %>% subset(select= region) %>%  group_by(region) %>% summarize() 

wp.sr <- wp.sr %>% st_as_sf %>% st_transform(crs(AOI)) %>% st_intersection(AOI) %>% mutate(region='sr') %>% st_make_valid() %>% st_as_sf %>% subset(select= region) %>%  group_by(region) %>% summarize() 

wp.ssn <- wp.ssn %>% st_as_sf %>% st_transform(crs(AOI)) %>% st_intersection(AOI) %>% mutate(region='ssn') %>% st_make_valid() %>% st_as_sf %>% subset(select= region) %>%  group_by(region) %>% summarize() 

wp.sw <- wp.sw %>% st_as_sf %>% st_transform(crs(AOI)) %>% st_intersection(AOI) %>% mutate(region='sw') %>% st_make_valid() %>% st_as_sf %>% subset(select= region) %>%  group_by(region) %>% summarize() 

regions <- rbind( wp, wp.cce, wp.gb ,wp.gye, wp.pnw, wp.sr, wp.ssn, wp.sw) %>% st_make_valid()

regions <- regions[!st_is_empty(regions),drop=FALSE]

save( regions, AOI, AOI.union, PIBA ,PIAL ,PIFL, PILO, PIAR, PIST,
wp.s, wp.cce.s, wp.gb.s, wp.gye.s, wp.sr.s,
wp.ssn.s, wp.sw.s,
wp, wp.cce, wp.gb, wp.gye, wp.pnw, wp.sr,
wp.ssn, wp.pnw, wp.sw,
PIBA.s, PIAL.s, PIFL.s, PILO.s, PIAR.s, PIST.s, invasion.regions,
file="/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/Final Scripts/Final_ShapeFiles.RDATA")

