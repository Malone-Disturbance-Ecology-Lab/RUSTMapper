#STEP5: climate change projections for the WPBR model layers:

# Layers for Spatial projection
library(dplyr)
library(sp)
library(AOI)
library(climateR)
library(terra)

data.dir <- "/Volumes/MaloneLab/Research/RUSTMAPPER"
setwd(data.dir)

# create spatial layers: ####

AOI = aoi_get(state = c("CO", "WA", "OR", "CA", "MT", "ID", "UT", "AZ", "NV", "WY", "NM", "TX", "ND", "SD", "KS", "NE"))

# get climate layers: ####

## Set dates for summer spring and fall:
startDate.spring = seq(as.Date('1980-04-01'), as.Date('2023-04-01'), by='year')
endDate.spring = seq(as.Date('1980-05-31'), as.Date('2023-05-31'), by='year')

startDate.summer = seq(as.Date('1980-06-01'), as.Date('2023-06-01'), by='year')
endDate.summer = seq(as.Date('1980-08-31'), as.Date('2023-08-31'), by='year')

startDate.fall = seq(as.Date('1980-09-01'), as.Date('2023-09-01'), by='year')
endDate.fall = seq(as.Date('1980-10-31'), as.Date('2023-10-31'), by='year')

startDate.winter = seq(as.Date('1980-01-01'), as.Date('2023-01-01'), by='year')
endDate.winter = seq(as.Date('1980-03-31'), as.Date('2023-03-31'), by='year')

mydir <- paste(data.dir,"/GRIDMET_CURRENT", sep ="")

setwd(mydir)

delfiles <- dir(path=mydir , pattern="*json")
file.remove(file.path(mydir, delfiles))


# Precipitation: 
for( i in 1:length(startDate.spring)){
  print(i)
  PRCP.spring <-  getGridMET( AOI, varname = 'pr',
                          startDate = startDate.spring[i],
                          endDate = endDate.spring[i])
  
  PRCP.Spring <- PRCP.spring$precipitation_amount %>% sum(na.rm=T)
  time(PRCP.Spring) <- format(startDate.spring[i], "%Y") %>% as.numeric
  writeRaster( PRCP.Spring, paste('PRCP_Spring',  format(startDate.spring[i], "%Y"), ".tiff" ,sep="_" ) ,overwrite=TRUE ) 
}

for( i in 1:length(startDate.summer)){
  print(i)
  PRCP.summer <- getGridMET( AOI, varname = 'pr',
                          startDate = startDate.summer[i],
                          endDate = endDate.summer[i])
  
  PRCP_Summer <- PRCP.summer$precipitation_amount %>% sum(na.rm=T)
  time(PRCP_Summer) <- format(startDate.summer[i], "%Y")%>% as.numeric
  writeRaster( PRCP_Summer ,paste('PRCP_Summer',  format(startDate.summer[i], "%Y"), ".tiff" ,sep="_" ) ,overwrite=TRUE)
}

for( i in 1:length(startDate.fall)){
  print(i)
  PRCP.fall<- getGridMET( AOI, varname = 'pr',
                       startDate = startDate.fall[i],
                       endDate = endDate.fall[i])
  
  PRCP_Fall <- PRCP.fall$precipitation_amount %>% sum(na.rm=T)
  time(PRCP_Fall) <- format(startDate.fall[i], "%Y")%>% as.numeric
  writeRaster( PRCP_Fall ,paste('PRCP_Fall',  format(startDate.fall[i], "%Y"), ".tiff" ,sep="_" ) ,overwrite=TRUE)
}

# Temperature Variables:
for( i in 1:length(startDate.spring)){
  print(i)
  Tmin.spring <- getGridMET( AOI, varname = 'tmmn',
                          startDate = startDate.spring[i],
                          endDate = endDate.spring[i])
  
  Tmin.Spring <- min(Tmin.spring$daily_minimum_temperature, na.rm=T ) -273.15 
  time(Tmin.Spring) <- format(startDate.spring[i], "%Y")%>% as.numeric
  writeRaster( Tmin.Spring, paste('Tmin_Spring',  format(startDate.spring[i], "%Y"), ".tiff" ,sep="_" ), overwrite=T )
  
  
  Tmax.spring <- getGridMET( AOI, varname = 'tmmx',
                          startDate = startDate.spring[i],
                          endDate = endDate.spring[i])
  Tmax.Spring <- max(Tmax.spring$daily_maximum_temperature ) - 273.15 
  time(Tmax.Spring) <- format(startDate.spring[i], "%Y")%>% as.numeric
  writeRaster( Tmax.Spring,paste('Tmax_Spring',  format(startDate.spring[i], "%Y"), ".tiff" ,sep="_" ) , overwrite=T)
  
  Tmean.Spring = (0.606 * Tmax.Spring) + (0.394 * Tmin.Spring) 
  
  time(Tmean.Spring) <- format(startDate.spring[i], "%Y")%>% as.numeric
  writeRaster( Tmean.Spring, paste('Tmean_Spring',  format(startDate.spring[i], "%Y"), ".tiff" ,sep="_" ) , overwrite=T)
}

for( i in 1:length(startDate.summer)){
  print(i)
  Tmin.summer<- getGridMET( AOI, varname = 'tmmn',
                         startDate = startDate.summer[i],
                         endDate = endDate.summer[i])
  Tmin.Summer <- min(Tmin.summer$daily_minimum_temperature ,na.rm=T ) -273.15 
  time(Tmin.Summer) <- format(startDate.summer[i], "%Y")%>% as.numeric
  writeRaster( Tmin.Summer,paste('Tmin_Summer',  format(startDate.summer[i], "%Y"), ".tiff" ,sep="_" ) , overwrite=T )
  
  Tmax.summer <- getGridMET( AOI, varname = 'tmmx',
                          startDate = startDate.summer[i],
                          endDate = endDate.summer[i], verbose=T)
  Tmax.Summer <- max(Tmax.summer$daily_maximum_temperature  )  - 273.15 
  time(Tmax.Summer) <- format(startDate.summer[i], "%Y")%>% as.numeric
  writeRaster( Tmax.Summer,paste('Tmax_Summer',  format(startDate.summer[i], "%Y"), ".tiff" ,sep="_" ), overwrite=T)
  
  Tmean.Summer = (0.606 *Tmax.Summer) + (0.394 * Tmin.Summer)
  time(Tmean.Summer) <- format(startDate.summer[i], "%Y")%>% as.numeric
  writeRaster( Tmean.Summer,paste('Tmean_Summer',  format(startDate.summer[i], "%Y"), ".tiff" ,sep="_" ) , overwrite=T)
  
}

for( i in 1:length(startDate.fall)){
  print(i)
  Tmin.fall <- getGridMET( AOI, varname = 'tmmn',
                        startDate = startDate.fall[i],
                        endDate = endDate.fall[i])
  Tmin.Fall <- min(Tmin.fall$daily_minimum_temperature , na.rm=T ) -273.15 
  time(Tmin.Fall) <- format(startDate.fall[i], "%Y")%>% as.numeric
  writeRaster( Tmin.Fall,paste('Tmin_Fall',  format(startDate.fall[i], "%Y"), ".tiff" ,sep="_" ) , overwrite=T)
  
  Tmax.fall <- getGridMET( AOI, varname = 'tmmx',
                        
                        startDate = startDate.fall[i],
                        endDate = endDate.fall[i])
  Tmax.Fall <- max(Tmax.fall$daily_maximum_temperature ,na.rm=T )  - 273.15 
  time(Tmax.Fall) <- format(startDate.fall[i], "%Y")%>% as.numeric
  writeRaster( Tmax.Fall,paste('Tmax_Fall',  format(startDate.fall[i], "%Y"), ".tiff" ,sep="_" ) , overwrite=T)
  
  
  Tmean.Fall = (0.606 *Tmax.Fall) + (0.394 * Tmin.Fall)
  time(Tmean.Fall) <- format(startDate.fall[i], "%Y")%>% as.numeric
  writeRaster( Tmean.Fall,paste('Tmean_Fall',  format(startDate.fall[i], "%Y"), ".tiff" ,sep="_" ) , overwrite=T)
  
}

# VPD variables:
for( i in 1:length(startDate.spring)){
  print(i)
  VPD.spring <- getGridMET( AOI, varname = 'vpd',
                         startDate = startDate.spring[i],
                         endDate = endDate.spring[i])
  
  VPD.Spring <- mean(VPD.spring$daily_mean_vapor_pressure_deficit, na.rm=T) * 1000
  time(VPD.Spring) <- format(startDate.spring[i], "%Y")%>% as.numeric
  writeRaster( VPD.Spring,paste('VPD_Spring',  format(startDate.spring[i], "%Y"), ".tiff" ,sep="_" ), overwrite=T )
  VPDMAX.Spring <- max(VPD.spring$daily_mean_vapor_pressure_deficit, na.rm=T)* 1000
  time(VPDMAX.Spring) <- format(startDate.spring[i], "%Y")%>% as.numeric
  writeRaster( VPDMAX.Spring,paste('VPDMAX_Spring',  format(startDate.spring[i], "%Y"), ".tiff" ,sep="_" ), overwrite=T )
  
}
for( i in 1:length(startDate.summer)){
  print(i)
  VPD.summer <- getGridMET( AOI, varname = 'vpd',
                         startDate = startDate.spring[i],
                         endDate = endDate.spring[i])
  VPD.Summer <- mean(VPD.summer$daily_mean_vapor_pressure_deficit, na.rm=T) * 1000
  time(VPD.Summer) <- format(startDate.spring[i], "%Y")%>% as.numeric
  writeRaster( VPD.Summer,paste('VPD_Summer',  format(startDate.spring[i], "%Y"), ".tiff" ,sep="_" ) , overwrite=T) 
  VPDMAX.Summer <- max(VPD.summer$daily_mean_vapor_pressure_deficit, na.rm=T)* 1000
  time(VPDMAX.Summer) <- format(startDate.spring[i], "%Y")%>% as.numeric
  writeRaster( VPDMAX.Summer,paste('VPDMAX_Summer',  format(startDate.spring[i], "%Y"), ".tiff" ,sep="_" ), overwrite=T )
  rm( VPD.summer )
}
for( i in 1:length(startDate.fall)){
  print(i)
  VPD.fall <- getGridMET( AOI, varname = 'vpd',
                       startDate = startDate.fall[i],
                       endDate = endDate.fall[i])
  VPD.Fall <- mean(VPD.fall$daily_mean_vapor_pressure_deficit, na.rm=T) * 1000
  time(VPD.Fall) <- format(startDate.fall[i], "%Y")%>% as.numeric
  writeRaster( VPD.Fall,paste('VPD_fall',  format(startDate.fall[i], "%Y"), ".tiff" ,sep="_" ), overwrite=T)
  VPDMAX.Fall <- max(VPD.fall$daily_mean_vapor_pressure_deficit, na.rm=T)*1000
  time(VPDMAX.Fall) <- format(startDate.fall[i], "%Y")%>% as.numeric
  writeRaster( VPDMAX.Fall,paste('VPDMAX_fall',  format(startDate.fall[i], "%Y"), ".tiff" ,sep="_" ) , overwrite=T)
  rm( VPD.fall)
}

# RH

for( i in 1:length(startDate.spring)){
  print(i)
  RH.spring.min <- getGridMET( AOI, varname = 'rmin',
                            startDate = startDate.spring[i],
                            endDate = endDate.spring[i])
  
  RH.spring.max <- getGridMET( AOI, varname = 'rmax',
                            startDate = startDate.spring[i],
                            endDate = endDate.spring[i])
  
  RH.Spring.min <- mean(RH.spring.min$daily_minimum_relative_humidity, na.rm=T) 
  RH.Spring.max <- mean(RH.spring.max$daily_maximum_relative_humidity, na.rm=T)
  RH.Spring <- mean(RH.Spring.min , RH.Spring.max, na.rm=T) 
  
  time(RH.Spring) <- format(startDate.spring[i], "%Y")%>% as.numeric
  writeRaster( RH.Spring, paste('RH.Spring',  format(startDate.spring[i], "%Y"), ".tiff" ,sep="_" ), overwrite=T )
}
for( i in 1:length(startDate.summer)){
  print(i)
  RH.summer.min <- getGridMET( AOI, varname = 'rmin',
                            startDate = startDate.summer[i],
                            endDate = endDate.summer[i])
  
  RH.summer.max <- getGridMET( AOI, varname = 'rmax',
                            startDate = startDate.summer[i],
                            endDate = endDate.summer[i])
  
  RH.summer.min <- mean(RH.summer.min$daily_minimum_relative_humidity, na.rm=T) 
  RH.summer.max <- mean(RH.summer.max$daily_maximum_relative_humidity, na.rm=T)
  RH.Summer <- mean(RH.summer.min , RH.summer.max, na.rm=T)
  time(RH.Summer) <- format(startDate.summer[i], "%Y")%>% as.numeric
  writeRaster( RH.Summer,paste('RH.Summer',  format(startDate.summer[i], "%Y"), ".tiff" ,sep="_" ) , overwrite=T) 
}
for( i in 1:length(startDate.fall)){
  print(i)
  RH.fall.max <- getGridMET( AOI, varname = 'rmax',
                          startDate = startDate.fall[i],
                          endDate = endDate.fall[i])
  RH.fall.min <- getGridMET( AOI, varname = 'rmin',
                          startDate = startDate.fall[i],
                          endDate = endDate.fall[i])
  
  RH.fall.min <- mean(RH.fall.min$daily_minimum_relative_humidity, na.rm=T) 
  RH.fall.max <- mean(RH.fall.max$daily_maximum_relative_humidity, na.rm=T)
  RH.Fall <- mean(RH.fall.min , RH.fall.max, na.rm=T) 
  time(RH.Fall) <- format(startDate.fall[i], "%Y")%>% as.numeric
  writeRaster( RH.Fall,paste('RH.Fall',  format(startDate.fall[i], "%Y"), ".tiff" ,sep="_" ) , overwrite=T)
}

# Winter temp to use for H_Zone:
for( i in 1:length(startDate.winter)){
  print(i)
  Tmin.winter <- getGridMET( AOI, varname = 'tmmn',
                             startDate = startDate.winter[i],
                             endDate = endDate.winter[i])
  
  Tmin.Winter <- min(Tmin.winter$daily_minimum_temperature, na.rm=T ) -273.15 
  time(Tmin.Winter) <- format(startDate.winter[i], "%Y")%>% as.numeric
  writeRaster( Tmin.Winter, paste('Tmin_Winter',  format(startDate.winter[i], "%Y"), ".tiff" ,sep="_" ), overwrite=T )
}