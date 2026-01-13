# Table 2: Model Summary_regional
 
rm(list = ls())

library(tidyverse)
library(sf)
library(AOI)
library(terra)

data.dir <- "/Volumes/MaloneLab/Research/RUSTMAPPER"
figure.dir <- "/Users/sm3466/Dropbox (YSE)/Research/RUSTMapper/FIGURES"

setwd(data.dir)

ensemble.est <- rast( paste(data.dir,"/Ensemble_1980-2099_EST.tif", sep=""))
ensemble.inv <- rast( paste(data.dir,"/Ensemble_1980-2099_INV.tif", sep=""))
load( "Final_ShapeFiles.RDATA")

sf.summary <- function (raster, SP){
  
  P.50 <- function(x ){
    p50 <- (length( x[ x > 0.5 ])/ length( x))*100
    return(p50)
  }
  
  raster.cropped <- terra::crop(raster, SP) %>% terra::mask( SP)
  df <-as.data.frame(raster.cropped) %>% na.omit()
  RANGE <- paste(round(range(df[,1])[1], 2)  , round(range(df[,1])[2], 2), sep = "-")
  MEAN <- mean(df[,1], na.rm=T) %>% round(2)
  Frac.High <- P.50( df[,1]) %>% round(2)
  summary <- data.frame( RANGE=RANGE, mean=MEAN , FH=Frac.High )
  return(summary)
  
}

table.SD <- function(FUN, raster.inv, raster.est ){
  load( "Final_ShapeFiles.RDATA")
  wp.summary.inv <- FUN (raster.inv, wp) %>% mutate(id = 'wp')
  wp.summary.est <- FUN (raster.est, wp) %>% mutate(id = 'wp')
  
  sr.summary.inv <- FUN (raster.inv, wp.sr) %>% mutate(id = 'wp.sr')
  sr.summary.est <- FUN (raster.est, wp.sr)%>% mutate(id = 'wp.sr')

  
  
  inv <- rbind(wp.summary.inv, 
               sr.summary.inv) %>% mutate(model = "Inv")
  
  est <- rbind(wp.summary.est, 
               sr.summary.est) %>% mutate(model = "EST")
  
  total.df <- inv %>% full_join( est, by = 'id')
  return( total.df )
}

# Summarize MODEL Layers: ####

Table.Current <- table.SD ( FUN =sf.summary,
                        raster.inv= ensemble.inv[[1:44]],
                        raster.est= ensemble.est[[1:44]])

# Nature Manuscript: ####

regional.table <- function(FUN, raster.inv, raster.est ){
  load( "/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/04042023/Final_ShapeFiles.RDATA")
  
  wp.summary.inv <- FUN (raster.inv, wp) %>% mutate(id = 'wp')
  wp.summary.est <- FUN (raster.est, wp) %>% mutate(id = 'wp')
  
  sr.summary.inv <- FUN (raster.inv, wp.sr) %>% mutate(id = 'wp.sr')
  sr.summary.est <- FUN (raster.est, wp.sr)%>% mutate(id = 'wp.sr')
  
  gb.summary.inv <- FUN (raster.inv, wp.gb) %>% mutate(id = 'wp.gb')
  gb.summary.est <- FUN (raster.est, wp.gb)%>% mutate(id = 'wp.gb')
  
  sw.summary.inv <- FUN (raster.inv, wp.sw) %>% mutate(id = 'wp.sw')
  sw.summary.est <- FUN (raster.est, wp.sw) %>% mutate(id = 'wp.sw')
  
  ssn.summary.inv <- FUN (raster.inv, wp.ssn) %>% mutate(id = 'wp.ssn')
  ssn.summary.est <- FUN (raster.est, wp.ssn)  %>% mutate(id = 'wp.ssn')
  
  pnw.summary.inv <- FUN (raster.inv, wp.pnw)  %>% mutate(id = 'wp.pnw')
  pnw.summary.est <- FUN (raster.est, wp.pnw)  %>% mutate(id = 'wp.pnw')
  
  gye.summary.inv <- FUN (raster.inv, wp.gye)  %>% mutate(id = 'wp.gye')
  gye.summary.est <- FUN (raster.est, wp.gye) %>% mutate(id = 'wp.gye')
  
  cce.summary.inv <- FUN (raster.inv, wp.cce) %>% mutate(id = 'wp.cce')
  cce.summary.est <- FUN (raster.est, wp.cce) %>% mutate(id = 'wp.cce')
  
  s.summary.inv <- FUN (raster.inv, wp.s) %>% mutate(id = 'wp.s')
  s.summary.est <- FUN (raster.est, wp.s)%>% mutate(id = 'wp.s')
  
  PIBA.summary.inv <- FUN (raster.inv, PIBA.s) %>% mutate(id = 'PIBA.s')
  PIBA.summary.est <- FUN (raster.est, PIBA)  %>% mutate(id = 'PIBA')
  PIBA.summary.est.s <- FUN (raster.est, PIBA.s)  %>% mutate(id = 'PIBA.s')
  
  PIAL.summary.inv <- FUN (raster.inv, PIAL.s)  %>% mutate(id = 'PIAL.s')
  PIAL.summary.est <- FUN (raster.est, PIAL) %>% mutate(id = 'PIAL')
  PIAL.summary.est.s <- FUN (raster.est, PIAL.s) %>% mutate(id = 'PIAL.s')
  
  PIFL.summary.inv <- FUN (raster.inv, PIFL.s) %>% mutate(id = 'PIFL.s')
  PIFL.summary.est <- FUN (raster.est, PIFL) %>% mutate(id = 'PIFL')
  PIFL.summary.est.s <- FUN (raster.est, PIFL.s) %>% mutate(id = 'PIFL.s')
  
  PILO.summary.inv <- FUN (raster.inv, PILO.s) %>% mutate(id = 'PILO.s')
  PILO.summary.est <- FUN (raster.est, PILO)%>% mutate(id = 'PILO')
  PILO.summary.est.s <- FUN (raster.est, PILO.s)%>% mutate(id = 'PILO.s')
  
  PIAR.summary.inv <- FUN (raster.inv, PIAR.s) %>% mutate(id = 'PIAR.s')
  PIAR.summary.est <- FUN (raster.est, PIAR)  %>% mutate(id = 'PIAR')
  PIAR.summary.est.s <- FUN (raster.est, PIAR.s)  %>% mutate(id = 'PIAR.s')
  
  PIST.summary.inv <- FUN (raster.inv, PIST.s)  %>% mutate(id = 'PIST.s')
  PIST.summary.est <- FUN (raster.est, PIST)  %>% mutate(id = 'PIST')
  PIST.summary.est.s <- FUN (raster.est, PIST.s)  %>% mutate(id = 'PIST.s')
  
  # Incomplete data regions
  
  pnw.summary.inv[, 1:(ncol(pnw.summary.inv)-1) ] <- NA
  cce.summary.inv[, 1:(ncol(cce.summary.inv)-1)] <- NA
  gye.summary.inv[, 1:(ncol(gye.summary.inv)-1)] <- NA
  
  inv <- rbind(wp.summary.inv, 
               s.summary.inv,
               pnw.summary.inv,
               cce.summary.inv,
               gye.summary.inv,
               sr.summary.inv, 
               gb.summary.inv,
               sw.summary.inv,
               ssn.summary.inv,
               
               PIAL.summary.inv,
               PIBA.summary.inv, 
               PIFL.summary.inv, 
               PILO.summary.inv,
               PIST.summary.inv,
               PIAR.summary.inv) %>% mutate(model = "Inv")
  
  est <- rbind(wp.summary.est, 
               s.summary.est,
               pnw.summary.est,
               cce.summary.est,
               gye.summary.est,
               sr.summary.est, 
               gb.summary.est,
               sw.summary.est,
               ssn.summary.est,
               
               PIAL.summary.est,
               PIAL.summary.est.s,
               PIBA.summary.est,
               PIBA.summary.est.s,
               PIFL.summary.est, 
               PIFL.summary.est.s,
               PILO.summary.est,
               PILO.summary.est.s,
               PIST.summary.est,
               PIST.summary.est.s,
               PIAR.summary.est,
               PIAR.summary.est.s) %>% mutate(model = "EST")
  
  total.df <- inv %>% full_join( est, by = 'id')
  return( total.df )
}


Table.Future <- regional.table( FUN =sf.summary ,
                          raster.inv= ensemble.inv[[45:114]],
                          raster.est= ensemble.est[[45:114]])

write.csv(Table.Current, "/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/WPBR/NewData/Final Scripts/FIGURES/08_Model_Current-Summary_Table.csv")
write.csv(Table.Future , "/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/WPBR/NewData/Final Scripts/FIGURES/08_Model_Future-Summary_Table.csv")

# High Fraction Table: ####
sf.summary.FracH <- function (raster, SP){
  
  P.50 <- function(x ){
    p50 <- (length( x[ x > 0.5 ])/ length( x))*100
    return(p50)
  }
  

  
  raster.cropped <- terra::crop(raster, SP) %>% terra::mask( SP)
  df <-as.data.frame(raster.cropped) %>% na.omit()
  Frac.High <- P.50( df[,1]) %>% round(2)
  summary <- data.frame( FH=Frac.High )
  return(summary)
  
}

Sub.table <- function(FUN, raster){
  load( "/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/04042023/Final_ShapeFiles.RDATA")

  wp.summary <- FUN (raster, wp) %>% mutate(id = 'wp')
  sr.summary <- FUN (raster, wp.sr)%>% mutate(id = 'wp.sr')
  gb.summary <- FUN (raster, wp.gb)%>% mutate(id = 'wp.gb')
  sw.summary <- FUN (raster, wp.sw) %>% mutate(id = 'wp.sw')
  ssn.summary <- FUN (raster, wp.ssn) %>% mutate(id = 'wp.ssn')
  pnw.summary <- FUN (raster, wp.pnw)  %>% mutate(id = 'wp.pnw')
  gye.summary <- FUN (raster, wp.gye)  %>% mutate(id = 'wp.gye')
  cce.summary <- FUN (raster.inv, wp.cce) %>% mutate(id = 'wp.cce')
 
  PIBA.summary <- FUN (raster, PIBA)  %>% mutate(id = 'PIBA')
  PIBA.summary.s <- FUN (raster, PIBA.s)  %>% mutate(id = 'PIBA.s')
  
  PIAL.summary <- FUN (raster, PIAL) %>% mutate(id = 'PIAL')
  PIAL.summary.s <- FUN (raster[[raster.limit]], PIAL.s) %>% mutate(id = 'PIAL.s')
  
  PIFL.summary <- FUN (raster, PIFL) %>% mutate(id = 'PIFL')
  PIFL.summary.s <- FUN (raster, PIFL.s) %>% mutate(id = 'PIFL.s')
  
  PILO.summary <- FUN (raster, PILO)%>% mutate(id = 'PILO')
  PILO.summary.s <- FUN (raster, PILO.s)%>% mutate(id = 'PILO.s')
  
  PIAR.summary <- FUN (raster, PIAR)  %>% mutate(id = 'PIAR')
  PIAR.summary.s <- FUN (raster, PIAR.s)  %>% mutate(id = 'PIAR.s')
  
  PIST.summary <- FUN (raster, PIST)  %>% mutate(id = 'PIST')
  PIST.summary.s <- FUN (raster, PIST.s)  %>% mutate(id = 'PIST.s')
  
  table.df <- rbind(wp.summary, 
               sr.summary,
               pnw.summary,
               cce.summary,
               gye.summary,
               sr.summary, 
               gb.summary,
               sw.summary,
               ssn.summary,
               
               PIAL.summary,
               PIAL.summary.s,
               PIBA.summary,
               PIBA.summary.s,
               PIFL.summary, 
               PIFL.summary.s,
               PILO.summary,
               PILO.summary.s,
               PIST.summary,
               PIST.summary.s,
               PIAR.summary,
               PIAR.summary.s)

  return( table.df )
}

FH.Table1 <- function( raster){
  
CurrentFH <- Sub.table(FUN =sf.summary, raster = raster[[1:44]] )
FH2030 <- Sub.table(FUN = sf.summary.FracH , raster = raster[[45:54]] ) %>% mutate( F2030 = FH) %>% select(id, F2030) %>% data.frame
FH2040 <- Sub.table(FUN = sf.summary.FracH , raster = raster[[55:64]] ) %>% mutate( F2040 = FH) %>% select(id, F2040) %>% data.frame
FH2050 <- Sub.table(FUN = sf.summary.FracH , raster = raster[[65:74]] ) %>% mutate( F2050 = FH) %>% select(id, F2050)%>% data.frame
FH2060 <- Sub.table(FUN = sf.summary.FracH , raster = raster[[75:84]] ) %>% mutate( F2060 = FH) %>% select(id, F2060)%>% data.frame
FH2070 <- Sub.table(FUN = sf.summary.FracH , raster = raster[[85:94]] ) %>% mutate( F2070 = FH) %>% select(id, F2070)%>% data.frame
FH2080 <- Sub.table(FUN = sf.summary.FracH , raster = raster[[95:104]] ) %>% mutate( F2080 = FH) %>% select(id, F2080)%>% data.frame
FH2090 <- Sub.table(FUN = sf.summary.FracH , raster = raster[[105:114]] ) %>% mutate( F2090 = FH) %>% select(id, F2090)%>% data.frame

 table.out <- cbind(CurrentFH, FH2030$F2030,
      FH2040$F2040, 
      FH2050$F2050, 
      FH2060$F2060,
      FH2070$F2070, 
      FH2080$F2080, 
      FH2090$F2090)

return(table.out )
}

FH.Table.inv <- FH.Table1(raster = ensemble.inv)
FH.Table.est <- FH.Table1(raster = ensemble.est)


# Regional Summaries Figures:

Table.Current <- read.csv("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/WPBR/NewData/Final Scripts/FIGURES/08_Model_Current-Summary_Table.csv")
Table.Current %>% ggplot( aes( x=id, y = mean.x)) + geom_bar(stat = "identity")