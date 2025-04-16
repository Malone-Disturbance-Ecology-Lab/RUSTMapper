# STEP6 CHANGE over time:
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
# Import data: 
data.dir <- "/Volumes/MaloneLab/Research/RUSTMAPPER"
figure.dir <- "/Users/sm3466/Dropbox (YSE)/Research/RUSTMapper/FIGURES"

setwd(data.dir)

ensemble.est <- rast( paste(data.dir,"/Ensemble_1980-2099_EST.tif", sep=""))
ensemble.inv <- rast( paste(data.dir,"/Ensemble_1980-2099_INV.tif", sep=""))
load( "Final_ShapeFiles.RDATA")



frac <- function( col){
  tot = length(col)
  high = length(col[col > 0.50])
  high.frac =(high/tot)*100
  return( high.frac )
}

frac.df <- function(sf, raster){
  df <- raster %>% terra::crop(sf) %>% 
    terra::mask(sf) %>% as.data.frame() %>% 
    na.omit %>% summarise_all(frac) %>% 
    t %>% as.data.frame()
  
}

# Create stacks. for species and regions
wp.inv <- frac.df( sf = wp, raster = ensemble.inv) %>% rename( southern.H5=V1)
wp.gb.inv <- frac.df( sf = wp.gb, raster = ensemble.inv) %>% rename( wp.gb=V1)
wp.sr.inv <- frac.df( sf = wp.sr, raster = ensemble.inv) %>% rename( wp.sr=V1)
wp.ssn.inv <- frac.df( sf = wp.ssn, raster = ensemble.inv) %>% rename( wp.ssn=V1)
wp.sw.inv <- frac.df( sf = wp.sw, raster = ensemble.inv) %>% rename( wp.sw=V1)

PIBA.inv <- frac.df( sf = PIBA, raster = ensemble.inv) %>% rename( PIBA=V1)
PIAL.inv <- frac.df( sf = PIAL, raster = ensemble.inv) %>% rename( PIAL=V1)
PIFL.inv <- frac.df( sf = PIFL, raster = ensemble.inv) %>% rename( PIFL=V1)
PILO.inv <- frac.df( sf = PILO, raster = ensemble.inv) %>% rename( PILO=V1)
PIAR.inv <- frac.df( sf = PIAR, raster = ensemble.inv) %>% rename( PIAR=V1)
PIST.inv <- frac.df( sf = PIST, raster = ensemble.inv) %>% rename( PIST=V1)

# ESTABLISHED MODELS
wp.cce.est <- frac.df( sf = wp.cce, raster = ensemble.est) %>% rename( wp.cce=V1)
wp.gb.est <- frac.df( sf = wp.gb, raster = ensemble.est) %>% rename( wp.gb=V1)
wp.gye.est <- frac.df( sf = wp.gye, raster = ensemble.est) %>% rename( wp.gye=V1)
wp.pnw.est <- frac.df( sf = wp.pnw, raster = ensemble.est) %>% rename( wp.pnw=V1)
wp.sr.est <- frac.df( sf = wp.sr, raster = ensemble.est) %>% rename( wp.sr=V1)
wp.ssn.est <- frac.df( sf = wp.ssn, raster = ensemble.est) %>% rename( wp.ssn=V1)
wp.sw.est <- frac.df( sf = wp.sw, raster = ensemble.est) %>% rename( wp.sw=V1)

PIBA.est <- frac.df( sf = PIBA, raster = ensemble.est) %>% rename( PIBA=V1)
PIAL.est <- frac.df( sf = PIAL, raster = ensemble.est) %>% rename( PIAL=V1)
PIFL.est <- frac.df( sf = PIFL, raster = ensemble.est) %>% rename( PIFL=V1)
PILO.est <- frac.df( sf = PILO, raster = ensemble.est) %>% rename( PILO=V1)
PIAR.est <- frac.df( sf = PIAR, raster = ensemble.est) %>% rename( PIAR=V1)
PIST.est <- frac.df( sf = PIST, raster = ensemble.est) %>% rename( PIST=V1)

PIBAs.est <- frac.df( sf = PIBA.s, raster = ensemble.est) %>% rename( PIBAs=V1)
PIALs.est <- frac.df( sf = PIAL.s, raster = ensemble.est) %>% rename( PIALs=V1)
PIFLs.est <- frac.df( sf = PIFL.s, raster = ensemble.est) %>% rename( PIFLs=V1)
PILOs.est <- frac.df( sf = PILO.s, raster = ensemble.est) %>% rename( PILOs=V1)
PIARs.est <- frac.df( sf = PIAR.s, raster = ensemble.est) %>% rename( PIARs=V1)
PISTs.est <- frac.df( sf = PIST.s, raster = ensemble.est) %>% rename( PISTs=V1)
wp.est <- frac.df( sf = wp, raster = ensemble.est) %>% rename( wp=V1)
southern.H5.est <- frac.df( sf = wp.s, raster = ensemble.est) %>% rename( southern.H5=V1)


Frac.High.inv <- cbind( wp.inv,
                        wp.gb.inv ,
                        wp.sr.inv ,
                        wp.ssn.inv, 
                        wp.sw.inv ,
                        
                        PIBA.inv ,
                        PIAL.inv ,
                        PIFL.inv ,
                        PILO.inv ,
                        PIAR.inv ,
                        PIST.inv ) %>% as.data.frame

Frac.High.est <- c( wp.est,
                    southern.H5.est, 
                    wp.cce.est, 
                    wp.gb.est ,
                    wp.gye.est, 
                    wp.pnw.est, 
                    wp.sr.est ,
                    wp.ssn.est, 
                    wp.sw.est ,
                    
                    PIBA.est ,
                    PIAL.est ,
                    PIFL.est ,
                    PILO.est ,
                    PIAR.est ,
                    PIST.est,
                    
                    PIBAs.est,
                    PIALs.est,
                    PIFLs.est ,
                    PILOs.est,
                    PIARs.est,
                    PISTs.est)%>% as.data.frame

Frac.High <- cbind(Frac.High.inv, Frac.High.est)

Frac.High$Year <-time(ensemble.est) %>% as.numeric

write.csv( Frac.High,paste(data.dir,'/Frac.High.csv', sep=""))

# High Frac Tables:

regional.table <- function(df ){
  
  t.mean <- df %>% summarise_all(.fun=mean) %>% t %>% as.data.frame
  t.max <-df %>% summarise_all(.fun=max) %>% t %>% as.data.frame
  t.min <-df %>% summarise_all(.fun=min) %>% t %>% as.data.frame
  t.sd <-df %>% summarise_all(.fun=sd) %>% t %>% as.data.frame
  
  Frac.High.df <- cbind(t.min, t.max, t.mean, t.sd)
  names(Frac.High.df) <- cbind( 'Minimum', 'Maximum', 'Mean', "SD")
  
  Frac.High.df2 <- Frac.High.df %>% round(2) %>% 
    mutate(Region = rownames(Frac.High.df))
  
  return(Frac.High.df2 )
}

FH.current_table.inv <- regional.table(df=Frac.High.inv[1:44, ]) %>% rename( Minimum.inv = Minimum,
                                                                             Maximum.inv = Maximum,
                                                                             Mean.inv = Mean, SD.inv = SD)

FH.current_table.est <- regional.table(df=Frac.High.est[1:44,]) %>% rename( Minimum.est = Minimum,
                                                                            Maximum.est = Maximum,
                                                                            Mean.est = Mean)

FH.table.current <- FH.current_table.inv %>% dplyr::full_join(FH.current_table.est, by='Region')


FH.future_table.inv <- regional.table(df=Frac.High.inv[45:114, ]) %>% rename( Minimum.inv = Minimum,
                                                                              Maximum.inv = Maximum,
                                                                              Mean.inv = Mean, SD.inv = SD)

FH.future_table.est <- regional.table(df=Frac.High.est[45:114, ]) %>% rename( Minimum.est = Minimum,
                                                                              Maximum.est = Maximum,
                                                                              Mean.est = Mean)

FH.table.future <- FH.future_table.inv %>% dplyr::full_join(FH.future_table.est, by='Region')

# Order:
FH.table.future$Order[FH.table.future$Region == 'wp']<- 1
FH.table.future$Order[FH.table.future$Region == 'southern.H5']<- 2
FH.table.future$Order[FH.table.future$Region == 'wp.pnw']<- 3
FH.table.future$Order[FH.table.future$Region == 'wp.cce']<- 4
FH.table.future$Order[FH.table.future$Region == 'wp.gye']<- 5
FH.table.future$Order[FH.table.future$Region == 'wp.sr']<- 6
FH.table.future$Order[FH.table.future$Region == 'wp.gb']<- 7
FH.table.future$Order[FH.table.future$Region == 'wp.sw']<- 8
FH.table.future$Order[FH.table.future$Region == 'wp.ssn']<- 9
FH.table.future$Order[FH.table.future$Region == 'PIALs']<- 10
FH.table.future$Order[FH.table.future$Region == 'PIBAs']<- 11
FH.table.future$Order[FH.table.future$Region == 'PIFLs']<- 12
FH.table.future$Order[FH.table.future$Region == 'PILOs']<- 13
FH.table.future$Order[FH.table.future$Region == 'PISTs']<- 14
FH.table.future$Order[FH.table.future$Region == 'PIARs']<- 15
FH.table.future$Order[FH.table.future$Region == 'PIAL']<- 16
FH.table.future$Order[FH.table.future$Region == 'PIBA']<- 17
FH.table.future$Order[FH.table.future$Region == 'PIFL']<- 18
FH.table.future$Order[FH.table.future$Region == 'PILO']<- 19
FH.table.future$Order[FH.table.future$Region == 'PIST']<- 20
FH.table.future$Order[FH.table.future$Region == 'PIAR']<- 21

FH.table.current$Order[FH.table.current$Region == 'wp']<- 1
FH.table.current$Order[FH.table.current$Region == 'southern.H5']<- 2
FH.table.current$Order[FH.table.current$Region == 'wp.pnw']<- 3
FH.table.current$Order[FH.table.current$Region == 'wp.cce']<- 4
FH.table.current$Order[FH.table.current$Region == 'wp.gye']<- 5
FH.table.current$Order[FH.table.current$Region == 'wp.sr']<- 6
FH.table.current$Order[FH.table.current$Region == 'wp.gb']<- 7
FH.table.current$Order[FH.table.current$Region == 'wp.sw']<- 8
FH.table.current$Order[FH.table.current$Region == 'wp.ssn']<- 9
FH.table.current$Order[FH.table.current$Region == 'PIALs']<- 10
FH.table.current$Order[FH.table.current$Region == 'PIBAs']<- 11
FH.table.current$Order[FH.table.current$Region == 'PIFLs']<- 12
FH.table.current$Order[FH.table.current$Region == 'PILOs']<- 13
FH.table.current$Order[FH.table.current$Region == 'PISTs']<- 14
FH.table.current$Order[FH.table.current$Region == 'PIARs']<- 15
FH.table.current$Order[FH.table.current$Region == 'PIAL']<- 16
FH.table.current$Order[FH.table.current$Region == 'PIBA']<- 17
FH.table.current$Order[FH.table.current$Region == 'PIFL']<- 18
FH.table.current$Order[FH.table.current$Region == 'PILO']<- 19
FH.table.current$Order[FH.table.current$Region == 'PIST']<- 20
FH.table.current$Order[FH.table.current$Region == 'PIAR']<- 21

FH.table.future <- arrange(FH.table.future, Order)
FH.table.current <- arrange(FH.table.current, Order)


write.csv(FH.table.current,paste(data.dir,'/08_FH.table-current.csv' , sep="")  )
write.csv(FH.table.future, paste(data.dir,'/08_FH.table-future.csv' , sep="") )

# Percentage of H5 with FH 2030 - 2099

Frac.High.inv$southern.H5[45:114] %>% mean
Frac.High.inv$southern.H5[45:114] %>% min
Frac.High.inv$southern.H5[45:114] %>% max

# ELEVATED RISK: ####

Elev.risk.inv <- ensemble.inv
Elev.risk.inv[ensemble.inv < 0.5] <- 0
Elev.risk.inv[ensemble.inv >= 0.5] <- 1

Elev.risk.inv.total <- Elev.risk.inv %>% sum()

Elev.risk.INV <- ggplot() + geom_sf(data=AOI, fill="black",  col="black",alpha=0.75) + 
  geom_spatraster( data=Elev.risk.inv.total, aes(fill = sum)) + 
  scale_fill_gradient2(
    low = "yellow", 
    mid = "cyan", 
    high = "magenta", 
    midpoint =40, 
    na.value = "transparent", name="Elevated Risk (Years)",
    limits=c(0,114)) + theme(text = element_text(size = 15),
                             panel.background = element_rect(fill='transparent'))


Elev.risk.est <- ensemble.est
Elev.risk.est[ensemble.est < 0.5] <- 0
Elev.risk.est[ensemble.est >= 0.5] <- 1

Elev.risk.est.total <- sum(Elev.risk.est, na.rm=T)

Elev.risk.inv.total %>% plot
Elev.risk.inv.total <- sum(Elev.risk.inv, na.rm=T)
Elev.risk.inv.total %>% plot

Elev.risk.EST <-ggplot() + geom_sf(data=AOI, fill="black",  col="black",alpha=0.75) + geom_spatraster( data=Elev.risk.est.total, aes(fill = sum)) + scale_fill_gradient2(
  low = "yellow", 
  mid = "cyan", 
  high = "magenta", 
  midpoint =40, 
  na.value = "transparent", name="Elevated Risk (Years)",
  limits=c(0,114)) + theme(text = element_text(size = 15),
                           panel.background = element_rect(fill='transparent'))


setwd(figure.dir)
png("08_HighFraction_FIGURE_ElevRisk.png", width = 600, height = 300)

ggpubr::ggarrange(Elev.risk.INV, Elev.risk.EST, ncol=2, nrow=1, 
                  labels=c("a", "b"),
                  common.legend = TRUE, legend="top")
dev.off()

