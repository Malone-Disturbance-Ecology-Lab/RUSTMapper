rm(list=ls())

library(viridis)
library(AOI)
library(sf)
library(RColorBrewer)
library(terra)
library(ggplot2)
library(tidyterra)
library(egg) # adds tags to facets
library(gridExtra)
library("cowplot")
library(ggpubr)


# Import dataframes: 
data.dir <- "/Volumes/MaloneLab/Research/RUSTMAPPER"
figure.dir <- "/Users/sm3466/Dropbox (YSE)/Research/RUSTMapper/FIGURES"
setwd(data.dir)

# Import maps made in 04_Figure_MAPS
load( file=paste(data.dir,"maps.Rdata", sep="/"))


ensemble.est <- rast(  paste(data.dir,"/Ensemble_1980-2099_EST.tif", sep=""))
ensemble.inv <- rast(paste(data.dir,"/Ensemble_1980-2099_INV.tif",sep=""))

load("Final_ShapeFiles.RDATA")
load('CCTRENDS_Regions_Species.RDATA')

# Future summary:
ensemble.inv[[45:114]] %>% range
# Mean increase:
1-ensemble.inv[[1:44]] %>% terra::app( fun = mean) %>% global(na.rm=T)/ensemble.inv[[45:114]] %>% terra::app( fun = mean) %>% global(na.rm=T) 

# Min increase:
1-(ensemble.inv[[1:44]] %>% terra::app( fun = min) %>% global(na.rm=T, mean)/ensemble.inv[[45:114]] %>% terra::app( fun = min) %>% global(na.rm=T, mean) ) 

ensemble.est[[1:44]] %>% terra::app( fun = mean) %>% global(na.rm=T)
ensemble.est[[45:114]] %>% terra::app( fun = mean) %>% global(na.rm=T)

ensemble.est[[45:114]] %>% range
1-(ensemble.est[[1:44]] %>% terra::app( fun = mean) %>% global(na.rm=T)/ensemble.est[[45:114]] %>% terra::app( fun = mean) %>% global(na.rm=T) )

inv.delta <- ((ensemble.inv[[1:44]] %>% mean - ensemble.inv[[45:114]] %>% mean)/ensemble.inv[[1:44]] %>% mean ) *100
inv.delta %>% global('mean', na.rm=T)

est.delta <- ((ensemble.est[[1:44]] %>% mean - ensemble.est[[45:114]] %>% mean)/ensemble.est[[1:44]] %>% mean ) *100
est.delta %>% global('mean', na.rm=T)

ensemble.inv[[45:114]] %>% range()
ensemble.est[[45:114]] %>% range()

# 
ensemble.est.future.df <- ensemble.est[[45:114]] %>% mean( na.rm = T) %>% as.data.frame() %>% na.omit()

ensemble.inv.future.df <- ensemble.inv[[45:114]] %>% mean( na.rm = T) %>% as.data.frame() %>% na.omit()

length(ensemble.est.future.df$mean[ensemble.est.future.df$mean>0.5 ])/ length(ensemble.est.future.df$mean)*100

length(ensemble.inv.future.df$mean[ensemble.inv.future.df$mean>0.5 ])/ length(ensemble.inv.future.df$mean)*100

ensemble.inv %>% range


ensemble.inv.future <- rast()
ensemble.inv.future$Current <-ensemble.inv[[1:44]] %>% mean( na.rm = T) 
ensemble.inv.future$Future <-ensemble.inv[[45:114]] %>% mean( na.rm = T) 

ensemble.inv.future.df2 <- ensemble.inv.future %>% as.data.frame %>% na.omit()

lm( ensemble.inv.future.df2$Future ~ ensemble.inv.future.df2$Current) %>% summary

# Create Decadal Layers:

#Create decadal summaries
idx <-c(1,1,1,1,1,1,1,1,1,1,
        2,2,2,2,2,2,2,2,2,2,
        3,3,3,3,3,3,3,3,3,3,
        4,4,4,4,4,4,4,4,4,4,
        5,5,5,5,5,5,5,5,5,5,
        6,6,6,6,6,6,6,6,6,6,
        7,7,7,7,7,7,7,7,7,7,
        8,8,8,8,8,8,8,8,8,8,
        9,9,9,9,9,9,9,9,9,9) # Makes an index to summarise by

ensemble.inv.decade <- ensemble.inv[[ time(ensemble.inv) >= 2000 ]] %>% tapp( index=idx, fun=mean, na.rm=T)
names(ensemble.inv.decade) <- terra::time(ensemble.inv.decade) <- c( 2000, 2010, 2030, 2040, 2050, 2060, 2070, 2080, 2090)

ensemble.est.decade <- ensemble.est[[ time(ensemble.est) >= 2000 ]] %>% tapp( index=idx, fun=mean, na.rm=T)
names(ensemble.est.decade) <- terra::time(ensemble.est.decade) <- c( 2000, 2010, 2030, 2040, 2050, 2060, 2070, 2080, 2090)

labels <- c('2010'= 'b    2010-2020', '2030' = 'c    2030-2039', '2040'= 'd    2040-2049', '2050'= 'e    2050-2059', 
            '2060'= 'f    2060-2069', '2070'= 'g    2070-2079', '2080'= 'h    2080-2089', '2090' ='i    2090-2099')

names(ensemble.inv.decade )


setwd(figure.dir)

plot.inv.cc <- ggplot() + geom_sf(data=AOI, fill="black") + 
  geom_spatraster(data = ensemble.inv.decade[[2:9]], na.rm = TRUE) + facet_wrap(~lyr, ncol = 4, nrow=2, labeller = as_labeller(labels)) +
  scale_fill_whitebox_c(palette = "bl_yl_rd", limits=c(0.1, 0.8)) + labs(fill = expression("P(WPBR)"[INV])) +
  theme(text = element_text(size = 20), 
        panel.spacing = unit(0.25, "lines"),
        axis.text.x = element_text(angle = 90),
        panel.background = element_rect(fill='transparent')) 

plot.est.cc <- ggplot() + geom_sf(data=AOI, fill="black") + 
  geom_spatraster(data = ensemble.est.decade[[2:9]], na.rm = TRUE) + facet_wrap(~lyr, ncol = 4, nrow=2, labeller = as_labeller(labels)) +
  scale_fill_whitebox_c(palette = "bl_yl_rd", limits=c(0.1, 0.8)) + labs(fill = expression("P(WPBR)"[EST])) +
  theme(text = element_text(size = 20), 
        panel.spacing = unit(0.25, "lines"),
        axis.text.x = element_text(angle = 90),
        panel.background = element_rect(fill='transparent'))


# Timeseries for whole plots:
cc.trends.regional$wp.est

timeseries.inv <- cc.trends.regional %>% ggplot() + geom_point(aes( x=year , y = sr.inv)) + theme_bw() + ylim( 0,1) + geom_smooth(aes( x= year, y= sr.inv), col="darkgray", method='loess', linetype = "dashed", size = 0.5)+ theme_bw() + ylim( 0.25,0.6) + ylab(expression("P(WPBR)"[INV])) + xlab("") + theme(text = element_text(size = 25), panel.spacing = unit(0.25, "lines"),
               axis.text.x = element_text(angle = 90),panel.background = element_rect(fill='transparent'))


timeseries.est <- cc.trends.regional %>% ggplot() + geom_point(aes( x=year , y = wp.est)) + geom_smooth(aes( x= year, y= wp.est), col="black", method='loess', linetype = "dashed", size = 0.5)+ theme_bw() + ylim( 0.5,0.7)+ ylab(expression("P(WPBR)"[EST])) + xlab("") +
  theme(text = element_text(size = 25), panel.spacing = unit(0.25, "lines"),
        axis.text.x = element_text(angle = 90),panel.background = element_rect(fill='transparent'))


png("07_CCplot_Invades_CCProjections2020-2090_timeseries.png", width = 1000, height = 300)

ggarrange(timeseries.inv, labels="a",  font.label = list(size = 25))

dev.off()

png("07_CCplot_Established_CCProjections2020-2090_timeseries.png", width = 1000, height = 300)

ggarrange(timeseries.est, labels="a",  font.label = list(size = 25))

dev.off()


png("07_CCplot_Invades_CCProjections2020-2090.png", width = 1000, height = 600)

plot.inv.cc

dev.off()

png("07_CCplot_Establishing_CCProjections2020-2090.png", width = 1000, height = 600)
plot.est.cc
dev.off()


# Regional and Species range trends:  #####

# Trends Figures Regional:  cc.trends.regional
Trends.regions.cc.inv.p <- ggplot(data= cc.trends.regional) + 
  geom_smooth(aes( x= year, y= sw.inv), col="magenta", method='loess',linetype = "dashed", size = 1)+ ylim(0.2,0.5) +
  geom_smooth(aes( x= year, y= sr.inv), col="goldenrod",linetype = "dashed", size = 1) +
  geom_smooth(aes( x= year, y= ssn.inv), col="darkblue",linetype = "dashed", size = 1) +
  geom_smooth(aes( x= year, y= gb.inv), col="darkgreen",linetype = "dashed", size = 1) + ylab(expression("P(WPBR)"[INV])) + xlab("") +theme_bw()  +
  theme(text = element_text(size = 20)) + ylim( 0.2, 0.5)
#annotate('text', label="Southern Rockies", col="goldenrod", x=2000, y=0.32, hjust = 0, size=5) +
#annotate('text', label="Southern Sierra Nevada", col="darkblue", x=2000, y=0.3, hjust = 0, size=5)+ 
#annotate('text', label="Great Basin", col="darkgreen", x=2000, y=0.28, hjust = 0, size=5)+
#annotate('text', label="Southwest", col="magenta", x=2000, y=0.26, hjust = 0, size=5)




Trends.regions.cc.est.p <- ggplot(data=cc.trends.regional ) + 
  geom_smooth(aes( x= year, y= sw.est), col="magenta", method='loess', linetype = "dashed", size = 1)+ ylim(0.4,0.8)+
  geom_smooth(aes( x= year, y= sr.est), col="goldenrod", linetype = "dashed", size = 1) +
  geom_smooth(aes( x= year, y= ssn.est), col="darkblue", linetype = "dashed", size = 1) +
  geom_smooth(aes( x= year, y= gb.est), col="darkgreen", linetype = "dashed", size = 1) + 
  geom_smooth(aes( x= year, y= cce.est), col="black", method='loess', linetype = "dashed", size = 1)+
  geom_smooth(aes( x= year, y= pnw.est), col="lightblue", size=1,linetype = "dashed") +
  geom_smooth(aes( x= year, y= gye.est), col="darkgrey", size=1,linetype = "dashed")  + 
  ylab(expression("P(WPBR)"[EST])) + xlab("")  +
  theme_bw() + theme(text = element_text(size = 20)) + ylim( 0.4, 0.8)
#annotate('text', label="Pacific Northwest", col="lightblue", x=1980, y=0.8, hjust = 0, size=6) +
#annotate('text', label="Crown of the Continent", col="black", x=1980, y=0.77, hjust = 0, size=6)+ 
#annotate('text', label="Greater Yellowstone Ecosystem", col="darkgrey", x=1980, y=0.74, hjust = 0, size=6)+
 

Trends.species.cc.inv.p <- ggplot( data= cc.trends.species) + 
  geom_smooth(aes( x= year, y= PIBA.inv), col="cyan4", method='loess', linetype = "dashed", size = 1)+ 
  geom_smooth(aes( x= year, y= PILO.inv), col="green", method='loess', linetype = "dashed", size = 1)+ 
  geom_smooth(aes( x= year, y= PIFL.inv), col="blue4", method='loess', linetype = "dashed", size = 1)+ 
  geom_smooth(aes( x= year, y= PIAR.inv), col="red", method='loess', linetype = "dashed", size = 1)+ 
  geom_smooth(aes( x= year, y= PIAL.inv), col="cyan", method='loess', linetype = "dashed", size = 1)+ 
  geom_smooth(aes( x= year, y= PIST.inv), col="goldenrod", method='loess', linetype = "dashed", size = 1)+
#annotate('text', label="PIBA", col="cyan4", x=2070, y=0.3, hjust = 0, size=5) + 
#annotate('text', label="PIAL", col="cyan", x=2070, y=0.28, hjust = 0, size=5) + 
#annotate('text', label="PIFL2", col="blue4", x=2070, y=0.26, hjust = 0, size=5) +
#annotate('text', label="PILO", col="green", x=2070, y=0.24, hjust = 0, size=5) +
#annotate('text', label="PIST", col="goldenrod", x=2070, y=0.22, hjust = 0, size=5) +
#annotate('text', label="PIAR", col="red", x=2070, y=0.2, hjust = 0, size=5)+
  ylim(0.2,0.5) + xlab("") + ylab(expression("P(WPBR)"[INV])) +theme_bw() +
  theme(text = element_text(size = 20))


Trends.species.cc.est.p <- ggplot( data=cc.trends.species) + 
  geom_smooth(aes( x= year, y= PIBA.est), col="cyan4", method='loess', linetype = "dashed", size = 1)+ 
  geom_smooth(aes( x= year, y= PIAL.est), col="cyan", method='loess', linetype = "dashed", size = 1)+ 
  geom_smooth(aes( x= year, y= PIFL.est), col="blue4", method='loess', linetype = "dashed", size = 1)+ 
  geom_smooth(aes( x= year, y= PILO.est), col="green", method='loess', linetype = "dashed",size = 1) + 
  geom_smooth(aes( x= year, y= PIST.est), col="goldenrod", method='loess', linetype = "dashed", size = 1)+ 
  geom_smooth(aes( x= year, y= PIAR.est), col="red", method='loess', linetype = "dashed", size = 1)+
  ylim(0.4,0.8)  + xlab("") + ylab(expression("P(WPBR)"[EST]))+
#annotate('text', label="PIBA", col="cyan4", x=2050, y=0.44, hjust = 0, size=6) + 
#annotate('text', label="PIAL", col="cyan", x=2050, y=0.42, hjust = 0, size=6) + 
#annotate('text', label="PIFL2", col="blue4", x=2050, y=0.40, hjust = 0, size=6) +
#annotate('text', label="PILO", col="green", x=2070, y=0.44, hjust = 0, size=6) +
#annotate('text', label="PIST", col="goldenrod", x=2070, y=0.42, hjust = 0, size=6) +
#annotate('text', label="PIAR", col="red", x=2070, y=0.40, hjust = 0, size=6)+
  theme_bw() + theme(text = element_text(size = 20))



png("07_CCPLOTS_Trends_Inv_Est.png", width = 1200, height =600)
ggarrange( Species.plot,  Trends.species.cc.inv.p,Trends.species.cc.est.p,
           regions.plot,Trends.regions.cc.inv.p,Trends.regions.cc.est.p,
          ncol=3, nrow=2, labels=c("a", "b", "c", "d", "e", "f"),  font.label = list(size = 25))  
dev.off()

png("07_CCPLOTS_Trends_Inv_Est_Long.png", width = 900, height =950)
ggarrange( regions.plot, Species.plot, 
           Trends.regions.cc.inv.p,Trends.species.cc.inv.p,
           Trends.regions.cc.est.p,Trends.species.cc.est.p,
           ncol=2, nrow=3, labels=c("a", "d", "b", "e","c","f"),
           font.label = list(size = 25))  
dev.off()
