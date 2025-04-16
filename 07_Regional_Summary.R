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

ensemble.est <- rast(  paste(data.dir,"/Ensemble_1980-2099_EST.tif", sep=""))
ensemble.inv <- rast(paste(data.dir,"/Ensemble_1980-2099_INV.tif",sep=""))

load("Final_ShapeFiles.RDATA")
load('CCTRENDS_Regions_Species.RDATA')

# Future summary:
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

labels <- c('2010'= 'a    2010-2020', '2030' = 'b    2030-2039', '2040'= 'c    2040-2049', '2050'= 'd    2050-2059', 
            '2060'= 'e    2060-2069', '2070'= 'f    2070-2079', '2080'= 'g    2080-2089', '2090' ='h    2090-2099')

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

png("07_CCplot_Invades_CCProjections2020-2090.png", width = 1000, height = 600)
plot.inv.cc 
dev.off()

png("07_CCplot_Establishing_CCProjections2020-2090.png", width = 1000, height = 600)
plot.est.cc
dev.off()


# Trends Figures Regional:  cc.trends.regional
Trends.cc.inv.p <- ggplot(data= cc.trends.regional) + 
  geom_smooth(aes( x= year, y= sw.inv), col="magenta", method='loess')+ ylim(0.2,0.5) +
  geom_smooth(aes( x= year, y= sr.inv), col="goldenrod") +
  geom_smooth(aes( x= year, y= ssn.inv), col="darkblue") +
  geom_smooth(aes( x= year, y= gb.inv), col="darkgreen") + ylab(expression("P(WPBR)"[INV])) + xlab("") +
  annotate('text', label="Southern Rockies", col="goldenrod", x=2000, y=0.32, hjust = 0, size=5) +
  annotate('text', label="Southern Sierra Nevada", col="darkblue", x=2000, y=0.3, hjust = 0, size=5)+ 
  annotate('text', label="Great Basin", col="darkgreen", x=2000, y=0.28, hjust = 0, size=5)+
  annotate('text', label="Southwest", col="magenta", x=2000, y=0.26, hjust = 0, size=5)+theme_bw() + ylim( 0.2, 0.5) +
  theme(text = element_text(size = 20))


Trends.cc.est.p <- ggplot(data=cc.trends.regional ) + 
  geom_smooth(aes( x= year, y= sw.est), col="magenta", method='loess')+ ylim(0.4,0.8)+
  geom_smooth(aes( x= year, y= sr.est), col="goldenrod") +
  geom_smooth(aes( x= year, y= ssn.est), col="darkblue") +
  geom_smooth(aes( x= year, y= gb.est), col="darkgreen") + ylab(expression("P(WPBR)"[EST])) + xlab("")  +
  annotate('text', label="Southern Rockies", col="goldenrod", x=2020, y=0.8, hjust = 0, size=6) +
  annotate('text', label="Southern Sierra Nevada", col="darkblue", x=2020, y=0.77, hjust = 0, size=6)+ 
  annotate('text', label="Great Basin", col="darkgreen", x=2020, y=0.74, hjust = 0, size=6)+
  annotate('text', label="Southwest", col="magenta", x=2020, y=0.71, hjust = 0, size=6)+theme_bw() + theme(text = element_text(size = 20)) + ylim( 0.4, 0.8)


Trends.cc.est.p2 <- ggplot(data=cc.trends.regional ) + 
  geom_smooth(aes( x= year, y= cce.est), col="black", method='loess')+ ylim(0.4,0.8)+
  geom_smooth(aes( x= year, y= pnw.est), col="lightblue") +
  geom_smooth(aes( x= year, y= gye.est), col="grey")  + 
  ylab(expression("P(WPBR)"[EST])) + xlab("")  +
  annotate('text', label="Pacific Northwest", col="lightblue", x=2020, y=0.5, hjust = 0, size=6) +
  annotate('text', label="Crown of the Continent", col="black", x=2020, y=0.47, hjust = 0, size=6)+ 
  annotate('text', label="Greater Yellowstone Ecosystem", col="darkgrey", x=2020, y=0.44, hjust = 0, size=6) +theme_bw() + theme(text = element_text(size = 20)) + ylim( 0.4, 0.8)

Trends.cc.est.pV1 <- ggplot(data=cc.trends.regional ) + 
  geom_smooth(aes( x= year, y= sw.est), col="magenta", method='loess')+ ylim(0.4,0.8)+
  geom_smooth(aes( x= year, y= sr.est), col="goldenrod") +
  geom_smooth(aes( x= year, y= ssn.est), col="darkblue") +
  geom_smooth(aes( x= year, y= gb.est), col="darkgreen") + 
  geom_smooth(aes( x= year, y= cce.est), col="black", method='loess')+
  geom_smooth(aes( x= year, y= pnw.est), col="lightblue") +
  geom_smooth(aes( x= year, y= gye.est), col="darkgrey")  + 
  ylab(expression("P(WPBR)"[EST])) + xlab("")  +
  annotate('text', label="Pacific Northwest", col="lightblue", x=1980, y=0.8, hjust = 0, size=6) +
  annotate('text', label="Crown of the Continent", col="black", x=1980, y=0.77, hjust = 0, size=6)+ 
  annotate('text', label="Greater Yellowstone Ecosystem", col="darkgrey", x=1980, y=0.74, hjust = 0, size=6)+
  theme_bw() + theme(text = element_text(size = 20)) + ylim( 0.4, 0.8)

Trends.species.cc.inv.p <- ggplot( data= cc.trends.species) + 
  geom_smooth(aes( x= year, y= PIBA.inv), col="cyan4", method='loess')+ 
  geom_smooth(aes( x= year, y= PILO.inv), col="green", method='loess')+ 
  geom_smooth(aes( x= year, y= PIFL.inv), col="blue4", method='loess')+ 
  geom_smooth(aes( x= year, y= PIAR.inv), col="red", method='loess')+ 
  geom_smooth(aes( x= year, y= PIAL.inv), col="cyan", method='loess')+ 
  geom_smooth(aes( x= year, y= PIST.inv), col="goldenrod", method='loess')+
  annotate('text', label="PIBA", col="cyan4", x=2070, y=0.3, hjust = 0, size=5) + 
  annotate('text', label="PIAL", col="cyan", x=2070, y=0.28, hjust = 0, size=5) + 
  annotate('text', label="PIFL2", col="blue4", x=2070, y=0.26, hjust = 0, size=5) +
  annotate('text', label="PILO", col="green", x=2070, y=0.24, hjust = 0, size=5) +
  annotate('text', label="PIST", col="goldenrod", x=2070, y=0.22, hjust = 0, size=5) +
  annotate('text', label="PIAR", col="red", x=2070, y=0.2, hjust = 0, size=5)+
  ylim(0.2,0.5) + xlab("") + ylab(expression("P(WPBR)"[INV])) +theme_bw() +
  theme(text = element_text(size = 20))


Trends.species.cc.est.p <- ggplot( data=cc.trends.species) + 
  geom_smooth(aes( x= year, y= PIBA.est), col="cyan4", method='loess')+ 
  geom_smooth(aes( x= year, y= PIAL.est), col="cyan", method='loess')+ 
  geom_smooth(aes( x= year, y= PIFL.est), col="blue4", method='loess')+ 
  geom_smooth(aes( x= year, y= PILO.est), col="green", method='loess')+ 
  geom_smooth(aes( x= year, y= PIST.est), col="goldenrod", method='loess')+ 
  geom_smooth(aes( x= year, y= PIAR.est), col="red", method='loess')+
  ylim(0.4,0.8)  + xlab("") + ylab(expression("P(WPBR)"[EST]))+
  annotate('text', label="PIBA", col="cyan4", x=2050, y=0.44, hjust = 0, size=6) + 
  annotate('text', label="PIAL", col="cyan", x=2050, y=0.42, hjust = 0, size=6) + 
  annotate('text', label="PIFL2", col="blue4", x=2050, y=0.40, hjust = 0, size=6) +
  annotate('text', label="PILO", col="green", x=2070, y=0.44, hjust = 0, size=6) +
  annotate('text', label="PIST", col="goldenrod", x=2070, y=0.42, hjust = 0, size=6) +
  annotate('text', label="PIAR", col="red", x=2070, y=0.40, hjust = 0, size=6)+theme_bw() + theme(text = element_text(size = 20))

Trends.species.cc.est.pV2 <- ggplot( data=cc.trends.species) + 
  geom_smooth(aes( x= year, y= PIBA.est), col="cyan4", method='loess')+ 
  geom_smooth(aes( x= year, y= PIAL.est), col="cyan", method='loess')+ 
  geom_smooth(aes( x= year, y= PIFL.est), col="blue4", method='loess')+ 
  geom_smooth(aes( x= year, y= PILO.est), col="green", method='loess')+ 
  geom_smooth(aes( x= year, y= PIST.est), col="goldenrod", method='loess')+ 
  geom_smooth(aes( x= year, y= PIAR.est), col="red", method='loess')+
  ylim(0.4,0.8)  + xlab("") + ylab(expression("P(WPBR)"[EST]))+
  theme_bw() + theme(text = element_text(size = 20))


png("07_CCPLOTS_Regional Trends_ Inv_Est.png", width = 800, height =550)
ggarrange(Trends.cc.inv.p, Trends.species.cc.inv.p,
          Trends.cc.est.pV1,Trends.species.cc.est.pV2,
          ncol=2, nrow=2, labels=c("a", "b", "c", "d"))  
dev.off()


png("07_CCPLOTS_Regional Trends_ Invading.png", width = 550, height =250)
ggarrange(Trends.cc.inv.p, Trends.species.cc.inv.p,
          ncol=2, labels=c("a", "b"))  
dev.off()


png("07_CCPLOTSRegional Trends_ Establishing.png", width = 450, height =800)
ggarrange(Trends.cc.est.p2 , Trends.cc.est.p, Trends.species.cc.est.p,
          ncol=1, labels=c("a", "b", "c"))  
dev.off()

inv.delta <- ((ensemble.inv[[1:44]] %>% mean - ensemble.inv[[45:114]] %>% mean)/ensemble.inv[[1:44]] %>% mean ) *100
inv.delta %>% global('mean', na.rm=T)

est.delta <- ((ensemble.est[[1:44]] %>% mean - ensemble.est[[45:114]] %>% mean)/ensemble.est[[1:44]] %>% mean ) *100
est.delta %>% global('mean', na.rm=T)

ensemble.inv[[45:114]] %>% range()
ensemble.est[[45:114]] %>% range()