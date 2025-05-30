# Maps for WPBR studies:

rm(list=ls())

#  https://pjbartlein.github.io/REarthSysSci/rasterVis01.html
library(AOI)
library(sf)
library(terra)
library(ggplot2)
library(tidyterra)
library(ggpubr)

data.dir <- "/Volumes/MaloneLab/Research/RUSTMAPPER"
figure.dir <- "/Users/sm3466/Dropbox (YSE)/Research/RUSTMapper/FIGURES"
setwd(data.dir)

AOI = AOI::aoi_get(state = c("CO", "WA", "OR", "CA", "MT", "ID", "UT", "AZ", "NV", "WY", "NM"))
AOI <- st_transform( AOI, 4326)


load( "Final_ShapeFiles.RDATA")
load(file='WPBR_plots_DAYMET.RDATA')


# Regional Map
regions.plot <- ggplot( ) +   geom_sf(data=AOI , fill="white", color="black", lwd=0.5) +
  theme(panel.background = element_rect(fill='transparent'), 
        axis.text.x = element_text(angle = 90),
        text = element_text(size = 20)) +
  geom_sf(data=wp.pnw, fill="lightblue", alpha = 0.85, color= "lightblue") +
  geom_sf(data=wp.gye, fill="grey", alpha = 0.85, color= "grey") +
  geom_sf(data=wp.cce, fill="black", alpha = 0.5, color= "black") +
  geom_sf(data=wp.gb, fill="darkgreen", alpha = 0.5, color= "darkgreen") +
  geom_sf(data=wp.sr, fill="goldenrod", alpha = 0.5, color="goldenrod")  + 
  geom_sf(data=wp.ssn, fill="darkblue", alpha = 0.5, color="darkblue")  + 
  geom_sf(data=wp.sw, fill="magenta", alpha = 0.5, color= "magenta") +
  xlab("") + ylab("")

# Species range Map:
Species.plot <- ggplot( ) + geom_sf(data=AOI, fill="white", color="black", lwd=0.5) +
  geom_sf(data=PIAL, fill="cyan",color=NA, alpha = 0.5) + 
  geom_sf(data=PIBA, fill="cyan4",color=NA, alpha = 1) + 
  geom_sf(data=PIFL, fill="blue4",color=NA, alpha = 0.5) + 
  geom_sf(data=PIST, fill="goldenrod",color=NA, alpha = 1) +
  geom_sf(data=PIAR, fill="red",color=NA, alpha = 0.75) +
  geom_sf(data=PILO, fill="green",color=NA, alpha = 1) +
  xlab("") + ylab("") +
  theme(panel.background = element_rect(fill='transparent'), 
        axis.text.x = element_text(angle = 90),
        text = element_text(size = 20)) 

# Plot Data Map

plots.plot <- ggplot( ) + geom_sf(data=AOI, fill="transparent", colour='black', lwd=0.5) +
  geom_sf(data=wpbr$geometry[wpbr$WPBR_stat == 'Established'], color='black', size=0.5) +
  geom_sf(data=wpbr$geometry[wpbr$WPBR_stat == 'Invading'], color='gray', size=0.5) + xlab("") + ylab("")+
  theme(panel.background = element_rect(fill='transparent'), 
        axis.text.x = element_text(angle = 90),
        text = element_text(size = 20))

library(ggpubr)

setwd(figure.dir)
png("04_Figure1_Map.png", width =700, height = 300)
ggarrange(regions.plot, plots.plot, Species.plot,
          nrow= 1, ncol= 3, labels =c("a", "b", "c") ,vjust=1)
dev.off()

png("04_Regions_Map.png", width =300, height = 300)
ggarrange(regions.plot,
          nrow= 1, ncol= 1 ,vjust=1)
dev.off()

png("04_Species_Map.png", width =300, height = 300)
ggarrange(Species.plot,
          nrow= 1, ncol= 1 ,vjust=1)
dev.off()

png("04_DataPoints_Map.png", width =300, height = 300)
ggarrange(plots.plot,
          nrow= 1, ncol= 1 ,vjust=1)
dev.off()

save( Species.plot,regions.plot, file=paste(data.dir,"maps.Rdata", sep="/"))

# Supplemental Figure for the ranges of projection:
invasion.regions <- st_transform(invasion.regions, crs(wp)) 
invasion.wp <-wp %>% st_intersection(invasion.regions)

projection.plot <- ggplot( ) + geom_sf(data=AOI, fill="white", color="black", lwd=0.5) +
  geom_sf(data=wp, fill="black",color=NA, alpha = 0.5) + 
  geom_sf(data=invasion.wp, fill="cyan4",color="cyan4", alpha = 0.5) +
  xlab("") + ylab("") +
  theme(panel.background = element_rect(fill='transparent'), 
        text = element_text(size = 20)) 


png("04_FigureS1_Map.png", width =500, height = 500)
ggarrange(projection.plot,
          nrow= 1, ncol= 1)
dev.off()


data.plot <- ggplot( ) + geom_sf(data=AOI, fill="white", color="black", lwd=0.5) +
  geom_sf(data=wp, fill="black",color=NA, alpha = 0.35) + 
  geom_sf(data=wpbr$geometry[wpbr$WPBR_stat == 'Established'], color='black', size=0.15) +
  geom_sf(data=wpbr$geometry[wpbr$WPBR_stat == 'Invading'], color='cyan4', size=0.15)+
  xlab("") + ylab("")+
  theme(panel.background = element_rect(fill='transparent'), 
        text = element_text(size = 20)) 

png("04_Figure1_Map_DataPaper.png", width =500, height = 500)
ggarrange(data.plot ,
          nrow= 1, ncol= 1)
dev.off()

wpbr$WPBR_stat %>% length
wpbr$WPBR_stat[wpbr$WPBR_stat == 'Established'] %>% length
wpbr$WPBR_stat[wpbr$WPBR_stat == 'Invading'] %>% length


(wpbr$WPBR_stat[wpbr$WPBR_stat == 'Invading' & wpbr$RUST ==1] %>% 
    length / wpbr$WPBR_stat[wpbr$WPBR_stat == 'Invading'] %>% length) *100

(wpbr$WPBR_stat[wpbr$WPBR_stat == 'Invading' & wpbr$RUST ==0] %>% 
    length / wpbr$WPBR_stat[wpbr$WPBR_stat == 'Invading'] %>% length) *100

(wpbr$WPBR_stat[wpbr$WPBR_stat == 'Established' & wpbr$RUST ==1] %>% 
    length / wpbr$WPBR_stat[wpbr$WPBR_stat == 'Established'] %>% length) *100

(wpbr$WPBR_stat[wpbr$WPBR_stat == 'Established' & wpbr$RUST ==0] %>% 
    length / wpbr$WPBR_stat[wpbr$WPBR_stat == 'Established'] %>% length) *100


