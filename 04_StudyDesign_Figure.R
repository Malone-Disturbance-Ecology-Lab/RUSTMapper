# 04_StudyDesign

library(terra)
library(sf)
library(tidyverse)
library(ggplot2)
library(ggpubr)

figure.dir <- "/Users/sm3466/Dropbox (YSE)/Research/RUSTMapper/FIGURES"

data.dir <- "/Volumes/MaloneLab/Research/RUSTMAPPER"
setwd(data.dir)

load(file = 'WPBR_plots_DAYMET.RDATA')
load( "Final_ShapeFiles.RDATA")

AOI = AOI::aoi_get(state = c("CO", "WA", "OR", "CA", "MT", "ID", "UT", "AZ", "NV", "WY", "NM"))
AOI <- st_transform( AOI, 4326)

est.plot.0 <- ggplot( ) + geom_sf(data=AOI, fill="transparent", colour='black', lwd=0.5) +
  geom_sf(data=wpbr$geometry[wpbr$WPBR_stat == 'Established' & wpbr$RUST == 0], color='black', size=0.5, alpha=0.5) + xlab("") + ylab("")+
  theme(panel.background = element_rect(fill='transparent'), 
        axis.text.x = element_text(angle = 90),
        text = element_text(size = 20))

est.plot.1 <- ggplot( ) + geom_sf(data=AOI, fill="transparent", colour='black', lwd=0.5) +
  geom_sf(data=wpbr$geometry[wpbr$WPBR_stat == 'Established' & wpbr$RUST == 1], color='red', size=0.5, alpha=0.5) + xlab("") + ylab("")+
  theme(panel.background = element_rect(fill='transparent'), 
        axis.text.x = element_text(angle = 90),
        text = element_text(size = 20))

inv.plot.0 <- ggplot( ) + geom_sf(data=AOI, fill="transparent", colour='black', lwd=0.5) +
  geom_sf(data=wpbr$geometry[wpbr$WPBR_stat == 'Invading' & wpbr$RUST == 0], color='black', size=0.5, alpha=0.5) + xlab("") + ylab("")+
  theme(panel.background = element_rect(fill='transparent'), 
        axis.text.x = element_text(angle = 90),
        text = element_text(size = 20))

inv.plot.1 <- ggplot( ) + geom_sf(data=AOI, fill="transparent", colour='black', lwd=0.5) +
  geom_sf(data=wpbr$geometry[wpbr$WPBR_stat == 'Invading' & wpbr$RUST == 1], color='red', size=0.5, alpha=0.5) + xlab("") + ylab("")+
  theme(panel.background = element_rect(fill='transparent'), 
        axis.text.x = element_text(angle = 90),
        text = element_text(size = 20))



regions.plot.low <- ggplot( ) +   geom_sf(data=AOI , fill="white", color="black", lwd=0.5) +
  theme(panel.background = element_rect(fill='transparent'), 
        axis.text.x = element_text(angle = 90),
        text = element_text(size = 20)) +
  geom_sf(data=wp.pnw, fill="lightblue", alpha = 0.85, color= "lightblue") +
  geom_sf(data=wp.gye, fill="grey", alpha = 0.85, color= "grey") +
  geom_sf(data=wp.cce, fill="black", alpha = 0.5, color= "black") +
  geom_sf(data=wp.sr, fill="goldenrod", alpha = 0.5, color="goldenrod")  + 
  geom_sf(data=wp.gb, fill="darkgreen", alpha = 0.5, color= "darkgreen") +
  geom_sf(data=wp.ssn, fill="darkblue", alpha = 0.5, color="darkblue")  + 
  geom_sf(data=wp.sw, fill="magenta", alpha = 0.5, color= "magenta") +
  xlab("") + ylab("")

regions.plot.low <- ggplot( ) +   geom_sf(data=AOI , fill="white", color="black", lwd=0.5) +
  theme(panel.background = element_rect(fill='transparent'), 
        axis.text.x = element_text(angle = 90),
        text = element_text(size = 20)) +
  geom_sf(data=wp.gb, fill="darkgreen", alpha = 0.5, color= "darkgreen") +
  geom_sf(data=wp.ssn, fill="darkblue", alpha = 0.5, color="darkblue")  + 
  geom_sf(data=wp.sw, fill="magenta", alpha = 0.5, color= "magenta") +
  geom_sf(data=wp.sr, fill="goldenrod", alpha = 0.5, color="goldenrod") +
  xlab("") + ylab("")

regions.plot.high <- ggplot( ) +   geom_sf(data=AOI , fill="white", color="black", lwd=0.5) +
  theme(panel.background = element_rect(fill='transparent'), 
        axis.text.x = element_text(angle = 90),
        text = element_text(size = 20)) +
  geom_sf(data=wp.pnw, fill="lightblue", alpha = 0.85, color= "lightblue") +
  geom_sf(data=wp.gye, fill="grey", alpha = 0.85, color= "grey") +
  geom_sf(data=wp.cce, fill="black", alpha = 0.5, color= "black") 
  xlab("") + ylab("")
  
  setwd(figure.dir)
  
png("04_StudyDesign.png", width =700, height = 1000)
  
ggpubr::ggarrange( regions.plot.low, regions.plot.high,
                     inv.plot.0 , inv.plot.1,
                     est.plot.0 , est.plot.1,
                     ncol=2, nrow= 3,
                     labels= c("a", "b", "c", "d", "e", "f"),
                   font.label = list(size = 20))

dev.off()

