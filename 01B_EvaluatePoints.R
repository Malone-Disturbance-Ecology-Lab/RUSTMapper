# Look at the frequency of points and make Hex plots:
rm(list=ls())

library(sf)
library(AOI)
library(terra)
library(tidyverse)

data.dir <- "/Volumes/MaloneLab/Research/RUSTMAPPER"
figure.dir <- "/Users/sm3466/Dropbox (YSE)/Research/RUSTMapper/FIGURES"
setwd(data.dir)

AOI = AOI::aoi_get(state = c("CO", "WA", "OR", "CA", "MT", "ID", "UT", "AZ", "NV", "WY", "NM"))
AOI <- st_transform( AOI, 4326)


load( "Final_ShapeFiles.RDATA")
load(file='WPBR_plots_DAYMET.RDATA')

data.10years$YEAR_LAST %>% summary


length(data.10years$TYPE2[ data.10years$TYPE2 == "plot"])/ length(data.10years$TYPE2)
# Create the point files we will count later:

invasion.regions <- invasion.regions %>% st_transform( crs((AOI)))

wpbr.est.1 <- wpbr %>% filter(WPBR_stat == "Established" & RUST == 1) %>%  st_transform( crs((AOI)))
wpbr.est.0 <- wpbr %>% filter(WPBR_stat == "Established" & RUST == 0) %>%  st_transform( crs((AOI)))

wpbr.inv.1 <- wpbr %>% filter(WPBR_stat == "Invading" & RUST == 1) %>%  st_transform( crs((AOI))) %>% st_crop(invasion.regions)

wpbr.inv.0 <- wpbr %>% filter(WPBR_stat == "Invading" & RUST == 0) %>%  st_transform( crs((AOI))) %>% st_crop(invasion.regions)

rm(wpbr)

# Create a new wpbr file: Because some invading are in the North
wpbr <- rbind( wpbr.est.1, wpbr.est.0,wpbr.inv.1 , wpbr.inv.0)

# The buffer file:
wpbr.1000 <- st_buffer( wpbr, 1000)

# Count points in the buffers:
wpbr.1000$est.1_count <- lengths(st_intersects(wpbr.1000, wpbr.est.1))
wpbr.1000$est.0_count <- lengths(st_intersects(wpbr.1000, wpbr.est.0))
wpbr.1000$inv.1_count <- lengths(st_intersects(wpbr.1000, wpbr.inv.1))
wpbr.1000$inv.0_count <- lengths(st_intersects(wpbr.1000, wpbr.inv.0))


wpbr.1000 <- wpbr.1000 %>% mutate( est.freq = est.1_count/(est.1_count + est.0_count),
                                   inv.freq = inv.1_count/(inv.1_count + inv.0_count) )

wpbr.1000$inv.freq %>% summary
wpbr.1000$est.freq %>% summary

# Hex plots....
hex.grid = st_make_grid(wpbr, square = FALSE) 
hex.grid_sf = st_sf(hex.grid) %>%
  mutate(grid_id = 1:length(lengths(hex.grid))) # add grid ID
hex.grid_sf1 <- st_join(hex.grid_sf,wpbr.1000 )
hex.grid_sf1$inv.freq <- as.numeric(hex.grid_sf1$inv.freq)


hex.inv <- hex.grid_sf1  %>% filter(WPBR_stat == "Invading",!is.na(inv.freq)) %>% ggplot() + geom_sf( aes(fill= inv.freq)) + scale_fill_continuous(type = "viridis") + geom_sf(data = AOI, fill=NA)+ labs(fill = expression("WPBR"[INV])) + theme(text = element_text(size = 20), panel.background = element_rect(fill='transparent'))


hex.est <-hex.grid_sf1  %>%filter(!is.na(est.freq)) %>%  ggplot() + geom_sf( aes(fill= est.freq))+  scale_fill_continuous(type = "viridis") + geom_sf(data = AOI, fill=NA) + labs(fill = expression("WPBR"[EST])) + theme(text = element_text(size = 20), panel.background = element_rect(fill='transparent'))


den.plot <- ggplot() +
  geom_density(data=hex.grid_sf1 ,
               aes(x=inv.freq, after_stat(scaled)),
               color="cyan4") + ylab('Density') + 
    geom_density(data=hex.grid_sf1, aes(x=est.freq, after_stat(scaled)), color="black") + 
                   ylab('Density')  +
    theme(text = element_text(size = 20), panel.background = element_rect(fill='transparent')) +
  xlab("Observed P(WPBR)") + 
  annotate('text', x = 0.2, y = 0.9, label = "Invading", size=5, color="cyan4") +
  annotate('text', x = 0.2, y = 0.8, label = "Established", size=5, col="black")


library(ggpubr)

setwd(figure.dir)

png("01b_EvaluatePoints.png", width = 400, height = 400)
ggarrange(den.plot) 
dev.off()



# Summarise the data used to build models:

# sample size:

# Invading
length(wpbr.inv.1$FID_) + length(wpbr.inv.0$FID_) #Total points
length(wpbr.inv.1$FID_)/(length(wpbr.inv.1$FID_) + length(wpbr.inv.0$FID_)) # Total fraction of WPBR
wpbr.1000 %>% filter(WPBR_stat == "Invading", !is.na(inv.freq)) %>% summary

# Establishing

length(wpbr.est.1$FID_) + length(wpbr.est.0$FID_) #Total points
length(wpbr.est.1$FID_)/(length(wpbr.est.1$FID_) + length(wpbr.est.0$FID_)) # Total fraction of WPBR
wpbr.1000 %>% filter(WPBR_stat == "Established", !is.na(est.freq)) %>% summary

library(dplyr)
# Summarize the source of plt data:

wpbr.df <- wpbr %>% as.data.frame


wpbr.df2 <- wpbr.df %>% mutate(Count = 1) %>% as.data.frame %>% select(TYPE2, CONTACT, Count) %>% reframe(.by= c(TYPE2, CONTACT), Count = sum(Count))

wpbr.obs <- wpbr.df2 %>% filter( TYPE2 =='obs')
100 - sum(wpbr.obs$Count)/ sum(wpbr.df2$Count) *100
 
