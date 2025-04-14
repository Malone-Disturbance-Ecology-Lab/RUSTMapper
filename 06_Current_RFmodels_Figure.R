# Current Spatial Figures :

library(ggplot2)
library(terra)
library(sf)
library(dplyr)
library(AOI)

rm(list=ls())

data.dir <- "/Volumes/MaloneLab/Research/RUSTMAPPER"
figure.dir <- "/Users/sm3466/Dropbox (YSE)/Research/RUSTMapper/FIGURES"
setwd(data.dir)

ensemble.est <- rast( paste(data.dir,"/Ensemble_1980-2099_EST.tif", sep=""))
ensemble.inv <- rast( paste(data.dir,"/Ensemble_1980-2099_INV.tif", sep=""))

AOI = AOI::aoi_get(state = c("CO", "WA", "OR", "CA", "MT", "ID", "UT", "AZ", "NV", "WY", "NM")) %>% st_transform(crs(ensemble.inv)) %>% st_as_sf

# Mean conditions 1980-2023:
ensemble.est.mean <- ensemble.est[[1:44]] %>% mean(na.rm=T)
ensemble.inv.mean <- ensemble.inv[[1:44]] %>% mean(na.rm=T)
ensemble.est.stdev <- ensemble.est[[1:44]] %>% stdev(na.rm=T)
ensemble.inv.stdev <- ensemble.inv[[1:44]] %>% stdev(na.rm=T)

ensemble.est.sum <- c(ensemble.est.mean, 
                   ensemble.est.stdev)

ensemble.inv.sum <- c(ensemble.inv.mean, 
                      ensemble.inv.stdev)

names( ensemble.est.sum ) <- c("mean","sd")
names( ensemble.inv.sum ) <- c("mean","sd")

val.df <- c(ensemble.est.mean,  ensemble.inv.mean) %>% as.data.frame
names(val.df) <- c("est", "inv")
lm(val.df$est ~ val.df$inv) %>% summary

# Description of 2020 results:
df.est <- as.data.frame(ensemble.est.sum) %>% na.omit()
df.inv <- as.data.frame(ensemble.inv.sum) %>% na.omit()

# Percentage of the landscape with a greather than 50% probaility:
P.50 <- function(x ){
  p50 <- (length( x[ x > 0.5 ])/ length( x))*100
  return(p50)
}

P.50(df.est$mean)
P.50(df.inv$mean)

summary(df.est$sd)
summary(df.inv$sd)
# 
library(tidyterra)
library(ggplot2)

setwd(figure.dir)
 
# ensemble.est.sum ; ensemble.inv.sum 
plot.2020.inv <- ggplot() + geom_sf(data=AOI, fill="black", colour="gray")+
  geom_spatraster(data = ensemble.inv.sum$mean, na.rm = TRUE) + 
  scale_fill_whitebox_c(palette = "bl_yl_rd", limits=c(0.1, 0.75))  + 
  labs(fill ="P(WPBR)") +
  theme(text = element_text(size = 18), 
        axis.text.x=element_text(angle=90),
        plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.35),
        panel.background = element_rect(fill='transparent')) 

plot.2020.est <- ggplot() + geom_sf(data=AOI, fill="black", colour="gray")+
  geom_spatraster(data = ensemble.est.sum$mean, na.rm = TRUE) + 
  scale_fill_whitebox_c(palette = "bl_yl_rd", limits=c(0.1, 0.75))  + 
  labs(fill ="P(WPBR)")  +
  theme(text = element_text(size = 18), 
        axis.text.x=element_text(angle=90),
        plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.35),
        panel.background = element_rect(fill='transparent'))

plot.2020.sd.inv <- ggplot() + geom_sf(data=AOI, fill="black", colour="gray")+
  geom_spatraster(data = ensemble.inv.sum$sd, na.rm = TRUE) + 
  scale_fill_whitebox_c(palette = "bl_yl_rd", limits=c(0, 0.2))  + 
  labs(fill =expression("Deviation")) +
  theme(text = element_text(size = 18), 
        axis.text.x=element_text(angle=90),
        plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.35),
        panel.background = element_rect(fill='transparent'))

plot.2020.sd.est <- ggplot() + geom_sf(data=AOI, fill="black", colour="gray")+
  geom_spatraster(data = ensemble.est.sum$sd, na.rm = TRUE) + 
  scale_fill_whitebox_c(palette = "bl_yl_rd", limits=c(0, 0.2))  + 
  labs(fill =expression("Deviation")) +
  theme(text = element_text(size = 18), 
        axis.text.x=element_text(angle=90),
        plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.35),
        panel.background = element_rect(fill='transparent'))

library(ggpubr)

P_WPBR_2020 <- ggpubr::ggarrange(plot.2020.inv , plot.2020.est, ncol=2, nrow=1, common.legend = TRUE, labels=c("a", "b") , legend="right")

P_WPBR_2020_SD <- ggpubr::ggarrange(plot.2020.sd.inv, plot.2020.sd.est, ncol=2,nrow=1, common.legend = TRUE, labels=c("c", "d") , legend="right")


# This is the first figure mapped plot used in the Analyses:
png("07_CURRENT_RFMODELS_Figure3.png",  width = 480, height = 520)
ggpubr::ggarrange(P_WPBR_2020,
P_WPBR_2020_SD, ncol=1, nrow=2)
dev.off()

# Overlap:
ensemble <- c(ensemble.est.sum, ensemble.inv.sum)
names(ensemble) <- c('est.mean', "est.sd", 'inv.mean', "inv.sd")

# Description of 2020 results:
df.ensemble <- as.data.frame(ensemble) %>% na.omit()
cor(df.ensemble$est.mean,df.ensemble$inv.mean, method = "pearson")
summary(lm(df.ensemble$est.mean~df.ensemble$inv.mean))

#EOF

