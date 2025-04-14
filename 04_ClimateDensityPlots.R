# Climate Density Plots:

rm(list=ls())

library(sf)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(tidyverse)
library(gridExtra)
library(plyr)

data.dir <- "/Volumes/MaloneLab/Research/RUSTMAPPER"
figure.dir <- "/Users/sm3466/Dropbox (YSE)/Research/RUSTMapper/FIGURES"
setwd(data.dir)

# Functions:
re_process <- function( data.Y){

   data.Yn <- data.Y %>% filter( RUST < 2) %>% mutate(RUST = as.factor(RUST))
  return(data.Yn)
}
seasonal.means <- function( data.Yn){
  
  mu <- ddply(data.Yn, 'WPBR_stat', summarise, 
              Tmin_Spring=mean(Tmin_Spring, na.rm=T),
              Tmin_Summer=mean(Tmin_Summer, na.rm=T),
              Tmin_Fall=mean(Tmin_Fall, na.rm=T),
              Tmax_Spring=mean(Tmax_Spring, na.rm=T),
              Tmax_Summer=mean(Tmax_Summer, na.rm=T),
              Tmax_Fall=mean(Tmax_Fall, na.rm=T),
              PRCP_Spring=mean(PRCP_Spring, na.rm=T),
              PRCP_Summer=mean(PRCP_Summer, na.rm=T),
              PRCP_Fall=mean(PRCP_Fall, na.rm=T),
              VPD_Spring=mean(VPD_Spring, na.rm=T),
              VPD_Summer=mean(VPD_Summer, na.rm=T),
              VPD_Fall=mean(VPD_Fall, na.rm=T))
  return(mu)
}
climate.density.plots <- function(data.Yn, data.means ){

  p.Tmin_Spring <- ggplot(data.Yn, aes(x=Tmin_Spring, colour=WPBR_stat)) +
    geom_density(aes( y = ..scaled..)) + 
    geom_vline(data=data.means, aes(xintercept= Tmin_Spring, group=WPBR_stat), linetype="dashed",  colour = c("black", "cyan4") ) + 
    theme(legend.position="none", panel.background = element_rect(fill='transparent'), text = element_text(size = 20)) +
    scale_color_manual(values = c("black", "cyan4")) + xlim(-25,0) +  labs(colour = NULL) + ylab("Density")
  
  p.Tmin_Summer <-ggplot(data.Yn, aes(x=Tmin_Summer, color=WPBR_stat)) +
    geom_density(aes( y = ..scaled..)) + geom_vline(data=data.means, aes(xintercept= Tmin_Summer, group=WPBR_stat),  colour = c("black", "cyan4"),
                                linetype="dashed") + theme(legend.position="none", panel.background = element_rect(fill='transparent'), text = element_text(size = 20)) +
    scale_color_manual(values = c("black", "cyan4"))+  xlim(-25,0) +  labs(colour = NULL) + ylab("Density")
  
  p.Tmin_Fall <-ggplot(data.Yn, aes(x=Tmin_Fall, color=WPBR_stat)) +
    geom_density(aes( y = ..scaled..)) + geom_vline(data=data.means, aes(xintercept= Tmin_Fall, group=WPBR_stat),  colour = c("black", "cyan4"),
                                linetype="dashed")+ theme(legend.position="none", panel.background = element_rect(fill='transparent'), text = element_text(size = 20))+
    scale_color_manual(values = c("black", "cyan4"))+ xlim(-25,0) +  labs(colour = NULL) + ylab("Density")
  
  p.Tmax_Spring <-ggplot(data.Yn, aes(x=Tmax_Spring, color=WPBR_stat)) +
    geom_density(aes( y = ..scaled..)) + geom_vline(data=data.means, aes(xintercept= Tmax_Spring,  group=WPBR_stat),  colour = c("black", "cyan4"),linetype="dashed") + 
    theme(legend.position="none", panel.background = element_rect(fill='transparent'), text = element_text(size = 20))+
    scale_color_manual(values = c("black", "cyan4")) + xlim(20,30) + ylab("Density")
  
  p.Tmax_Summer <-ggplot(data.Yn, aes(x=Tmax_Summer, color=WPBR_stat)) +
    geom_density(aes( y = ..scaled..)) + geom_vline(data=data.means, aes(xintercept= Tmax_Summer,  group=WPBR_stat),  colour = c("black", "cyan4"),
                                linetype="dashed")+ theme(legend.position="none", panel.background = element_rect(fill='transparent'), text = element_text(size = 20))+
    scale_color_manual(values = c("black", "cyan4"))+ xlim(20,30) +  labs(colour = NULL) + ylab("Density")
  
  p.Tmax_Fall <-ggplot(data.Yn, aes(x=Tmax_Fall, color=WPBR_stat)) +
    geom_density(aes( y = ..scaled..)) + geom_vline(data=data.means, aes(xintercept= Tmax_Fall, color=WPBR_stat),
                                linetype="dashed")+ theme(legend.position="none", panel.background = element_rect(fill='transparent'), text = element_text(size = 20))+
    scale_color_manual(values = c("black", "cyan4"))+ xlim(20,30) +  labs(colour = NULL) + ylab("Density")
  
  # PRCP
  p.PRCP_Spring <-ggplot(data.Yn, aes(x=PRCP_Spring, color=WPBR_stat)) +
    geom_density(aes( y = ..scaled..)) + geom_vline(data=data.means, aes(xintercept= PRCP_Spring,  group=WPBR_stat),  colour = c("black", "cyan4"),
                                linetype="dashed")+ scale_color_manual(values = c("black", "cyan4")) +
    theme(panel.background = element_rect(fill='transparent'), text = element_text(size = 20)) + xlim(0,300) +  labs(colour = NULL) + ylab("Density")
  
  p.PRCP_Summer <-ggplot(data.Yn, aes(x=PRCP_Summer, color=WPBR_stat)) +
    geom_density(aes( y = ..scaled..)) + geom_vline(data=data.means, aes(xintercept= PRCP_Summer,  group=WPBR_stat),  colour = c("black", "cyan4"),
                                linetype="dashed")+ theme(legend.position="none", panel.background = element_rect(fill='transparent'), text = element_text(size = 20))+
    scale_color_manual(values = c("black", "cyan4"))+ xlim(0,300) +  labs(colour = NULL) + ylab("Density")
  
  p.PRCP_Fall <-ggplot(data.Yn, aes(x=PRCP_Fall, color=WPBR_stat)) +
    geom_density(aes( y = ..scaled..)) + geom_vline(data=data.means, aes(xintercept= PRCP_Fall,  group=WPBR_stat),  colour = c("black", "cyan4"),
                                linetype="dashed")+ theme(legend.position="none", panel.background = element_rect(fill='transparent'), text = element_text(size = 20))+
    scale_color_manual(values = c("black", "cyan4"))+ xlim(0,300) +  labs(colour = NULL) + ylab("Density")
  
  # VPD
  p.VPD_Spring <-ggplot(data.Yn, aes(x=VPD_Spring, color=WPBR_stat)) +
    geom_density(aes( y = ..scaled..)) + geom_vline(data=data.means, aes(xintercept= VPD_Spring,  group=WPBR_stat),  colour = c("black", "cyan4"),
                                linetype="dashed")+ theme(legend.position="none", panel.background = element_rect(fill='transparent'), text = element_text(size = 20))+
    scale_color_manual(values = c("black", "cyan4"))+ xlim(200,1500) +  labs(colour = NULL) + ylab("Density")
  
  p.VPD_Summer <-ggplot(data.Yn, aes(x=VPD_Summer, color=WPBR_stat)) +
    geom_density(aes( y = ..scaled..)) + geom_vline(data=data.means, aes(xintercept= VPD_Summer,  group=WPBR_stat),  colour = c("black", "cyan4"),
                                linetype="dashed")+ theme(legend.position="none", panel.background = element_rect(fill='transparent'), text = element_text(size = 20))+
    scale_color_manual(values = c("black", "cyan4"))+ xlim(200,1500) +  labs(colour = NULL) + ylab("Density")
  
  p.VPD_Fall <-ggplot(data.Yn, aes(x=VPD_Fall, color=WPBR_stat)) +
    geom_density(aes( y = ..scaled..)) + geom_vline(data=data.means, aes(xintercept= VPD_Fall,  group=WPBR_stat),  colour = c("black", "cyan4"),
                                linetype="dashed")+ theme(legend.position="none", panel.background = element_rect(fill='transparent'), text = element_text(size = 20))+
    scale_color_manual(values = c("black", "cyan4"))+ xlim(200,1500) +  labs(colour = NULL) + ylab("Density")
  
  
  
  plot<- ggarrange(
    p.PRCP_Spring,p.PRCP_Summer, p.PRCP_Fall,
    p.Tmin_Spring,p.Tmin_Summer, p.Tmin_Fall,
    p.Tmax_Spring,p.Tmax_Summer, p.Tmax_Fall,
    p.VPD_Spring,p.VPD_Summer, p.VPD_Fall, ncol=3,nrow=4,
    labels= c( "a.", "b.", "c.", 
               "d.", "e.", "f.", 
               "g.", "h.", "i.",
               "j.", "k.", "l."), common.legend=T,font.label = list(size = 16) )
  return(plot)
}

#===========================================================================================
# These files were developed in STEP 1_BuildingDataFiles_040423.R

load("RF_MODELFIT_Results_DAYMET.RDATA")


data.5 <- data.5years
data.10 <- data.10years
data.20 <- data.20years 

data.5n <-data.5 %>%  re_process()
data.5means <-data.5 %>%  re_process() %>% seasonal.means()
plots.5 <- climate.density.plots(data.5n, data.5means)

data.10n <-data.10 %>%  re_process()
data.10means <-data.10 %>%  re_process() %>% seasonal.means()
plots.10 <- climate.density.plots(data.10n, data.10means)

data.20n <-data.20 %>%  re_process()
data.20means <-data.20 %>%  re_process() %>% seasonal.means()
plots.20 <- climate.density.plots(data.20n, data.20means)

setwd(figure.dir)
png("04_Figure_ClimateDensity_5Years.png", width = 900, height = 800)
plots.5
dev.off()

png("04_Figure_ClimateDensity_10Years.png", width = 900, height = 800)
plots.10
dev.off()

png("04_Figure_ClimateDensity_20Years.png", width = 900, height = 800)
plots.20
dev.off()
