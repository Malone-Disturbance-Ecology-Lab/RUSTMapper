library(randomForest)
library(tidyverse)
library(ggplot2)
library(gridExtra)

rm(list=ls())

load("/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/Final Scripts/RF_MODELFIT_Results_DAYMET.RDATA")

model.pred <- function(data){
  
  data.pred <- data.frame(
    est.5 = predict(data, object=rf.established.5, type="prob")[,2],
    est.10 = predict(data, object=rf.established.10, type="prob")[,2],
    est.20 = predict(data, object=rf.established.20, type="prob")[,2],
    
    inv.5 = predict(data, object=rf.invading.5, type="prob")[,2],
    inv.10 = predict(data, object=rf.invading.10, type="prob")[,2],
    inv.20 = predict(data, object=rf.invading.20, type="prob")[,2] )
  
  data.pred <- data.pred %>% mutate(EST = rowMeans( data.pred[,c('est.5', 'est.10', 'est.20')] )) %>% mutate(INV = rowMeans( data.pred[,c('inv.5', 'inv.10', 'inv.20')] ))
  
  data$Established <- data.pred$EST
  data$Invading <- data.pred$INV
  return(data)
}

# Missing TPI(-147 210)  and TRI (0, 338)!!!!
stream  <- data.frame( StreamDen= seq(0,9,1 )) 

data.max <- data.5years %>% na.omit %>% group_by(H_Zone) %>% summarise(
  Type = "max",
  TRI = max( TRI, na.rm=T),
  TPI = max( TPI, na.rm=T),
  PRCP_Spring = max( PRCP_Spring, na.rm=T),
  PRCP_Summer = max( PRCP_Summer, na.rm=T),
  PRCP_Fall = max( PRCP_Fall, na.rm=T), 
  
  Tmin_Spring = max( Tmin_Spring, na.rm=T),
  Tmin_Summer = max( Tmin_Summer, na.rm=T),
  Tmin_Fall = max( Tmin_Fall, na.rm=T), 
  
  Tmax_Spring = max( Tmax_Spring, na.rm=T),
  Tmax_Summer = max( Tmax_Summer, na.rm=T),
  Tmax_Fall = max( Tmax_Fall, na.rm=T), 
  
  VPD_Spring = max( VPD_Spring, na.rm=T),
  VPD_Summer = max( VPD_Summer, na.rm=T),
  VPD_Fall = max( VPD_Fall, na.rm=T), 
  
  VPDmax_Spring = max( VPDmax_Spring, na.rm=T),
  VPDmax_Summer = max( VPDmax_Summer, na.rm=T),
  VPDmax_Fall = max( VPDmax_Fall, na.rm=T), 
  
  RH_Spring = max( RH_Spring, na.rm=T),
  RH_Summer = max( RH_Summer, na.rm=T),
  RH_Fall = max( RH_Fall, na.rm=T)) %>% na.omit() %>% merge(stream)

data.min <- data.5years %>% na.omit %>% group_by(H_Zone) %>% summarise(
  Type = "min",
  TRI = min( TRI, na.rm=T),
  TPI = min( TPI, na.rm=T),
  PRCP_Spring = min( PRCP_Spring, na.rm=T),
  PRCP_Summer = min( PRCP_Summer, na.rm=T),
  PRCP_Fall = min( PRCP_Fall, na.rm=T), 
  
  Tmin_Spring = min( Tmin_Spring, na.rm=T),
  Tmin_Summer = min( Tmin_Summer, na.rm=T),
  Tmin_Fall = min( Tmin_Fall, na.rm=T), 
  
  Tmax_Spring = min( Tmax_Spring, na.rm=T),
  Tmax_Summer = min( Tmax_Summer, na.rm=T),
  Tmax_Fall = min( Tmax_Fall, na.rm=T), 
  
  VPD_Spring = min( VPD_Spring, na.rm=T),
  VPD_Summer = min( VPD_Summer, na.rm=T),
  VPD_Fall = min( VPD_Fall, na.rm=T), 
  
  VPDmax_Spring = min( VPDmax_Spring, na.rm=T),
  VPDmax_Summer = min( VPDmax_Summer, na.rm=T),
  VPDmax_Fall = min( VPDmax_Fall, na.rm=T), 
  
  RH_Spring = min( RH_Spring, na.rm=T),
  RH_Summer = min( RH_Summer, na.rm=T),
  RH_Fall = min( RH_Fall, na.rm=T)) %>% na.omit() %>% merge(stream)

data.mean <- data.5years %>% na.omit %>% group_by(H_Zone) %>% summarise(
  Type = "mean",
  TRI = mean( TRI, na.rm=T),
  TPI = mean( TPI, na.rm=T),
  PRCP_Spring = mean( PRCP_Spring, na.rm=T),
  PRCP_Summer = mean( PRCP_Summer, na.rm=T),
  PRCP_Fall = mean( PRCP_Fall, na.rm=T), 
  
  Tmin_Spring = mean( Tmin_Spring, na.rm=T),
  Tmin_Summer = mean( Tmin_Summer, na.rm=T),
  Tmin_Fall = mean( Tmin_Fall, na.rm=T), 
  
  Tmax_Spring = mean( Tmax_Spring, na.rm=T),
  Tmax_Summer = mean( Tmax_Summer, na.rm=T),
  Tmax_Fall = mean( Tmax_Fall, na.rm=T), 
  
  VPD_Spring = mean( VPD_Spring, na.rm=T),
  VPD_Summer = mean( VPD_Summer, na.rm=T),
  VPD_Fall = mean( VPD_Fall, na.rm=T), 
  
  VPDmax_Spring = mean( VPDmax_Spring, na.rm=T),
  VPDmax_Summer = mean( VPDmax_Summer, na.rm=T),
  VPDmax_Fall = mean( VPDmax_Fall, na.rm=T), 
  
  RH_Spring = mean( RH_Spring, na.rm=T),
  RH_Summer = mean( RH_Summer, na.rm=T),
  RH_Fall = mean( RH_Fall, na.rm=T)) %>% na.omit() %>% merge(stream) 

Sensitivity <- rbind( data.min, data.mean, data.max) %>% filter( H_Zone != 0)

model.pred <- function(data){
  
  data.pred <- data.frame(
    est.5 = predict(data, object=rf.established.5, type="prob")[,2],
    est.10 = predict(data, object=rf.established.10, type="prob")[,2],
    est.20 = predict(data, object=rf.established.20, type="prob")[,2],
    
    inv.5 = predict(data, object=rf.invading.5, type="prob")[,2],
    inv.10 = predict(data, object=rf.invading.10, type="prob")[,2],
    inv.20 = predict(data, object=rf.invading.20, type="prob")[,2] )
  
  data.pred <- data.pred %>% mutate(EST = rowMeans( data.pred[,c('est.5', 'est.10', 'est.20')] )) %>% mutate(INV = rowMeans( data.pred[,c('inv.5', 'inv.10', 'inv.20')] ))
  
  data$Established <- data.pred$EST
  data$Invading <- data.pred$INV
  return(data)
}
# Create the sequence for the numeric variables in the model:



TPI <- data.frame(TPI = seq( -147, 210, 30))
TRI <-data.frame(TRI = seq( 0, 338, 20))
PRCP <- data.frame( PRCP_Spring = seq( 0, 300, 20), PRCP_Summer = seq( 0, 300, 20), PRCP_Fall = seq( 0, 300, 20))


Temp <- data.frame( Tmin_Spring = seq( -10, 40, 10), 
                    Tmin_Summer = seq( -10, 40, 10), 
                    Tmin_Fall = seq( -10, 40, 10),
                    Tmax_Spring = seq( -10, 40, 10), 
                    Tmax_Summer = seq( -10, 40, 10), 
                    Tmax_Fall = seq( -10, 40, 10))

VPD <- data.frame( VPD_Spring = seq( 200, 1500, 50), 
                   VPD_Summer = seq( 200, 1500, 50), 
                   VPD_Fall = seq( 200, 1500, 50),
                   VPDmax_Spring = seq( 700, 2700, 75),
                   VPDmax_Summer = seq( 700, 2700, 75), 
                   VPDmax_Fall = seq( 700, 2700, 75))

RH <- data.frame( RH_Spring = seq( 20, 90, 10), RH_Summer = seq( 20, 90, 10), RH_Fall = seq( 20, 90, 10))

Sensitivity.TRI <- Sensitivity %>% dplyr::select(-c(TRI) ) %>% merge(TRI, all=T ) %>% model.pred()
Sensitivity.TPI <- Sensitivity %>% dplyr::select(-c(TPI) ) %>% merge(TPI, all=T ) %>% model.pred()

Sensitivity.PRCP <- Sensitivity %>% dplyr::select(-c(PRCP_Spring, PRCP_Summer, PRCP_Fall) ) %>% merge(PRCP, all=T ) %>% model.pred() %>%  mutate(PRCP = PRCP_Spring)

Sensitivity.Temp <- Sensitivity %>% dplyr::select(
  -c(Tmin_Spring, Tmin_Summer, Tmin_Fall,
     Tmax_Spring, Tmax_Summer, Tmax_Fall) ) %>% merge(Temp, all=T ) %>% model.pred() %>%  mutate(TEMP = Tmin_Spring)

Sensitivity.VPD <- Sensitivity %>% dplyr::select(-c(VPD_Spring, VPD_Summer, VPD_Fall,VPDmax_Spring, VPDmax_Summer, VPDmax_Fall) ) %>% merge(VPD, all=T ) %>% model.pred()%>%  mutate(VPD = VPD_Spring,
                                                                                                                                                                              VPDmax = VPDmax_Spring)

Sensitivity.RH <- Sensitivity %>% dplyr::select(-c(RH_Spring, RH_Summer, RH_Fall) ) %>% merge(RH, all=T ) %>% model.pred() %>%  mutate(RH = RH_Spring) %>% na.omit

Sensitivity <- Sensitivity %>% model.pred() %>% na.omit

# Vizualize the data: 

library(ggplot2)

label = as_labeller(c( '3' = 'Hardiness Zone 3', '4' = 'Hardiness Zone 4','5' = 'Hardiness Zone 5',
                       '6' = 'Hardiness Zone 6','7' = 'Hardiness Zone 7','8' = 'Hardiness Zone 8'))

# Article Figure: ####
Plot.Sensitivity.HZONE <- Sensitivity %>% ggplot() + 
  geom_line( aes( y = Established, x= StreamDen, linetype= Type)) + 
  facet_wrap(vars(H_Zone), nrow=4, ncol=2, labeller = label )+ geom_line( data = Sensitivity, aes( y = Invading, x= StreamDen, linetype= Type), col="grey50") + 
  facet_wrap(vars(H_Zone), nrow=4, ncol=2 , labeller = label) + xlab("Stream Density") + ylab("P(WPBR)") + 
  guides(linetype=guide_legend(title="Condition")) +theme_bw() + theme(legend.position = "top") 


PLOT.H_ZONE <- Sensitivity  %>% ggplot() + geom_smooth(method = 'loess',aes( y = Established, x= H_Zone), col="Black") + 
  geom_smooth(method = 'loess',aes( y = Invading, x= H_Zone), col="Grey50", linetype="dashed") + 
  xlab("H_ZONE") + ylab("P(WPBR)") + theme_bw()+ylim(0.2,0.8) +
  theme(text = element_text(size = 20))


PLOT.StreamDen <- Sensitivity  %>% ggplot() + geom_smooth(method = 'loess',aes( y = Established, x= StreamDen), col="Black") + geom_smooth(method = 'loess',aes( y = Invading, x= StreamDen), col="Grey50", linetype="dashed") + 
  xlab("STREAM_DEN") + ylab("P(WPBR)") + theme_bw()+ylim(0.2,0.8)+ 
  annotate( geom="text", x=2.8, y= 0.65, label="Established", col="black", size=8) +
  annotate( geom="text", x=2, y= 0.25, label="Invading", col="grey50", size=8) +
  theme(text = element_text(size = 20))

PLOT.TRI <- Sensitivity.TRI  %>% ggplot() + 
  geom_smooth(method = 'loess',aes( y = Established, x= TRI), col="black", linetype="solid") + 
  geom_smooth(method = 'loess',aes( y = Invading, x= TRI), col="grey50", linetype="dashed") + 
  xlab("TRI") + ylab("P(WPBR)") +ylim(0.2,0.8)+ theme_bw() +
  theme(text = element_text(size = 20))

PLOT.TPI <- Sensitivity.TPI  %>% ggplot() + 
  geom_smooth(method = 'loess',aes( y = Established, x= TPI), col="black", linetype="solid") + 
  geom_smooth(method = 'loess',aes( y = Invading, x= TPI), col="grey50", linetype="dashed") + 
  xlab("TPI") + ylab("P(WPBR)") +ylim(0.2,0.8)+ theme_bw() +
  theme(text = element_text(size = 20))

PLOT.PRCP <- Sensitivity.PRCP  %>% ggplot() + 
  geom_smooth(method = 'loess',aes( y = Established, x= PRCP), col="black", linetype="solid") + 
   geom_smooth(method = 'loess',aes( y = Invading, x= PRCP), col="grey50", linetype="dashed") + 
   xlab("PRCP") + ylab("P(WPBR)") +ylim(0.2,0.8)+ theme_bw() +
  theme(text = element_text(size = 20))

PLOT.RH <- Sensitivity.RH  %>% ggplot() + geom_smooth(method = 'loess',aes( y = Established, x= RH), col="black") + 
  geom_smooth(method = 'loess',aes( y = Invading, x= RH), col="grey50", linetype="dashed") +
  xlab("RH")+ ylab("P(WPBR)") +ylim(0.2,0.8) + theme_bw() +
  theme(text = element_text(size = 20))

PLOT.Tmin <- Sensitivity.Temp  %>% ggplot() + 
  geom_smooth(method = 'loess',aes( y = Established, x= TEMP), col="black") + 
  geom_smooth(method = 'loess',aes( y = Invading, x= TEMP), col="grey50", linetype="dashed") +
  xlab("Tmin") + ylab("P(WPBR)") + ylim(0.2,0.8) + theme_bw() +
  theme(text = element_text(size = 20))


PLOT.Tmax <- Sensitivity.Temp  %>% ggplot() + 
  geom_smooth(method = 'loess',aes( y = Established, x= TEMP), col="black") + 
  geom_smooth(method = 'loess',aes( y = Invading, x= TEMP), col="grey50", linetype="dashed") + 
  xlab("Tmax") +ylim(0.2,0.8) + ylab("P(WPBR)") + theme_bw()+
  theme(text = element_text(size = 20))

PLOT.VPDmax <- Sensitivity.VPD  %>% ggplot() + geom_smooth(method = 'loess',aes( y = Established, x= VPDmax), col="black") + 
  geom_smooth(method = 'loess',aes( y = Invading, x= VPDmax), col="grey50", linetype="dashed")+
  xlab("VPDmax") + ylab("P(WPBR)") +
  ylim(0.2,0.8) + theme_bw()+
  theme(text = element_text(size = 20))

PLOT.VPD <- Sensitivity.VPD  %>% ggplot() + 
  geom_smooth(method = 'loess',aes( y = Established, x= VPD), col="black") + 
  geom_smooth(method = 'loess',aes( y = Invading, x= VPD_Summer), col="grey50", linetype="dashed") +
  xlab("VPD") + ylab("P(WPBR)") + 
  ylim(0.2,0.8) + theme_bw()+
  theme(text = element_text(size = 20))

library(ggpubr)
final.plot <- ggarrange(
             PLOT.StreamDen,
             PLOT.H_ZONE,
             PLOT.TPI,
             PLOT.TRI,
             PLOT.PRCP,
             PLOT.Tmax,
             PLOT.Tmin,
             PLOT.VPD,
             PLOT.VPDmax,
             PLOT.RH, ncol=2,nrow=5, labels =c("a", "b", "c", "d", "e", "f", "g", "h", "i","j"))

setwd("/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/Final Scripts/FIGURES")
png("04_SensitivityAnalysis_FIGURE 2.png", width = 500, height = 1000)
final.plot
dev.off()

# Scientific Data Figures: ####


Plot.Sensitivity.HZONE <- Sensitivity %>% ggplot() + 
  geom_line( aes( y = Established, x= StreamDen, linetype= Type)) + 
  facet_wrap(vars(H_Zone), nrow=4, ncol=2, labeller = label )+ geom_line( data = Sensitivity, aes( y = Invading, x= StreamDen, linetype= Type), col="cyan4") + 
  facet_wrap(vars(H_Zone), nrow=4, ncol=2 , labeller = label) + xlab("Stream Density") + ylab("P(WPBR)") + 
  guides(linetype=guide_legend(title="Condition")) +theme_bw() + theme(legend.position = "top") 


PLOT.H_ZONE <- Sensitivity  %>% ggplot() + geom_smooth(method = 'loess',aes( y = Established, x= H_Zone), col="Black") + 
  geom_smooth(method = 'loess',aes( y = Invading, x= H_Zone), col="cyan4", linetype="dashed") + 
  xlab("H_ZONE") + ylab("P(WPBR)") + theme_bw()+ylim(0.2,0.8) +
  theme(text = element_text(size = 20))


PLOT.StreamDen <- Sensitivity  %>% ggplot() + geom_smooth(method = 'loess',aes( y = Established, x= StreamDen), col="Black") + geom_smooth(method = 'loess',aes( y = Invading, x= StreamDen), col="cyan4", linetype="dashed") + 
  xlab("STREAM_DEN") + ylab("P(WPBR)") + theme_bw()+ylim(0.2,0.8)+ 
  annotate( geom="text", x=2.8, y= 0.65, label="Established", col="black", size=8) +
  annotate( geom="text", x=2, y= 0.25, label="Invading", col="cyan4", size=8) +
  theme(text = element_text(size = 20))

PLOT.TRI <- Sensitivity.TRI  %>% ggplot() + 
  geom_smooth(method = 'loess',aes( y = Established, x= TRI), col="black", linetype="solid") + 
  geom_smooth(method = 'loess',aes( y = Invading, x= TRI), col="cyan4", linetype="dashed") + 
  xlab("TRI") + ylab("P(WPBR)") +ylim(0.2,0.8)+ theme_bw() +
  theme(text = element_text(size = 20))

PLOT.TPI <- Sensitivity.TPI  %>% ggplot() + 
  geom_smooth(method = 'loess',aes( y = Established, x= TPI), col="black", linetype="solid") + 
  geom_smooth(method = 'loess',aes( y = Invading, x= TPI), col="cyan4", linetype="dashed") + 
  xlab("TPI") + ylab("P(WPBR)") +ylim(0.2,0.8)+ theme_bw() +
  theme(text = element_text(size = 20))

PLOT.PRCP <- Sensitivity.PRCP  %>% ggplot() + 
  geom_smooth(method = 'loess',aes( y = Established, x= PRCP), col="black", linetype="solid") + 
  geom_smooth(method = 'loess',aes( y = Invading, x= PRCP), col="cyan4", linetype="dashed") + 
  xlab("PRCP") + ylab("P(WPBR)") +ylim(0.2,0.8)+ theme_bw() +
  theme(text = element_text(size = 20))

PLOT.RH <- Sensitivity.RH  %>% ggplot() + geom_smooth(method = 'loess',aes( y = Established, x= RH), col="black") + 
  geom_smooth(method = 'loess',aes( y = Invading, x= RH), col="cyan4", linetype="dashed") +
  xlab("RH")+ ylab("P(WPBR)") +ylim(0.2,0.8) + theme_bw() +
  theme(text = element_text(size = 20))

PLOT.Tmin <- Sensitivity.Temp  %>% ggplot() + 
  geom_smooth(method = 'loess',aes( y = Established, x= TEMP), col="black") + 
  geom_smooth(method = 'loess',aes( y = Invading, x= TEMP), col="cyan4", linetype="dashed") +
  xlab("Tmin") + ylab("P(WPBR)") + ylim(0.2,0.8) + theme_bw() +
  theme(text = element_text(size = 20))


PLOT.Tmax <- Sensitivity.Temp  %>% ggplot() + 
  geom_smooth(method = 'loess',aes( y = Established, x= TEMP), col="black") + 
  geom_smooth(method = 'loess',aes( y = Invading, x= TEMP), col="cyan4", linetype="dashed") + 
  xlab("Tmax") +ylim(0.2,0.8) + ylab("P(WPBR)") + theme_bw()+
  theme(text = element_text(size = 20))

PLOT.VPDmax <- Sensitivity.VPD  %>% ggplot() + geom_smooth(method = 'loess',aes( y = Established, x= VPDmax), col="black") + 
  geom_smooth(method = 'loess',aes( y = Invading, x= VPDmax), col="cyan4", linetype="dashed")+
  xlab("VPDmax") + ylab("P(WPBR)") +
  ylim(0.2,0.8) + theme_bw()+
  theme(text = element_text(size = 20))

PLOT.VPD <- Sensitivity.VPD  %>% ggplot() + 
  geom_smooth(method = 'loess',aes( y = Established, x= VPD), col="black") + 
  geom_smooth(method = 'loess',aes( y = Invading, x= VPD_Summer), col="cyan4", linetype="dashed") +
  xlab("VPD") + ylab("P(WPBR)") + 
  ylim(0.2,0.8) + theme_bw()+
  theme(text = element_text(size = 20))

library(ggpubr)
final.plot <- ggarrange(
  PLOT.StreamDen,
  PLOT.H_ZONE,
  PLOT.TPI,
  PLOT.TRI,
  PLOT.PRCP,
  PLOT.Tmax,
  PLOT.Tmin,
  PLOT.VPD,
  PLOT.VPDmax,
  PLOT.RH, ncol=2,nrow=5, labels =c("a", "b", "c", "d", "e", "f", "g", "h", "i","j"))

setwd("/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/Final Scripts/FIGURES")
png("04_SensitivityAnalysis_FIGURE8_ScientificData.png", width = 500, height = 1000)
final.plot
dev.off()

