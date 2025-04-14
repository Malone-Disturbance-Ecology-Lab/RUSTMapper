#STEP5_Plots_PartialEffects

library(randomForest) # models
library(ggplot2)
library(ggpubr)

rm(list=ls())

data.dir <- "/Volumes/MaloneLab/Research/RUSTMAPPER"
figure.dir <- "/Users/sm3466/Dropbox (YSE)/Research/RUSTMapper/FIGURES"
setwd(data.dir)

# Load the data layers:

load("RF_MODELFIT_Results_DAYMET.RDATA")

conDen.Plot <- function(df){
  
  p.prcp.1 <- ggplot(df, aes(PRCP_Spring, fill = as.factor(RUST))) + 
    geom_density(position='fill', alpha = 0.5, colour='black') + 
    xlab("PRCP") + labs(fill='WPBR') + ylab('Conditional Density') +
    theme(legend.text=element_text(size=18), 
          axis.title=element_text(size=18)) +
    scale_fill_manual(values=c("white", "black"), labels = c("No RUST", "RUST")) + theme_classic() + annotate("text", x = 60, y = 0.90, label = "Spring", size=8) 
  
  p.prcp.2 <-ggplot(df, aes(PRCP_Summer, fill = as.factor(RUST))) + 
    geom_density(position='fill', alpha = 0.5, colour='black') + 
    xlab("PRCP") + labs(fill='WPBR') + ylab('Conditional Density') +
    theme(legend.text=element_text(size=16), 
          axis.title=element_text(size=18)) + scale_fill_manual(values=c("white", "black"), labels = c("No RUST", "RUST")) + theme_classic() + annotate("text", x = 100, y = 0.90, label = "Summer", size=8) 
  
  p.prcp.3 <-ggplot(df, aes(PRCP_Fall, fill = as.factor(RUST))) + 
    geom_density(position='fill', alpha = 0.5, colour='black') + 
    xlab("PRCP") + labs(fill='WPBR') + ylab('Conditional Density') +
    theme(legend.text=element_text(size=16), 
          axis.title=element_text(size=18)) + scale_fill_manual(values=c("white", "black"), labels = c("No RUST", "RUST")) + theme_classic()+ 
    annotate("text", x = 60, y = 0.90, label = " Fall ", size=8) 
  
  p.tmin.1 <-ggplot(df, aes(Tmin_Spring, fill = as.factor(RUST))) + 
    geom_density(position='fill', alpha = 0.5, colour='black') + 
    xlab("TMIN") + labs(fill='WPBR') + ylab('Conditional Density') +
    theme(legend.text=element_text(size=16), 
          axis.title=element_text(size=18)) + scale_fill_manual(values=c("white", "black"), labels = c("No RUST", "RUST")) + theme_classic()
  
  p.tmin.2 <-ggplot(df, aes(Tmin_Summer, fill = as.factor(RUST))) + 
    geom_density(position='fill', alpha = 0.5, colour='black') + 
    xlab("TMIN") + labs(fill='WPBR') + ylab('Conditional Density') +
    theme(legend.text=element_text(size=16), 
          axis.title=element_text(size=18)) + scale_fill_manual(values=c("white", "black"), labels = c("No RUST", "RUST")) + theme_classic()
  p.tmin.3 <-ggplot(df, aes(Tmin_Fall, fill = as.factor(RUST))) + 
    geom_density(position='fill', alpha = 0.5, colour='black') + 
    xlab("TMIN") + labs(fill='WPBR') + ylab('Conditional Density') +
    theme(legend.text=element_text(size=16), 
          axis.title=element_text(size=18)) + scale_fill_manual(values=c("white", "black"), labels = c("No RUST", "RUST")) + theme_classic()
  p.tmax.1 <-ggplot(df, aes(Tmax_Spring, fill = as.factor(RUST))) + 
    geom_density(position='fill', alpha = 0.5, colour='black') + 
    xlab("TMAX") + labs(fill='WPBR') + ylab('Conditional Density') +
    theme(legend.text=element_text(size=16), 
          axis.title=element_text(size=18)) + scale_fill_manual(values=c("white", "black"), labels = c("No RUST", "RUST")) + theme_classic()
  
  p.tmax.2 <-ggplot(df, aes(Tmax_Summer, fill = as.factor(RUST))) + 
    geom_density(position='fill', alpha = 0.5, colour='black') + 
    xlab("TMAX") + labs(fill='WPBR') + ylab('Conditional Density') +
    theme(legend.text=element_text(size=16), 
          axis.title=element_text(size=18)) + scale_fill_manual(values=c("white", "black"), labels = c("No RUST", "RUST")) + theme_classic()
  p.tmax.3 <-ggplot(df, aes(Tmax_Fall, fill = as.factor(RUST))) + 
    geom_density(position='fill', alpha = 0.5, colour='black') + 
    xlab("TMAX") + labs(fill='WPBR') + ylab('Conditional Density') +
    theme(legend.text=element_text(size=16), 
          axis.title=element_text(size=18)) + scale_fill_manual(values=c("white", "black"), labels = c("No RUST", "RUST")) + theme_classic()
  
  p.vpd.1 <-ggplot(df, aes(VPD_Spring, fill = as.factor(RUST))) + 
    geom_density(position='fill', alpha = 0.5, colour='black') + 
    xlab("VPD") + labs(fill='WPBR') + ylab('Conditional Density') +
    theme(legend.text=element_text(size=16), 
          axis.title=element_text(size=18)) + scale_fill_manual(values=c("white", "black"), labels = c("No RUST", "RUST")) + theme_classic()
  
  p.vpd.2 <-ggplot(df, aes(VPD_Summer, fill = as.factor(RUST))) + 
    geom_density(position='fill', alpha = 0.5, colour='black') + 
    xlab("VPD") + labs(fill='WPBR') + ylab('Conditional Density') +
    theme(legend.text=element_text(size=16), 
          axis.title=element_text(size=18)) + scale_fill_manual(values=c("white", "black"), labels = c("No RUST", "RUST")) + theme_classic()
  
  p.vpd.3 <-ggplot(df, aes(VPD_Fall, fill = as.factor(RUST))) + 
    geom_density(position='fill', alpha = 0.5, colour='black') + 
    xlab("VPD") + labs(fill='WPBR') + ylab('Conditional Density') +
    theme(legend.text=element_text(size=16), 
          axis.title=element_text(size=18)) + scale_fill_manual(values=c("white", "black"), labels = c("No RUST", "RUST")) + theme_classic()
  
  # remove legend
  p.prcp.1 <- p.prcp.1+ theme(legend.position = "none")
  p.prcp.2 <-p.prcp.2+ theme(legend.position = "none") 
  p.prcp.3 <-p.prcp.3+ theme(legend.position = "none")
  p.tmin.1 <- p.tmin.1+ theme(legend.position = "none")
  p.tmin.2 <-p.tmin.2+ theme(legend.position = "none") 
  p.tmin.3 <- p.tmin.3+ theme(legend.position = "none")
  p.tmax.1 <-p.tmax.1+ theme(legend.position = "none")
  p.tmax.2 <-p.tmax.2+ theme(legend.position = "none") 
  p.tmax.3 <- p.tmax.3+ theme(legend.position = "none")
  p.vpd.1 <-p.vpd.1+ theme(legend.position = "none")
  p.vpd.2 <-p.vpd.2+ theme(legend.position = "none") 
  p.vpd.3 <-p.vpd.3+ theme(legend.position = "none") 
  
  
  plot <- ggarrange(
    p.prcp.1,p.prcp.2, p.prcp.3,
    p.tmin.1,p.tmin.2, p.tmin.3,
    p.tmax.1,p.tmax.2, p.tmax.3,
    p.vpd.1,p.vpd.2, p.vpd.3,
    ncol=3,nrow=4, common.legend=TRUE,
    vjust=-1,
    labels= c( "a", "b", "c", 
               "d", "e", "f", 
               "g", "h", "i",
               "j", "k", "l"))
  return(plot)
}
#The position of the seasonal label needs to change
conDen.Plot.est <- function(df){
  
  p.prcp.1 <- ggplot(df, aes(PRCP_Spring, fill = as.factor(RUST))) + 
    geom_density(position='fill', alpha = 0.5, colour='black') + 
    xlab("PRCP") + labs(fill='WPBR') + ylab('Conditional Density') +
    theme(legend.text=element_text(size=18), 
          axis.title=element_text(size=18)) +
    scale_fill_manual(values=c("white", "black"), labels = c("No RUST", "RUST")) + theme_classic() + annotate("text", x = 150, y = 0.90, label = "Spring", size=8) 
  
  p.prcp.2 <-ggplot(df, aes(PRCP_Summer, fill = as.factor(RUST))) + 
    geom_density(position='fill', alpha = 0.5, colour='black') + 
    xlab("PRCP") + labs(fill='WPBR') + ylab('Conditional Density') +
    theme(legend.text=element_text(size=16), 
          axis.title=element_text(size=18)) + scale_fill_manual(values=c("white", "black"), labels = c("No RUST", "RUST")) + theme_classic() + annotate("text", x = 75, y = 0.90, label = "Summer", size=8) 
  
  p.prcp.3 <-ggplot(df, aes(PRCP_Fall, fill = as.factor(RUST))) + 
    geom_density(position='fill', alpha = 0.5, colour='black') + 
    xlab("PRCP") + labs(fill='WPBR') + ylab('Conditional Density') +
    theme(legend.text=element_text(size=16), 
          axis.title=element_text(size=18)) + scale_fill_manual(values=c("white", "black"), labels = c("No RUST", "RUST")) + theme_classic()+ 
    annotate("text", x = 150, y = 0.90, label = " Fall ", size=8) 
  
  p.tmin.1 <-ggplot(df, aes(Tmin_Spring, fill = as.factor(RUST))) + 
    geom_density(position='fill', alpha = 0.5, colour='black') + 
    xlab("TMIN") + labs(fill='WPBR') + ylab('Conditional Density') +
    theme(legend.text=element_text(size=16), 
          axis.title=element_text(size=18)) + scale_fill_manual(values=c("white", "black"), labels = c("No RUST", "RUST")) + theme_classic()
  
  p.tmin.2 <-ggplot(df, aes(Tmin_Summer, fill = as.factor(RUST))) + 
    geom_density(position='fill', alpha = 0.5, colour='black') + 
    xlab("TMIN") + labs(fill='WPBR') + ylab('Conditional Density') +
    theme(legend.text=element_text(size=16), 
          axis.title=element_text(size=18)) + scale_fill_manual(values=c("white", "black"), labels = c("No RUST", "RUST")) + theme_classic()
  p.tmin.3 <-ggplot(df, aes(Tmin_Fall, fill = as.factor(RUST))) + 
    geom_density(position='fill', alpha = 0.5, colour='black') + 
    xlab("TMIN") + labs(fill='WPBR') + ylab('Conditional Density') +
    theme(legend.text=element_text(size=16), 
          axis.title=element_text(size=18)) + scale_fill_manual(values=c("white", "black"), labels = c("No RUST", "RUST")) + theme_classic()
  p.tmax.1 <-ggplot(df, aes(Tmax_Spring, fill = as.factor(RUST))) + 
    geom_density(position='fill', alpha = 0.5, colour='black') + 
    xlab("TMAX") + labs(fill='WPBR') + ylab('Conditional Density') +
    theme(legend.text=element_text(size=16), 
          axis.title=element_text(size=18)) + scale_fill_manual(values=c("white", "black"), labels = c("No RUST", "RUST")) + theme_classic()
  
  p.tmax.2 <-ggplot(df, aes(Tmax_Summer, fill = as.factor(RUST))) + 
    geom_density(position='fill', alpha = 0.5, colour='black') + 
    xlab("TMAX") + labs(fill='WPBR') + ylab('Conditional Density') +
    theme(legend.text=element_text(size=16), 
          axis.title=element_text(size=18)) + scale_fill_manual(values=c("white", "black"), labels = c("No RUST", "RUST")) + theme_classic()
  p.tmax.3 <-ggplot(df, aes(Tmax_Fall, fill = as.factor(RUST))) + 
    geom_density(position='fill', alpha = 0.5, colour='black') + 
    xlab("TMAX") + labs(fill='WPBR') + ylab('Conditional Density') +
    theme(legend.text=element_text(size=16), 
          axis.title=element_text(size=18)) + scale_fill_manual(values=c("white", "black"), labels = c("No RUST", "RUST")) + theme_classic()
  
  p.vpd.1 <-ggplot(df, aes(VPD_Spring, fill = as.factor(RUST))) + 
    geom_density(position='fill', alpha = 0.5, colour='black') + 
    xlab("VPD") + labs(fill='WPBR') + ylab('Conditional Density') +
    theme(legend.text=element_text(size=16), 
          axis.title=element_text(size=18)) + scale_fill_manual(values=c("white", "black"), labels = c("No RUST", "RUST")) + theme_classic()
  
  p.vpd.2 <-ggplot(df, aes(VPD_Summer, fill = as.factor(RUST))) + 
    geom_density(position='fill', alpha = 0.5, colour='black') + 
    xlab("VPD") + labs(fill='WPBR') + ylab('Conditional Density') +
    theme(legend.text=element_text(size=16), 
          axis.title=element_text(size=18)) + scale_fill_manual(values=c("white", "black"), labels = c("No RUST", "RUST")) + theme_classic()
  
  p.vpd.3 <-ggplot(df, aes(VPD_Fall, fill = as.factor(RUST))) + 
    geom_density(position='fill', alpha = 0.5, colour='black') + 
    xlab("VPD") + labs(fill='WPBR') + ylab('Conditional Density') +
    theme(legend.text=element_text(size=16), 
          axis.title=element_text(size=18)) + scale_fill_manual(values=c("white", "black"), labels = c("No RUST", "RUST")) + theme_classic()
  
  # remove legend
  p.prcp.1 <- p.prcp.1+ theme(legend.position = "none")
  p.prcp.2 <-p.prcp.2+ theme(legend.position = "none") 
  p.prcp.3 <-p.prcp.3+ theme(legend.position = "none")
  p.tmin.1 <- p.tmin.1+ theme(legend.position = "none")
  p.tmin.2 <-p.tmin.2+ theme(legend.position = "none") 
  p.tmin.3 <- p.tmin.3+ theme(legend.position = "none")
  p.tmax.1 <-p.tmax.1+ theme(legend.position = "none")
  p.tmax.2 <-p.tmax.2+ theme(legend.position = "none") 
  p.tmax.3 <- p.tmax.3+ theme(legend.position = "none")
  p.vpd.1 <-p.vpd.1+ theme(legend.position = "none")
  p.vpd.2 <-p.vpd.2+ theme(legend.position = "none") 
  p.vpd.3 <-p.vpd.3+ theme(legend.position = "none") 
  
  
  plot <- ggarrange(
    p.prcp.1,p.prcp.2, p.prcp.3,
    p.tmin.1,p.tmin.2, p.tmin.3,
    p.tmax.1,p.tmax.2, p.tmax.3,
    p.vpd.1,p.vpd.2, p.vpd.3,
    ncol=3,nrow=4, common.legend=TRUE,
    vjust=-1,
    labels= c( "a", "b", "c", 
               "d", "e", "f", 
               "g", "h", "i",
               "j", "k", "l"))
  return(plot)
}

#=========================================================================================
plots.inv.5 <- conDen.Plot(df= as.data.frame(invading.5))
plots.inv.10 <- conDen.Plot(df= as.data.frame(invading.10))
plots.inv.20 <- conDen.Plot(df= as.data.frame(invading.20))

plots.est.5 <- conDen.Plot.est(df= as.data.frame(established.5))
plots.est.10 <- conDen.Plot.est(df= as.data.frame(established.10))
plots.est.20 <- conDen.Plot.est(df= as.data.frame(established.20))

setwd(figure.dir)

png("04_ConditionalPlots_Inv_5Y.png", width = 700, height = 600)
plots.inv.5
dev.off()

png("04_ConditionalPlots_Inv_10Y.png", width = 700, height = 600)
plots.inv.10 
dev.off()

png("04_ConditionalPlots_Inv_20Y.png", width = 700, height = 600)
plots.inv.20 
dev.off()

png("04_ConditionalPlots_Est_5Y.png", width = 700, height = 600)
plots.est.5 
dev.off()

png("04_ConditionalPlots_Est_10Y.png", width = 700, height = 600)
plots.est.10 
dev.off()

png("04_ConditionalPlots_Est_20Y.png", width = 700, height = 600)
plots.est.20 
dev.off()



png("04_STREAM_PartialPlots_052023.png", width = 200, height = 200)
cdplot(as.factor(RUST) ~ StreamDen, data=as.data.frame(all.5), col=c("black", "white"), ylab="Conditional Density", 
       main= '', xlab="")
dev.off()

