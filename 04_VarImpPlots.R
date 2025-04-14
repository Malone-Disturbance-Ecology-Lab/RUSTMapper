# STEP5_VarImpPlots:

library(randomForest) # models
library(tidyverse)
library(gridExtra)


rm(list=ls())

data.dir <- "/Volumes/MaloneLab/Research/RUSTMAPPER"
figure.dir <- "/Users/sm3466/Dropbox (YSE)/Research/RUSTMapper/FIGURES"
setwd(data.dir)

# Load the data layers:
load("RF_MODELFIT_Results_DAYMET.RDATA")

VarImpPlot_GINI <- function( model){
  imp.df <- varImpPlot(model) %>% as.data.frame() 
  rnames <- rownames( imp.df) 
  try(rnames[ rnames == "StreamDen"] <- "STREAM_DEN", silent =T)
  rownames( imp.df) <- rnames
  imp.df$varnames <- rownames(imp.df) # row names to column
  rownames(imp.df) <- NULL  

  imp.plot <- ggplot(imp.df, aes(x=reorder(varnames,  MeanDecreaseGini), y= MeanDecreaseGini)) + 
    geom_point() + xlab("") + 
    geom_segment(aes(x=varnames,xend=varnames,y=0,yend= MeanDecreaseGini)) +   
    coord_flip() +  theme(text = element_text(size = 20),
                          panel.background = element_rect(fill='transparent')) 
  
  return(imp.plot)
}
VarImpPlot_MDA <- function( model){
  imp.df <- varImpPlot(model) %>% as.data.frame() 
  rnames <- rownames( imp.df) 
  try(rnames[ rnames == "StreamDen"] <- "STREAM_DEN", silent =T)
  rownames( imp.df) <- rnames
  imp.df$varnames <- rownames(imp.df) # row names to column
  rownames(imp.df) <- NULL  
  
  imp.plot <-ggplot(imp.df, aes(x=reorder(varnames,  MeanDecreaseAccuracy), y= MeanDecreaseAccuracy)) + 
    geom_point() + xlab("") + 
    geom_segment(aes(x=varnames,xend=varnames,y=0,yend= MeanDecreaseAccuracy)) + 
    coord_flip()+  theme(text = element_text(size = 20),
                         panel.background = element_rect(fill='transparent')) 
  
  return(imp.plot)
}

# 5 Years
imp.5.invading.gini <- VarImpPlot_GINI( rf.invading.5)
imp.5.invading.mda <- VarImpPlot_MDA( rf.invading.5)

imp.5.established.gini <- VarImpPlot_GINI( rf.established.5)
imp.5.established.mda <- VarImpPlot_MDA( rf.established.5)

# 10 Years
imp.10.invading.gini <- VarImpPlot_GINI( rf.invading.10)
imp.10.invading.mda <- VarImpPlot_MDA( rf.invading.10)

imp.10.established.gini <- VarImpPlot_GINI( rf.established.10)
imp.10.established.mda <- VarImpPlot_MDA( rf.established.10)

# 20 Years
imp.20.invading.gini <- VarImpPlot_GINI( rf.invading.20)
imp.20.invading.mda <- VarImpPlot_MDA( rf.invading.20)

imp.20.established.gini <- VarImpPlot_GINI( rf.established.20)
imp.20.established.mda <- VarImpPlot_MDA( rf.established.20)


library(ggpubr)

setwd(figure.dir)

png("VarImp_GINI_Plots_102024.png", width = 800, height = 900)

ggarrange( imp.5.invading.gini, imp.5.established.gini,
           imp.10.invading.gini, imp.10.established.gini,
           imp.20.invading.gini, imp.20.established.gini,
           nrow=3, ncol=2, 
           labels =c("a", "b", "c", "d", "e", "f"),
           font.label = list(size = 22) )

dev.off()

setwd(figure.dir)

png("VarImp_MDA_Plots_102024.png", width = 800, height = 900)

ggarrange( imp.5.invading.mda, imp.5.established.mda,
              imp.10.invading.mda, imp.10.established.mda,
              imp.20.invading.mda, imp.20.established.mda,
           nrow=3, ncol=2, 
           labels =c("a", "b", "c", "d", "e", "f"),
           font.label = list(size = 22) )

dev.off()

