# Functions used in 08_ Climate Summaries. 

# Calculate a summary of the fraction greater than 50% table over time:
regional.summary.Fraction50 <- function (raster, SP){
  
  cc.inv.sr <- terra::crop(rast(raster), SP)  %>% terra::mask( SP)
  cc.inv.sr.df <-as.data.frame(cc.inv.sr) %>% na.omit()
  
  RANGE <- range(cc.inv.sr.df$X2020)
  MEAN.2020 <- mean(cc.inv.sr.df$X2020, na.rm=T)
  
  
  Frac.High.20 <- length(cc.inv.sr.df$X2020[cc.inv.sr.df$X2020 > 0.50]) / length( cc.inv.sr.df$X2020)*100
  Frac.High.30 <- length(cc.inv.sr.df$X2030[cc.inv.sr.df$X2030 > 0.50]) / length( cc.inv.sr.df$X2030)*100
  Frac.High.40 <- length(cc.inv.sr.df$X2040[cc.inv.sr.df$X2040 > 0.50]) / length( cc.inv.sr.df$X2040)*100
  Frac.High.50 <- length(cc.inv.sr.df$X2050[cc.inv.sr.df$X2050 > 0.50]) / length( cc.inv.sr.df$X2050)*100
  Frac.High.60 <- length(cc.inv.sr.df$X2060[cc.inv.sr.df$X2060 > 0.50]) / length( cc.inv.sr.df$X2060)*100
  Frac.High.70 <- length(cc.inv.sr.df$X2070[cc.inv.sr.df$X2070 > 0.50]) / length( cc.inv.sr.df$X2070)*100
  Frac.High.80 <- length(cc.inv.sr.df$X2080[cc.inv.sr.df$X2080 > 0.50]) / length( cc.inv.sr.df$X2080)*100
  Frac.High.90 <- length(cc.inv.sr.df$X2090[cc.inv.sr.df$X2090 > 0.50]) / length( cc.inv.sr.df$X2090)*100
  
  RANGE <- round(RANGE, 2)
  range <-  paste(RANGE[1], RANGE[2], sep="-")
  list <-  c(range,
             round(MEAN.2020,2),
             round(Frac.High.20,2),
             round(Frac.High.30,2),
             round(Frac.High.40,2),
             round(Frac.High.50,2),
             round(Frac.High.60,2),
             round(Frac.High.70,2),
             round(Frac.High.80,2),
             round(Frac.High.90, 2))
  return (  list)
  
}

regional.summary <- function (raster, SP){
  
  cc.inv.sr <- terra::crop(rast(raster), SP)  %>% mask( SP)
  cc.inv.sr.df <-as.data.frame(cc.inv.sr) %>% na.omit()
  
  RANGE <- range(cc.inv.sr.df$X2020)
  MEAN.2020 <- mean(cc.inv.sr.df$X2020)
  Frac.High <- length(cc.inv.sr.df$X2020[cc.inv.sr.df$X2020 > 0.50]) / length( cc.inv.sr.df$X2020)*100
  
  cc.inv.sr.df$D2030 <- cc.inv.sr.df$X2030-cc.inv.sr.df$X2020
  cc.inv.sr.df$D2040 <- cc.inv.sr.df$X2040-cc.inv.sr.df$X2020
  cc.inv.sr.df$D2050 <- cc.inv.sr.df$X2050-cc.inv.sr.df$X2020
  cc.inv.sr.df$D2060 <- cc.inv.sr.df$X2060-cc.inv.sr.df$X2020
  cc.inv.sr.df$D2070 <- cc.inv.sr.df$X2070-cc.inv.sr.df$X2020
  cc.inv.sr.df$D2080 <- cc.inv.sr.df$X2080-cc.inv.sr.df$X2020
  cc.inv.sr.df$D2090 <- cc.inv.sr.df$X2090-cc.inv.sr.df$X2020
  
  Delta.mean <- mean(c(mean(cc.inv.sr.df$D2030),
                       mean(cc.inv.sr.df$D2040),
                       mean(cc.inv.sr.df$D2050),
                       mean(cc.inv.sr.df$D2060),
                       mean(cc.inv.sr.df$D2070),
                       mean(cc.inv.sr.df$D2080),
                       mean(cc.inv.sr.df$D2090)))
  RANGE <- round( RANGE, 2)
  range <-  paste(RANGE[1], RANGE[2], sep="-")
  
  list <- round(c(MEAN.2020, Frac.High, Delta.mean,
            mean(cc.inv.sr.df$D2040),
            mean(cc.inv.sr.df$D2050),
            mean(cc.inv.sr.df$D2060),
            mean(cc.inv.sr.df$D2070),
            mean(cc.inv.sr.df$D2080),
            mean(cc.inv.sr.df$D2090)), 2)
  
  list2 <- c(range, list)
  return (  list2)
  
}
# RANGE, MEAN.2020, Frac.High, Delta.mean

### CODE TO PRODUCE THE TABLES: 
regional.table.regional.summary <- function(FUN ){
  wp.summary.inv <- FUN (ensemble.inv, wp)
  wp.summary.est <- FUN (ensemble.est, wp)
  
  sr.summary.inv <- FUN (ensemble.inv, sr)
  sr.summary.est <- FUN (ensemble.est, sr)
  
  gb.summary.inv <- FUN (ensemble.inv, gb)
  gb.summary.est <- FUN (ensemble.est, gb)
  
  sw.summary.inv <- FUN (ensemble.inv, sw)
  sw.summary.est <- FUN (ensemble.est, sw)
  
  ssn.summary.inv <- FUN (ensemble.inv, ssn)
  ssn.summary.est <- FUN (ensemble.est, ssn)
  
  inv <- rbind(wp.summary.inv, sr.summary.inv, gb.summary.inv, sw.summary.inv, ssn.summary.inv )
  
  names(inv) <- c('RANGE',  'MEAN.2020', 'FH2020','DM2030',
                  'DM2040', 'DM2050', 'DM2060', 'DM2070', 'DM2080', 'DM2090')
  
  est <- rbind(wp.summary.est, sr.summary.est, gb.summary.est, sw.summary.est, ssn.summary.est )
  
  names(est) <- c('RANGE',  'MEAN.2020', 'FH2020','DM2030',
                  'DM2040', 'DM2050', 'DM2060', 'DM2070', 'DM2080', 'DM2090')
  
  
  total.df <- rbind( inv, est)
  total.df <- as.data.frame(total.df)
  names(total.df) <- c('RANGE',  'MEAN.2020', 'FH2020','DM2030',
                       'DM2040', 'DM2050', 'DM2060', 'DM2070', 'DM2080', 'DM2090')
  return( total.df )
}

regional.table.Fraction50 <- function(FUN ){
  wp.summary.inv <- FUN (ensemble.inv, wp)
  wp.summary.est <- FUN (ensemble.est, wp)
  
  sr.summary.inv <- FUN (ensemble.inv, sr)
  sr.summary.est <- FUN (ensemble.est, sr)
  
  gb.summary.inv <- FUN (ensemble.inv, gb)
  gb.summary.est <- FUN (ensemble.est, gb)
  
  sw.summary.inv <- FUN (ensemble.inv, sw)
  sw.summary.est <- FUN (ensemble.est, sw)
  
  ssn.summary.inv <- FUN (ensemble.inv, ssn)
  ssn.summary.est <- FUN (ensemble.est, ssn)
  
  inv <- rbind(wp.summary.inv, sr.summary.inv, gb.summary.inv, sw.summary.inv, ssn.summary.inv )
  
  names(inv) <- c('RANGE',  'MEAN.2020', 'FH2020','FH2030',
                  'FH2040', 'FH2050', 'FH2060', 'FH2070', 'FH2080', 'FH2090')
  
  est <- rbind(wp.summary.est, sr.summary.est, gb.summary.est, sw.summary.est, ssn.summary.est )
  
  names(est) <- c('RANGE',  'MEAN.2020', 'FH2020','FH2030',
                  'FH2040', 'FH2050', 'FH2060', 'FH2070', 'FH2080', 'FH2090')
  
  
  total.df <- rbind( inv, est)
  total.df <- as.data.frame(total.df)
  names(total.df) <- c('RANGE',  'MEAN.2020', 'FH2020','FH2030',
                  'FH2040', 'FH2050', 'FH2060', 'FH2070', 'FH2080', 'FH2090')
  return( total.df )
}



# Species tables:

species.table.regional.summary <- function(FUN ){
  
  PIAR.summary.inv <- FUN(ensemble.inv, PIAR)
  PIAR.summary.est <- FUN(ensemble.est, PIAR)
  
  PIST.summary.inv <- FUN(ensemble.inv, PIST)
  PIST.summary.est <- FUN(ensemble.est, PIST)
  
  PILO.summary.inv <- FUN(ensemble.inv, PILO)
  PILO.summary.est <- FUN(ensemble.est, PILO)
  
  PIFL.summary.inv <- FUN(ensemble.inv, PIFL)
  PIFL.summary.est <- FUN(ensemble.est, PIFL)
  
  PIAL.summary.inv <- FUN(ensemble.inv, PIAL)
  PIAL.summary.est <- FUN(ensemble.est, PIAL)
  
  PIBA.summary.inv <- FUN(ensemble.inv, PIBA)
  PIBA.summary.est <- FUN(ensemble.est, PIBA)
  
  inv <- rbind(  PIBA.summary.inv, PIAL.summary.inv , PIFL.summary.inv, PILO.summary.inv ,
                 PIST.summary.inv, PIAR.summary.inv)
  
  names(inv) <- c('RANGE',  'MEAN.2020', 'FH2020','DM2030',
                  'DM2040', 'DM2050', 'DM2060', 'DM2070', 'DM2080', 'DM2090')
  
  est <- rbind(  PIBA.summary.est, PIAL.summary.est , PIFL.summary.est, PILO.summary.est ,
                      PIST.summary.est, PIAR.summary.est)
  
  names(est) <- c('RANGE',  'MEAN.2020', 'FH2020','DM2030',
                  'DM2040', 'DM2050', 'DM2060', 'DM2070', 'DM2080', 'DM2090')
  
  
  total.df <- rbind( inv, est)
  total.df <- as.data.frame(total.df)
  names(total.df) <- c('RANGE',  'MEAN.2020', 'FH2020','DM2030',
                       'DM2040', 'DM2050', 'DM2060', 'DM2070', 'DM2080', 'DM2090')
  return( total.df )
}

species.table.Fraction50 <- function(FUN ){
  
  PIAR.summary.inv <- FUN(ensemble.inv, PIAR)
  PIAR.summary.est <- FUN(ensemble.est, PIAR)
  
  PIST.summary.inv <- FUN(ensemble.inv, PIST)
  PIST.summary.est <- FUN(ensemble.est, PIST)
  
  PILO.summary.inv <- FUN(ensemble.inv, PILO)
  PILO.summary.est <- FUN(ensemble.est, PILO)
  
  PIFL.summary.inv <- FUN(ensemble.inv, PIFL)
  PIFL.summary.est <- FUN(ensemble.est, PIFL)
  
  PIAL.summary.inv <- FUN(ensemble.inv, PIAL)
  PIAL.summary.est <- FUN(ensemble.est, PIAL)
  
  PIBA.summary.inv <- FUN(ensemble.inv, PIBA)
  PIBA.summary.est <- FUN(ensemble.est, PIBA)
  
  inv <- rbind(  PIBA.summary.inv, PIAL.summary.inv , PIFL.summary.inv, PILO.summary.inv ,
                 PIST.summary.inv, PIAR.summary.inv)
  
  names(inv) <- c('RANGE',  'MEAN.2020', 'FH2020','FH2030',
                  'FH2040', 'FH2050', 'FH2060', 'FH2070', 'FH2080', 'FH2090')
  
  est <- rbind(  PIBA.summary.est, PIAL.summary.est , PIFL.summary.est, PILO.summary.est ,
                 PIST.summary.est, PIAR.summary.est)
  
  names(est) <- c('RANGE',  'MEAN.2020', 'FH2020','FH2030',
                  'FH2040', 'FH2050', 'FH2060', 'FH2070', 'FH2080', 'FH2090')
  
  
  total.df <- rbind( inv, est)
  total.df <- as.data.frame(total.df)
  names(total.df) <- c('RANGE',  'MEAN.2020', 'FH2020','FH2030',
                       'FH2040', 'FH2050', 'FH2060', 'FH2070', 'FH2080', 'FH2090')
  return( total.df )
}
