rm(list=ls())

library(tidyverse)
library(randomForest)
library(hexbin)
# The purpose of this workflow is to understand the potential impacts of using difference sources for climate data.


# Load the models
load("/Users/sm3466/Dropbox (YSE)/Research/WPBR/NewData/RF_MODELFIT_Results.RDATA")

load('/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/WPBR/NewData/Final Scripts/WPBR_plots_GRIDMET.RDATA')

data.5years %>% names
data.5years <- data.5years %>% mutate( source ="GRIDMET") %>% filter(WPBR_stat == 'Invading')
#data.10years <- data.10years %>% mutate( source ="GRIDMET")
#data.20years <- data.20years %>% mutate( source ="GRIDMET")

data.5years$MODEL <-predict(rf.invading.5, data.5years, type = "prob") [,2]
#data.10years$MODEL <-predict(rf.established.10, data.10years, type = "prob") [,2]
#data.20years$MODEL <-predict(rf.established.20, data.20years, type = "prob") [,2]

GRIDMET <- rbind(data.5years) %>% select(geometry, MODEL, YEAR_LAST, YEAR_EST, Zone_easy) %>% mutate(GRIDMET = MODEL) %>% select(geometry,GRIDMET , YEAR_LAST, YEAR_EST, Zone_easy) %>% unique

rm(data.5years, data.10years, data.20years)

load('/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/WPBR/NewData/Final Scripts/WPBR_plots_DAYMET.RDATA')

data.5years <- data.5years %>% mutate( source ="DAYMET")%>% filter(WPBR_stat == 'Invading')
#data.10years <- data.10years %>% mutate( source ="DAYMET")
#data.20years <- data.20years %>% mutate( source ="DAYMET")

data.5years$MODEL <-predict(rf.invading.5, data.5years, type = "prob") [,2]
#data.10years$MODEL <-predict(model, data.10years, type = "prob") [,2]
#data.20years$MODEL <-predict(model, data.20years, type = "prob") [,2]


DAYMET <- rbind(data.5years) %>% select(geometry, MODEL, YEAR_LAST, YEAR_EST, Zone_easy) %>% mutate(DAYMET = MODEL) %>% select(geometry,DAYMET,YEAR_LAST , YEAR_EST, Zone_easy) %>% unique

rm(data.5years, data.10years, data.20years)

# Subset model results and combine into one df:

eval <- DAYMET %>% full_join( GRIDMET, by = c('geometry', 'YEAR_LAST', 'YEAR_EST','Zone_easy')) %>% mutate( diff = DAYMET - GRIDMET)
# One2One analysis for model results:

eval %>% ggplot() + geom_point(aes(x=DAYMET, y=GRIDMET)) +  geom_hex(aes(x=DAYMET, y=GRIDMET), bins = 10)

eval %>% ggplot() + geom_density(aes(x=DAYMET), col="blue") + geom_density(aes(x=GRIDMET))

library(sf)

eval.sf <- eval %>% st_as_sf()

# Discuss bias and general trends

eval.sf %>% ggplot + geom_sf( aes(col=diff))

eval %>% ggplot() + geom_density(aes(x=diff), col="blue") 

# Hexagonal Grid
# Get the bounding box of the data
hex_bbox <- st_bbox(eval.sf)
if (!exists("hex_bbox") || !inherits(hex_bbox, "bbox")) {
  hex_bbox <- st_bbox(drivers.sp)
}
# Calculate the cell size to generate desired number of hexagons
cell_size <- sqrt(
  as.numeric(hex_bbox["xmax"] - hex_bbox["xmin"]) * 
    as.numeric(hex_bbox["ymax"] - hex_bbox["ymin"]) / 1000)
# Create a hexagonal grid
hex_grid <- st_make_grid(eval.sf, cellsize = cell_size, square = FALSE)
hex_grid_sf <- st_sf(geometry = hex_grid)

DIFF.HEX.SF <- st_join(hex_grid_sf, eval.sf, join = st_intersects) %>%
  group_by(geometry) %>%
  summarise(
    diff = mean(diff, na.rm = TRUE),
    DAYMET = mean(DAYMET), 
    GRIDMET = mean(GRIDMET),
    Zone_easy = max(Zone_easy) %>% as.factor,
    .groups = "drop") %>% na.omit
DIFF.HEX.SF %>% ggplot + geom_sf(aes(fill=DAYMET, col=DAYMET)) + theme_bw() 
DIFF.HEX.SF %>% ggplot + geom_sf(aes(fill=GRIDMET, col=GRIDMET)) + theme_bw()
DIFF.HEX.SF %>% ggplot + geom_sf(aes(fill=diff, col=diff)) + theme_bw()

lm(DIFF.HEX.SF$DAYMET~DIFF.HEX.SF$GRIDMET) %>% summary

library(ggplot2)
library(ggpubr)

DIFF.HEX.SF %>% ggplot(aes(x=GRIDMET, y=DAYMET)) + geom_point(alpha=0.2) + theme_bw() +
  xlim(0, 1 ) + ylim(0, 1) +
  #stat_smooth(method = "lm", se=FALSE, color="red", formula = y ~ x) + 
  stat_cor(aes(label = paste(after_stat(rr.label), after_stat(..p.label..), sep = "~`,`~")), # adds R^2 and p-value
           r.accuracy = 0.01,
           p.accuracy = 0.001,
           label.x = 0.1, label.y = 1, size = 3) +
  stat_regline_equation(aes(label = ..eq.label..), # adds equation to linear regression
                        label.x = 0.1, label.y = 0.9, size =3) +
  geom_abline(intercept = 0, slope = 1, col = 'grey50',linetype="dashed") + facet_wrap(~Zone_easy)

