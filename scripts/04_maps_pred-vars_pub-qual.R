# creates fig 1 in the manuscript

# dependencies ------------------------------------------------------------

source('src/basemaps.R')
source("src/fig_params.R")
source("src/general_functions.R")
library(RColorBrewer)
library(sf)
library(terra)
library(tidyverse)

# load raster --------------------------------------------------------

s <- '_annf3_A-P_entire'
# This raster contains layers that are averages across years for all the 
# predictor variables (it also includes predicted fire probability, which
# is not used here). This tif is created in 03_model_sensitivity.R
r0 <- rast(paste0('data_processed/pred-fire-clim-veg_avg-across-yrs', s, '.tif'))


# base map ----------------------------------------------------------------
tmap_mode("plot")
bbox2 <- bbox3
bbox2[['xmin']] <- -3100000
base <- basemap_hist(bbox = bbox2)


# prepare raster -----------------------------------------------------------

r1 <- r0
r1[['MAT']] <- r0[['MAT']] - 273.15

# Maps --------------------------------------------------------------------



# * RAP -------------------------------------------------------------------


tm_afgAGB <- tm_shape(r1[["afgAGB"]], bbox = bbox2) +
  tm_raster(breaks = breaks_bio2,
            labels = label_creator(breaks_bio2),
            palette = palette_bio2,
            title = lab_bio0,
            legend.hist = TRUE) +
  base +
  tm_layout(main.title = paste(fig_letters[4], "Annual forb and grass aboveground biomass"))

tm_afgAGB
# perennials
tm_pfgAGB <- tm_shape(r1[["pfgAGB"]], bbox = bbox2) +
  tm_raster(breaks = breaks_bio1,
            labels = label_creator(breaks_bio1),
            palette = palette_bio1,
            title = lab_bio0,
            legend.hist = TRUE) +
  base +
  tm_layout(main.title = paste(fig_letters[5], "Perennial forb and grass aboveground biomass"))


tm_pfgAGB
# * climate ---------------------------------------------------------------

# making breaks at the low points between the the 3 humps in the prcpPropSum
# histogram, so the first hump is red, second purple, 3rd blue
mm <- as.numeric(minmax(r1[['prcpPropSum']]))
first <- seq(mm[1] - 0.0001, 0.165, # removing small amount to make sure limits are inclusive of the data
             length.out = 4)
second <- seq(0.165, 0.265, length.out = 4)
third <- seq(0.265, mm[2] + 0.0001,
             length.out = 4)

breaks_prop <- unique(c(first, second, third))

breaks_map <- c(0, 50, 100, 150, 200, 300, 400, 500, 800,
                2500)
met1 <- tm_shape(r1[["MAP"]], bbox = bbox2) +
  tm_raster(breaks = breaks_map,
            labels = label_creator(breaks_map),
            palette = brewer.pal(length(breaks_map) -1, 'YlGnBu'),
            title = 'Precip. (mm)',
            legend.hist = TRUE)  +
  base +
  tm_layout(main.title = paste(fig_letters[2], "Precipitation"))

met1

# MAT
breaks_met <- c(-5, seq(2, 18, by = 2), 30)
labels_met <- label_creator(breaks_met)
labels_met[1] <- paste("<", breaks_met[2])
met2 <- tm_shape(r1[["MAT"]], bbox = bbox2) +
  tm_raster(title = 'Temp. (\u00B0C)', # deg C
            palette = '-RdYlBu',
            breaks = breaks_met,
            labels = labels_met,
            legend.hist = TRUE,
            midpoint = 6)  +
  base+
  tm_layout(main.title = paste(fig_letters[1], "Temperature"))
met2
met3 <- tm_shape(r1[["prcpPropSum"]], bbox = bbox2) +
  tm_raster(title = 'Proportion',
            breaks = breaks_prop,
            palette = cols_prop,
            legend.hist = TRUE)  +
  base+
  tm_layout(main.title = paste(fig_letters[3], 'Proportion of precipitation that falls in summer (Jun-Aug)'))
met3


# * empty map -------------------------------------------------------------
# for blank spot on the map
blank_map <- tm_shape(spData::us_states) +
  tm_borders(col = 'white') +
  tm_layout(frame = FALSE)
# * combine -----------------------------------------------------------------


# left column is met, right column is biomass
jpeg("figures/maps_climate/maps_RAP-and-climate-with-hist_v4.jpeg", units = 'in', res = 600,
     height = 8, width = 7)
tmap_arrange( met2, tm_afgAGB, met1, tm_pfgAGB, met3,  nrow = 3, asp = NA)
dev.off()




