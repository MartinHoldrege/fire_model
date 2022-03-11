# Martin Holdrege

# Script started March 11, 2022

# Purpose: Create maps of spatial probability and fire occurence datasets


# dependencies ------------------------------------------------------------

library(terra)
library(tmap)
library(spData) # for us_states polygon
source("src/general_functions.R")

# read in data ------------------------------------------------------------
# this data downloaded in
# "scripts/02_download_GEE_output_from_gdrive.R"

p <- "data_processed/fire_probability"
# fire probability modelled by Pastick et al
rast_fire1 <- rast(file.path(p, "LT_Wildfire_Prob_85to19_v1-0_1000m.tif"))


# number of observed fires per pixel, MTBS data
rast_mtbs1 <- rast(file.path(
  p,  "mtbs_fires-per-pixel_1985-2019_1000m_pastick-etal-mask_v1.tif"))

# burn probability based on the fsim model 
# (https://doi.org/10.2737/RDS-2016-0034-2)
rast_fsim1 <- rast(file.path(
  p, "fsim_burn-prob_1000m_pastick-etal-mask_v1.tif"))


# prep data ---------------------------------------------------------------

# * pastick fir prob ------------------------------------------------------
# calculate probability of fire in a given year, from long term
# fire probability

fireProb <- values(rast_fire1)/100 # convert to proportion

# yearly fir probability, converted back to percent
rast_PastickYrPerc <- rast_fire1
rast_PastickYrPerc[] = calc_yearly_prob(fireProb, n = 35)*100

# Making sure values are masked (for some reason this tif
# doesn't have NA for masked values)
rast_PastickYrPerc[is.na(rast_mtbs1)] <- NA

# maps --------------------------------------------------------------------

tmap_mode("plot")

# * base map --------------------------------------------------------------
# extend bounding box
bbox <- tmaptools::bb(x = raster::raster(rast_fire1), ext = 1.15) 

base <- tmap_options( # increase number of pixels plotted
    max.raster = c(plot = 1e10, view = 1e6) 
  )+
  tm_shape(us_states) +
  tm_borders() +
  tm_layout(
    legend.outside = TRUE,
    legend.text.size = 0.75,
    main.title.size = 0.75,
    frame = FALSE,
    legend.position = c('center', 'top')) 

# * pastick fire prob -----------------------------------------------------

fprob_breaks <- c(0, 0.25, 0.5, 1, 2, 3, 5, 7, 10)
tm1 <- tm_shape(rast_PastickYrPerc, bbox = bbox) +
  tm_raster(title = "Fire probability (%/yr)",
            breaks = fprob_breaks) +
  base +
  tm_layout(main.title = "Pastick et al. modelled fire probability")

# * MTBS ------------------------------------------------------------------

breaks <- c(-0.5, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 10)
labels <-  c(0, 1, 2, 3, 4, 5, '>=6')
palette <- c('grey', RColorBrewer::brewer.pal(6, "YlOrRd"))
tm2 <- tm_shape(rast_mtbs1, bbox = bbox) +
  tm_raster(title = "N fires",
            breaks = breaks,
            labels = labels,
            palette = palette) +
  base +
  tm_layout(main.title = "MTBS, # of first over 35 years")

# * FSim ------------------------------------------------------------------

tm3 <- tm_shape(rast_fsim1*100, bbox = bbox) +
  tm_raster(title = "Fire probability (%/yr)",
            breaks = fprob_breaks) +
  base +
  tm_layout(main.title = "FSim modelled fire probability")
tm3


# * combine into multi panel map ------------------------------------------

pdf("figures/maps_fire_prob/fire_prob_Pastick-etal-mask_v1.pdf",
    width = 8, height = 7)
tmap_arrange(tm1, tm2, tm3, ncol = 2)
dev.off()
