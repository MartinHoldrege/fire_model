# Martin Holdrege

# Script started March 11, 2022

# Purpose: Create maps of spatial probability and fire occurrence datasets
# (i.e. response variables), and of predictor variables (e.g., RAP
# biomass cover datasets)


# dependencies ------------------------------------------------------------

library(terra)
library(tmap)
library(spData, quietly = TRUE) # for us_states polygon
source("src/general_functions.R")
source("src/fig_params.R")

# read in data ------------------------------------------------------------

# this data downloaded in
# "scripts/02_download_GEE_output_from_gdrive.R"

# * fire data -------------------------------------------------------------

p <- "data_processed/fire_probability"
# fire probability modelled by Pastick et al
rast_fire1 <- rast(file.path(p, "LT_Wildfire_Prob_85to19_v1-0_1000m.tif"))


# number of observed fires per pixel, MTBS data
# monitoring trends in burn severity, 
# ifph data (interagency fire perimeter history),
# and MTBS and IFPH combined
rast_fPerPixel <- rast(file.path(
  p,  "mtbs-ifph-comb_fires-per-pixel_1985-2019_1000m_pastick-etal-mask_v1.tif"))

# burn probability based on the fsim model 
# (https://doi.org/10.2737/RDS-2016-0034-2)
rast_fsim1 <- rast(file.path(
  p, "fsim_burn-prob_1000m_pastick-etal-mask_v1.tif"))


# * rap data --------------------------------------------------------------

# annual forb and grass biomass, perennial forb and grass biomass, and
# shrub cover
rast_rap1 <- rast("data_processed/RAP/RAP_afgAGB-pfgAGB-shrCover_1985-2019_median_1000m_pastick-etal-mask_v1.tif")


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
rast_PastickYrPerc[is.na(rast_fPerPixel[[1]])] <- NA

# maps --------------------------------------------------------------------

tmap_mode("plot")
# extend bounding box
bbox <- tmaptools::bb(x = raster::raster(rast_fire1), ext = 1.15) 

# * base map --------------------------------------------------------------


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

# * Observed fire occurence --------------------------------------------------

breaks <- c(-0.5, 0.5, 1.5, 2.5, 3.5, 4.5, 6.5, 13)
labels <-  c(0, 1, 2, 3, 4, 6, '>=6')
palette <- c('grey', RColorBrewer::brewer.pal(6, "YlOrRd"))
tm2 <- tm_shape(rast_fPerPixel[['mtbs']], bbox = bbox) +
  tm_raster(title = "N fires",
            breaks = breaks,
            labels = labels,
            palette = palette) +
  base +
  tm_layout(main.title = "MTBS, # of fires over 35 years")

tm3 <- tm_shape(rast_fPerPixel[['ifph']], bbox = bbox) +
  tm_raster(title = "N fires",
            breaks = breaks,
            labels = labels,
            palette = palette) +
  base +
  tm_layout(main.title = "IFPH, # of firest over 35 years")

tm4 <- tm_shape(rast_fPerPixel[['comb']], bbox = bbox) +
  tm_raster(title = "N fires",
            breaks = breaks,
            labels = labels,
            palette = palette) +
  base +
  tm_layout(main.title = "IFPH and MTBS combined\n# of fires over 35 years")



# * FSim ------------------------------------------------------------------

tm5 <- tm_shape(rast_fsim1*100, bbox = bbox) +
  tm_raster(title = "Fire probability (%/yr)",
            breaks = fprob_breaks) +
  base +
  tm_layout(main.title = "FSim modelled fire probability")



# * combine into multi panel map ------------------------------------------

pdf("figures/maps_fire_prob/fire_prob_Pastick-etal-mask_v1.pdf",
    width = 8, height = 9)
tmap_arrange(tm1, tm2, tm5, tm3, tm4,  ncol = 2)
dev.off()


# * RAP maps ----------------------------------------------------------------
# rangeland analysis platform biomass and cover data

title <- "\nMedian values (1985-2019)"

breaks_bio1 <- c(0, 10, 20, 50, 100, 200, 500, 1000, 5000)
palette_bio1 <- RColorBrewer::brewer.pal(length(breaks_bio), 'YlGn')

# annuals
tm_rap1 <- tm_shape(rast_rap1[["afgAGB"]], bbox = bbox) +
  tm_raster(breaks = breaks_bio1,
            palette = palette_bio1,
            title = lab_bio0) +
  base +
  tm_layout(main.title = paste("Annual forb and grass biomass", 
                               title))

# perennials
tm_rap2 <- tm_shape(rast_rap1[["pfgAGB"]], bbox = bbox) +
  tm_raster(breaks = breaks_bio1,
            palette = palette_bio1,
            title = lab_bio0) +
  base +
  tm_layout(main.title = paste("Perennial forb and grass biomass", 
                               title))

# shrubs
breaks_cov1 <- c(0,2, 5, 10, 15, 20, 30, 50, 70)
palette_cov1 <- RColorBrewer::brewer.pal(length(breaks_cov1), 'YlGn')

tm_rap3 <- tm_shape(rast_rap1[["shrCover"]], bbox = bbox) +
  tm_raster(breaks = breaks_cov1,
            palette = palette_cov1,
            title = "% Cover") +
  base +
  tm_layout(main.title = paste("Shrub cover", 
                               title))

# save maps
pdf("figures/maps_veg/RAP_bio-cover_Pastick-etal-mask_v1.pdf",
    width = 8, height = 7)
tmap_arrange(tm_rap1, tm_rap2, tm_rap3, nrow = 2)
dev.off()

