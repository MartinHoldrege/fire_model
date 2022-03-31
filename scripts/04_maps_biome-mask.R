# Martin Holdrege

# Script started March 31, 2022

# Purpose: Create maps of spatial probability and fire occurrence datasets
# (i.e. response variables), and of predictor variables (e.g., RAP
# biomass cover datasets). This is for data masked to the extent of 
# the sagebrush biome


# dependencies ------------------------------------------------------------

# this script is useful because it reads in the rasters used below
source('scripts/04_create_biome-mask_dataframe.R')
source("src/general_functions.R")
source("src/fig_params.R")
library(tmap)
library(spData, quietly = TRUE) # for us_states polygon
library(RColorBrewer)

# * fire data -------------------------------------------------------------

# number of observed fires per pixel, MTBS data
# monitoring trends in burn severity, 
# ifph data (interagency fire perimeter history),
# and MTBS and IFPH combined, and lba
rasts_fPerPixel # rasters for both paint and reduceToImage methods


# * rap data --------------------------------------------------------------

rast_rap1


# * daymet ----------------------------------------------------------------

rasts_clim1

# maps --------------------------------------------------------------------

tmap_mode("plot")
# extend bounding box
bbox <- tmaptools::bb(x = raster::raster(rasts_fPerPixel[[1]]), ext = 1.15) 

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


# * Observed fire occurence --------------------------------------------------

breaks <- c(-0.5, 0.5, 1.5, 2.5, 3.5, 4.5, 6.5, 37)
labels <-  c(0, 1, 2, 3, 4, 6, '>=6')
palette <- c('grey', RColorBrewer::brewer.pal(6, "YlOrRd"))

base_raster <- function() {
  tm_raster(title = "N fires",
            breaks = breaks,
            labels = labels,
            palette = palette) +
    base
}

yr_string <- "# of fires over 36 years"
maps_fire <- map2(rasts_fPerPixel, names(rasts_fPerPixel), function(r, name) {
  
  tm2 <- tm_shape(r[['mtbs']], bbox = bbox) +
    base_raster() +
    tm_layout(main.title = paste0("MTBS, ", yr_string, "( ", name, " method)"))
  
  tm3 <- tm_shape(r[['ifph']], bbox = bbox) +
    base_raster() +
    tm_layout(main.title = paste0("IFPH, ", yr_string, "( ", name, " method)"))
  
  tm4 <- tm_shape(r[['comb']], bbox = bbox) +
    base_raster() +
    tm_layout(main.title = paste0("IFPH and MTBS combined\n",
                                  yr_string, "( ", name, " method)"))
  
  tm5 <- tm_shape(r[['lba']], bbox = bbox) +
    base_raster() +
    tm_layout(main.title = paste0("LBA,", yr_string, "( ", name, " method)"))
  
  list(tm2, tm3, tm4, tm5)

})

# * combine into multi panel map ------------------------------------------

pdf("figures/maps_fire_prob/fire_prob_biome-mask_v1.pdf",
    width = 8, height = 7)
  tmap_arrange(maps_fire[[1]], ncol = 2)
  tmap_arrange(maps_fire[[2]], ncol = 2)
dev.off()


# * RAP maps ----------------------------------------------------------------
# rangeland analysis platform biomass and cover data

title <- "\nMedian values (1984-2019)"

breaks_bio1 <- c(0, 10, 20, 50, 100, 200, 300)
palette_bio1 <- RColorBrewer::brewer.pal(length(breaks_bio1), 'YlGn')
breaks_bio2 <- c(0, 5, 10, 20, 30, 50, 100, 200)
palette_bio2 <- brewer.pal(length(breaks_bio1), 'YlGn')
# annuals
tm_rap1 <- tm_shape(rast_rap1[["afgAGB"]], bbox = bbox) +
  tm_raster(breaks = breaks_bio2,
            palette = palette_bio2,
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
palette_cov1 <- brewer.pal(length(breaks_cov1), 'YlGn')

tm_rap3 <- tm_shape(rast_rap1[["shrCover"]], bbox = bbox) +
  tm_raster(breaks = breaks_cov1,
            palette = palette_cov1,
            title = "% Cover") +
  base +
  tm_layout(main.title = paste("Shrub cover", 
                               title))

# save maps
pdf("figures/maps_veg/RAP_bio-cover_biome-mask_v1.pdf",
    width = 8, height = 6)
tmap_arrange(tm_rap1, tm_rap2, tm_rap3, nrow = 2)
dev.off()


# * daymet data -----------------------------------------------------------


met1 <- tm_shape(rasts_clim1$Yearly[["prcp"]], bbox = bbox) +
  tm_raster(breaks = c(0, 50, 100, 150, 200, 300, 400, 500, 700, 1000,
                       2500),
            palette = 'Blues',
            title = 'MAP (mm)')  +
  base +
  tm_layout(main.title = 'Precipitation (1984 - 2019)')

met2 <- tm_shape(rasts_clim1$Yearly[["tavg"]], bbox = bbox) +
  tm_raster(title = 'MAT (deg C)',
            palette = 'Reds')  +
  base+
  tm_layout(main.title = 'Temperature (1984 - 2019)')

met3 <- tm_shape(rasts_clim1$Summer[["prcpProp"]], bbox = bbox) +
  tm_raster(title = 'Proportion',
            palette = 'Blues')  +
  base+
  tm_layout(main.title = 'Proportion of precipitation that \nfalls in summer (Jun-Aug) ')

# save maps
pdf("figures/maps_climate/climate_biome-mask_v1.pdf",
    width = 8, height = 6)
  tmap_arrange(met1, met2, met3, nrow = 2)
dev.off()
