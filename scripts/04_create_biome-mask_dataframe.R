# Martin Holdrege

# Script started March 1, 2022

# Purpose:
# Load rasters downloaded in 02_download_GEE_output_from_gdrive.R
# To create a dataframe of fire occurrence, annual grass & forb biomass,
# perennial grass and forb biomass, shrub cover, and precip and temp climate
# data. This scrip is focusing and data from across the sagebrush biome


# dependencies ------------------------------------------------------------

library(terra)
library(tidyverse)
theme_set(theme_classic())
# run if need to re-download the data
# source('scripts/02_download_GEE_output_from_gdrive.R')

# read in data ------------------------------------------------------------

maskString <- '_sagebrush-biome-mask_'

# *daymet data ------------------------------------------------------------

# reading in seperate raster 'stacks' for each season
seasons <- c('Yearly', 'Summer') # not using spring at the moment

clim_paths <- paste0("data_processed/daymet/daymet_clim",  seasons,
                     "Avg_1984-2019_1000m", maskString, "v1.tif")

names(clim_paths) <- seasons

rasts_clim1 <- map(clim_paths, terra::rast) # list of rasters

# * RAP -------------------------------------------------------------------


rast_rap1 <- rast(
  paste0("data_processed/RAP/RAP_afgAGB-pfgAGB-shrCover_1984-2019_median_1000m", 
         maskString, "v1.tif"))


# * fire occurrence -------------------------------------------------------

# two paths, based on the two ways that polygons were converted to
# rasters (i.e. paint() and reduceToImage() methods)

method <- c("paint", "reduceToImage")
fire_paths <- paste0(
  "data_processed/fire_probability/mtbs-ifph-lba_fires-per-pixel_1984-2019_1000m", 
  maskString, method, "_v1.tif")
names(fire_paths) <- method

# number of observed fires per pixel, MTBS data
# monitoring trends in burn severity, 
# ifph data (interagency fire perimeter history),
# and MTBS and IFPH combined (comb), and landsat burned area (lba)
rasts_fPerPixel <- map(fire_paths, terra::rast)

# check rasters -----------------------------------------------------------
# check that all rasters have the same origion, projection, and resolution.
# throws an error if not true

# the compare Geom function only seems to be working with comparing 3 S
# Spatrasters at a time
compareGeom(rasts_clim1[[1]], rasts_clim1[[2]], rast_rap1,
            lyrs = FALSE, crs = TRUE, ext = TRUE,
            rowcol = TRUE)

compareGeom(rast_rap1, rasts_fPerPixel[[1]], rasts_fPerPixel[[2]],
            lyrs = FALSE, 
            crs = TRUE, ext = TRUE, rowcol = TRUE)


# create dataframe --------------------------------------------------------

names(rasts_clim1)
rasts_clim1$Yearly %>% names()

# data frame with predictor variables
df_biome0 <- tibble(
  afgAGB = as.vector(values(rast_rap1$afgAGB)), # biomass of annuals
  pfgAGB = as.vector(values(rast_rap1$pfgAGB)), # biomass of perennials
  shrCover = as.vector(values(rast_rap1$shrCover)), # cover of shrubs
  MAT = as.vector(values(rasts_clim1$Yearly$tavg)),
  MAP = as.vector(values(rasts_clim1$Yearly$prcp)),
  # proportion ppt falling in summer
  prcpPropSum = as.vector(values(rasts_clim1$Summer$prcpProp))
)

# seperate data frames for the two polygon to raster methods
dfs_biome0 <- map(rasts_fPerPixel, function(r) {
  out <- df_biome0 %>% 
    mutate(
      nfire_mtbs = as.vector(values(r[['mtbs']])), # mtbs data
      nfire_ifph = as.vector(values(r[['ifph']])), # ifph data
      nfire_comb = as.vector(values(r[['comb']])), # combined ifph and mtbs
      nfire_lba = as.vector(values(r[['lba']])) # landsat burned area
    )
  out
})

# remove rows with missing values (these should be the cells that 
# were masked)
dfs_biome2 <- map(dfs_biome0, drop_na)

