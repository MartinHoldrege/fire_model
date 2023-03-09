# Martin Holdrege

# Script started March 1, 2022

# Purpose:
# Load rasters downloaded in 02_download_GEE_output_from_gdrive.R
# To create a dataframe of fire occurrence, annual grass & forb biomass,
# perennial grass and forb biomass, and precip and temp climate
# data. This scrip is focusing and data from across the sagebrush biome

# this script is for use in the updated 06_maps_biome-mask.R script.
# the diffrence is that time frames of the data (1986-2019) are a little
# different than the time frames for the data pulled
# gogether in 04_create_biome-mask_dataframe.R (1984-2019)
# additionally, here the RAP data are averages not medians
# for consistency with the data used to fit the models


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
                     "Avg_1986-2019_1000m", maskString, "v1.tif")

names(clim_paths) <- seasons

rasts_clim1 <- map(clim_paths, terra::rast) # list of rasters

# * RAP -------------------------------------------------------------------


rast_rap1 <- rast(
  paste0("data_processed/RAP/RAP_afgAGB-pfgAGB_1986-2019_mean_1000m", 
         maskString, "v1.tif"))


# * fire occurrence -------------------------------------------------------

# two paths, based on the two ways that polygons were converted to
# rasters (i.e. paint() and reduceToImage() methods)

method <- c("paint") # this data summarized via the paint method
fire_paths <- paste0("data_processed/fire_probability/cwf_fires-per-pixel_1987-2019_1000m", 
         maskString, "v1.tif")
names(fire_paths) <- method

# number of observed fires per pixel, combined wildland fire dataset
rasts_fPerPixel <- map(fire_paths, terra::rast)
names(rasts_fPerPixel[[1]]) <- 'cwf'

# * human modification ----------------------------------------------------

# from Theobalds  hmod layer used for calculating 2017-2020 SEI
rast_hmod <- rast("data_processed/human_mod/HM_US_v3_dd_2019_60ssagebrush_1000m_sagebrush-biome-mask_v1.tif")

# check rasters -----------------------------------------------------------
# check that all rasters have the same origion, projection, and resolution.
# throws an error if not true

# the compare Geom function only seems to be working with comparing 3 S
# Spatrasters at a time
compareGeom(rasts_clim1[[1]], rast_rap1, rast_hmod, 
            lyrs = FALSE, crs = TRUE, ext = TRUE,
            rowcol = TRUE)

compareGeom(rast_rap1, rasts_fPerPixel[[1]],
            lyrs = FALSE, 
            crs = TRUE, ext = TRUE, rowcol = TRUE)


# create dataframe --------------------------------------------------------

# data frame with predictor variables
df_biome0 <- tibble(
  afgAGB = as.vector(values(rast_rap1$afgAGB)), # biomass of annuals
  pfgAGB = as.vector(values(rast_rap1$pfgAGB)), # biomass of perennials
  herbAGB = afgAGB + pfgAGB, # total herbaceous biomass
  MAT = as.vector(values(rasts_clim1$Yearly$tavg)) + 273.15, # convert C to K,
  MAP = as.vector(values(rasts_clim1$Yearly$prcp)),
  # proportion ppt falling in summer
  prcpPropSum = as.vector(values(rasts_clim1$Summer$prcpProp)),
  hmod = as.vector(values(rast_hmod))
) %>% 
  mutate(cell_num = 1:nrow(.))

# seperate data frames for the two polygon to raster methods
dfs_biome0 <- map(rasts_fPerPixel, function(r) {
  out <- df_biome0 %>% 
    mutate(
      # this is only one layery and its the cwf data
      nfire_cwf = as.vector(values(r)), # cwf data
    )
  out
})

# remove rows with missing values (these should be the cells that 
# were masked)
dfs_biome2 <- map(dfs_biome0, function(df) {
  df %>% 
    # hmod has some additional NAs (so not including it here
    # to avoid downstream changes)
    select(-hmod) %>% 
    drop_na()
  })


# climate summary ----------------------------------------------------
# for site description
if (FALSE) {
  df_biome0 %>% 
    mutate(MAT = MAT -273.15) %>% 
    select(MAT, MAP, prcpPropSum) %>% 
    summarize(across(everything(), 
                     .fns = list(mean = mean, min = min, max = max), 
                     na.rm = TRUE))
}
