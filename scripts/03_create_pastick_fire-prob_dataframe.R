# Martin Holdrege

# Script started March 1, 2022

# Purpose:
# Load rasters downloaded in 02_download_GEE_output_from_gdrive.R
# To create a dataframe of fire probability, annual grass & forb biomass,
# perennial grass and forbe biomass, shrub cover, and precip and temp climate
# data. 


# dependencies ------------------------------------------------------------

library(terra)
library(tidyverse)
theme_set(theme_classic())
# run if need to re-download the data
# source('scripts/02_download_GEE_output_from_gdrive.R')

# read in data ------------------------------------------------------------

# *daymet data ------------------------------------------------------------

# reading in seperate raster 'stacks' for each season
seasons <- c('Yearly', 'Summer', "Spring")

clim_paths <- paste0("data_processed/daymet/daymet_clim",  seasons,
                     "Avg_1985-2019_1000m_Patrick-etal-mask_v1.tif")

names(clim_paths) <- seasons

rasts_clim1 <- map(clim_paths, terra::rast) # list of rasters

# * RAP -------------------------------------------------------------------

rast_rap1 <- rast("data_processed/RAP/RAP_afgAGB-pfgAGB-shrCover_1985-2019_median_1000m_Patrick-etal-mask_v1.tif")

# * fire probability ------------------------------------------------------

rast_fire1 <- rast("data_processed/fire_probability/LT_Wildfire_Prob_85to19_v1-0_1000m.tif")


# check rasters -----------------------------------------------------------
# check that all rasters have the same origion, projection, and resolution.
# throws an error if not true

# the compare Geom function only seems to be working with comparing 3 S
# Spatrasters at a time
compareGeom(rasts_clim1[[1]], rasts_clim1[[2]], rasts_clim1[[3]],
            lyrs = FALSE, crs = TRUE, ext = TRUE,
            rowcol = TRUE)

compareGeom(rast_rap1, rast_fire1, rasts_clim1[[1]], lyrs = FALSE, 
            crs = TRUE, ext = TRUE, rowcol = TRUE)


# create dataframe --------------------------------------------------------

names(rasts_clim1)
rasts_clim1$Yearly %>% names()

# data frame of data based on the pastick et al papers fire data (hence
# the 'pat' in the object name)
df_past1 <- tibble(
  fireProb = as.vector(values(rast_fire1)),
  afgAGB = as.vector(values(rast_rap1$afgAGB)), # biomass of annuals
  pfgAGB = as.vector(values(rast_rap1$pfgAGB)), # biomass of perennials
  shrCover = as.vector(values(rast_rap1$shrCover)) # cover of shrubs
)

# looping over seasons
for (season in names(rasts_clim1)) {
  rast <- rasts_clim1[[season]]
  # looping over layers (i.e. precip, tmax, tmin)
  for (lyr in names(rast)) {
    col_name <- paste0(lyr, season)
    df_past1[[col_name]] <- as.vector(values(rast[[lyr]]))
  }
}

# remove rows with missing values (these should be the cells that 
# were masked)
df_past2 <- df_past1 %>% 
  drop_na() %>% 
  mutate(fireProb = fireProb) # convert to proportion for modeling

