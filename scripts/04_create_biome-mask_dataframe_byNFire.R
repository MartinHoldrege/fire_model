# Martin Holdrege

# Script started June 6, 2022

# Purpose:
# Load rasters downloaded in 02_download_GEE_output_from_gdrive.R
# To create a dataframe of fire occurrence, annual grass & forb biomass,
# perennial grass and forb biomass, and precip and temp climate
# data. This scrip is focusing and data from across the sagebrush biome.

# This script is pulling together the biomass data that was separately
# summarized before and after fires. 
# (i.e. compiled in 02_compile_pre-and-post-fire.js)
# thus each grid cell will may show up multiple times in the dataset

# dependencies ------------------------------------------------------------

library(terra)
library(tidyverse)
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
  "data_processed/RAP/RAP_afgAGB-pfgAGB_byNFire_1986-2019_mean_1000m_sagebrush-biome-mask_v1.tif")

# * fire occurrence -------------------------------------------------------

# the number of yrs in each of the cumulative fire capabilities
# (i.e. number of years when 0 fires had occured, 1 fire had occured (but not 2),
# etc.). Here fire polygons were converted to pixels using the pain method
fire_path <- paste0(
  "data_processed/fire_probability/",
  "cwf_numYrsCumulativeFires_1986-2019_1000m_sagebrush-biome-mask_v1.tif")

rast_numYrs <- rast(fire_path)

# check rasters -----------------------------------------------------------
# check that all rasters have the same origion, projection, and resolution.
# throws an error if not true

# the compare Geom function only seems to be working with comparing 3 S
# Spatrasters at a time
compareGeom(rasts_clim1[[1]], rast_rap1, rast_numYrs,
            lyrs = FALSE, crs = TRUE, ext = TRUE,
            rowcol = TRUE)



# Prepare RAP data --------------------------------------------------------

# rast_rap1
# r <- rast_rap1[['fire_3_pfgAGB']]
# plot(r)
# cells(r)

get_values <- function(lyr, r) {
  x <- values(r[[lyr]]) %>% 
    as.numeric()
  
  out <- tibble(
    value = x,
    cell_num = cells(r)
  ) %>% 
    filter(!is.na(value)) %>% 
    mutate(lyr = lyr)
  
  out 
}

test <- get_values('fire_2_afgAGB', rast_rap1)
test

# extract cell values for all non NA cells (and store the cell number
# and layer name)
rap_df1 <- map_dfr(names(rast_rap1), get_values, r = rast_rap1)

rap_df2 <- rap_df1 %>% 
 # slice_sample(n = 100) %>% # for testing
  mutate(pft = str_extract(lyr, "[[:alpha:]]+$"),
         fire_num = str_extract(lyr, "(?<=_)\\d+(?=_)"),
         fire_num = as.numeric(fire_num)) 

rap_df3 <- rap_df2 %>% 
  select(-lyr) %>% 
  pivot_wider(names_from = 'pft', values_from = 'value')

# for exploration
df <- rap_df2 %>% 
  filter(
         fire_num < 3) %>% 
  select(-fire_num, -pft) %>% 
  pivot_wider(names_from = 'lyr',
              values_from = 'value')
ggplot(df, aes(fire_0_afgAGB, fire_1_afgAGB)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = 'lm') +
  geom_abline(slope = 1)

ggplot(df, aes(fire_0_afgAGB, fire_2_afgAGB)) +
  geom_point(alpha = 0.1) +
  geom_smooth() +
  geom_abline(slope = 1)

ggplot(df, aes(fire_0_pfgAGB, fire_2_pfgAGB)) +
  geom_point(alpha = 0.1) +
  geom_smooth() +
  geom_abline(slope = 1)
