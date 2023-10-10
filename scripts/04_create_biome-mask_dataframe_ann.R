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
theme_set(theme_classic())
source('src/general_functions.R')
# run if need to re-download the data
# source('scripts/02_download_GEE_output_from_gdrive.R')

# read in data ------------------------------------------------------------

maskString <- '_sagebrush-biome-mask_'

# *daymet data ------------------------------------------------------------

# reading in seperate raster 'stacks' for each season
seasons <- c('Yearly', 'Summer') # not using spring at the moment

# using the data starting in 1986 so that it matches the rap data
# that we're using
clim_paths <- paste0("data_processed/daymet/daymet_clim",  seasons,
                     "3yrAvg_1986-2019_1000m", maskString, "v2.tif")

names(clim_paths) <- seasons

rasts_clim1 <- map(clim_paths, terra::rast) # list of rasters

# * RAP & fire occurrence ---------------------------------------------------

# this data includes annual fire occurrence, as well as annual biomass
# (where the annual biomass is an average of the current and previous two years,
# but avg does not include for, example, biomass from 2 years previous if 2 or 
# 1 year prior there was a fire (i.e. don't want pre-fire data upping the vegetation
# levels inappropirately))
rast_rap1 <- rast(
  "data_processed/RAP/RAP_afgAGB-pfgAGB-fire_1986-2019_3yrAvg_1000m_sagebrush-biome-mask_v2.tif")

# * human modification ----------------------------------------------------

# from Theobalds  hmod layer used for calculating 2017-2020 SEI
rast_hmod <- rast("data_processed/human_mod/HM_US_v3_dd_2019_60ssagebrush_1000m_sagebrush-biome-mask_v2.tif")

# check rasters -----------------------------------------------------------
# check that all rasters have the same origion, projection, and resolution.
# throws an error if not true

# the compare Geom function only seems to be working with comparing 3 S
# Spatrasters at a time.
# this is important because otherwise can't combine the datasets
# based on their cell number
compareGeom(rasts_clim1[[1]], rast_rap1,
            lyrs = FALSE, crs = TRUE, ext = TRUE,
            rowcol = TRUE)

compareGeom(rast_hmod, rast_rap1,
            lyrs = FALSE, crs = TRUE, ext = TRUE,
            rowcol = TRUE)

# Prepare data --------------------------------------------------------

# * daymet ----------------------------------------------------------------

# columns have variable name and year in them, converting that to 
# longer format
pivot_longer_ann <- function(df) {
  df %>% 
  pivot_longer(cols = -cell_num,
               names_to = c(".value", "year"),
               names_pattern = "([[:alpha:]]+)_(\\d{4})")
}
# MAT and MAP
df_MATP <- as.data.frame(rasts_clim1$Yearly) %>% 
  mutate(cell_num = row.names(.)) %>% 
  pivot_longer_ann() %>% 
  rename(MAT = tavg, MAP = prcp) %>% 
  mutate(MAT = MAT + 273.15) # convert C to K

# MAT and MAP
df_prcpProp <- as.data.frame(rasts_clim1$Summer) %>% 
  mutate(cell_num = row.names(.)) %>% 
  pivot_longer_ann() %>% 
  rename(prcpPropSum = prcpProp) 

# combine all the climate datasets
df_daymet1 <- full_join(df_MATP, df_prcpProp, by = c('cell_num', 'year'))
stopifnot(nrow(df_daymet1) == nrow(df_MATP),
          nrow(df_daymet1) == nrow(df_prcpProp))

# * RAP/fire -------------------------------------------------------------------

rap_df1 <- as.data.frame(rast_rap1)
rap_df1$cell_num <- row.names(rap_df1)

rap_df2 <- rap_df1 %>% 
  pivot_longer_ann() %>% 
  # now each 'probability' is based on 1 year ie 0 or 1 fires out of
  # 1 years
  mutate(numYrs = 1)
    
# * cell size --------------------------------------------------------------
# used as a weight when calculating later averages?
r_size <- cellSize(rast_rap1[[1]], unit = 'km')
size_df1 <- get_values(1, r_size) %>% 
  rename(cell_size = value) %>% 
  select(-lyr)


# hmod --------------------------------------------------------------------

hmod_df1 <- get_values('constant', rast_hmod) %>% 
  rename(hmod = value) %>% 
  select(-lyr)

# note there are some (i.e. 755) gridcells that are NA in the hmod 
# raster, so are missing in hmod_df1
sum(!unique(rap_df2$cell_num) %in% hmod_df1$cell_num)

# combine -----------------------------------------------------------------
# combine response and predictor vars

df_ann1 <- rap_df2 %>% 
  left_join(df_daymet1, by = c("cell_num", "year")) %>% 
  mutate(cell_num = as.numeric(cell_num)) %>% 
  left_join(size_df1, by = 'cell_num')

# there shouldn't be any missing values in this
# final dataframe
check <- df_ann1 %>% 
  drop_na()
stopifnot(nrow(check) == nrow(df_ann1))

# creating columns and renaming as needed for downstream
# scripts.
df_ann2 <- df_ann1 %>% 
  rename(nfire_cwf = burned) %>% 
  # observed fire probability in any given period
  # note--here some of the observed fire probabilities will 
  # be high because the denominator (numYrs can be 1, ie 100% observed
  # fire probability)
  mutate(cwf_prop = nfire_cwf/numYrs,
         weight = numYrs*cell_size) %>% 
  select(-cell_size)

# creating this as a separate df to avoid downstream problems because
# there are are some rows (~700) where hmod is NA
df_ann2_hmod <- df_ann2 %>% 
  left_join(hmod_df1, by = "cell_num")


# write out data ----------------------------------------------------------

write_csv(df_ann2, 'data_processed/fire-clim-veg_3yrAvg_v1.csv')
# write_csv(df_ann2_hmod, 'data_processed/fire-clim-veg-hmod_3yrAvg_v1.csv')

# descriptive stats -------------------------------------------------------
# for figure 3 caption

if (FALSE) {
  df_ann2 %>% 
    mutate(MAT = MAT -273.15) %>% 
    select(MAT, MAP) %>% 
    summarize(across(everything(), 
                     .fns = list(min = min, max = max), 
                     na.rm = TRUE),
              # % over observations ommitted in the pdp plots (b/ of 
              # truncation of xlim)
              map_ommitted = mean(MAP > 1000)*100, # % of observations with MAP > 1000,
              MAT_omitted = mean(MAT < 0 | MAT > 20)*100,
              )
}




