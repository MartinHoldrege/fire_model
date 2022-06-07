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
                     "Avg_1986-2019_1000m", maskString, "v1.tif")

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

# right now this is only for data validation
rast_cwf1 <- rast("data_processed/fire_probability/cwf_fires-per-pixel_1987-2019_1000m_sagebrush-biome-mask_v1.tif")

# check rasters -----------------------------------------------------------
# check that all rasters have the same origion, projection, and resolution.
# throws an error if not true

# the compare Geom function only seems to be working with comparing 3 S
# Spatrasters at a time.
# this is important because otherwise can't combine the datasets
# based on their cell number
compareGeom(rasts_clim1[[1]], rast_rap1, rast_numYrs,
            lyrs = FALSE, crs = TRUE, ext = TRUE,
            rowcol = TRUE)

compareGeom(rasts_clim1[[2]], rast_cwf1,
            lyrs = FALSE, crs = TRUE, ext = TRUE,
            rowcol = TRUE)

# Prepare data --------------------------------------------------------

# * daymet ----------------------------------------------------------------

df_MAT <- get_values('tavg', rasts_clim1$Yearly)
df_MAT$value <- df_MAT$value + 273.15 # convert C to K

df_MAP <- get_values('prcp', rasts_clim1$Yearly)
df_prcpPropSum <- get_values('prcpProp', rasts_clim1$Summer)

# combine all the climate datasets
df_daymet1 <- bind_rows(df_MAT, df_MAP, df_prcpPropSum) %>% 
  pivot_wider(names_from = 'lyr', values_from = 'value') %>% 
  rename(MAT = tavg, MAP = prcp, prcpPropSum = prcpProp)


# * RAP -------------------------------------------------------------------

# rast_rap1
# r <- rast_rap1[['fire_3_pfgAGB']]
# plot(r)
# cells(r)


#test <- get_values('fire_2_afgAGB', rast_rap1)
# test

# extract cell values for all non NA cells (and store the cell number
# and layer name)
rap_df1 <- map_dfr(names(rast_rap1), get_values, r = rast_rap1)

rap_df2 <- rap_df1 %>% 
 # slice_sample(n = 100) %>% # for testing
  mutate(pft = str_extract(lyr, "[[:alpha:]]+$"),
         # cumulative number of num
         c_fire_num = str_extract(lyr, "(?<=_)\\d+(?=_)"),
         c_fire_num = as.numeric(c_fire_num)) 

rap_df3 <- rap_df2 %>% 
  select(-lyr) %>% 
  pivot_wider(names_from = 'pft', values_from = 'value')

# for exploration
if (FALSE) {
  df <- rap_df2 %>% 
    filter(
      c_fire_num < 3) %>% 
    select(-c_fire_num, -pft) %>% 
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
}


# * fire occurrence --------------------------------------------------------

cwf_nfire_df1 <- get_values('first', rast_cwf1)

# number of years when the cumulative number of fires has been 0, 1, 2, etc...
numYrs_df1 <- map_dfr(names(rast_numYrs), get_values, r = rast_numYrs)

numYrs_df2 <- numYrs_df1 %>% 
  mutate(c_fire_num = as.numeric(str_extract(lyr, "\\d+$"))) %>% 
  rename(numYrs = value) %>% 
  select(-lyr)

numYrs_df2 %>% 
  filter(c_fire_num == 0) %>% 
  arrange(numYrs)

numYrs_df3 <- numYrs_df2 %>% 
  group_by(cell_num) %>% 
  mutate(
    #
    max_c_fire_num = max(c_fire_num),
    min_c_fire_num = min(c_fire_num)) %>% 
  ungroup() %>% 
  mutate(
    # keeping original version for later joining
    c_fire_num_orig = c_fire_num,
    # adjust cumulative fire numbers, so that if the first fire occurred
    # in year 1, then subtract 1
    c_fire_num = ifelse(min_c_fire_num == 1, c_fire_num - 1, c_fire_num),
    max_c_fire_num = ifelse(min_c_fire_num == 1, max_c_fire_num - 1, 
                            max_c_fire_num),
    # b/ fires in 1986 aren't counted (b/ then no prior years RAP biomass)
    # the total period is 1 year shorter, so need to remove 1 year
    # from the last period. In other words this is b/ rap data is 
    # from 1986-2019, and fire data is 
    numYrs = ifelse(max_c_fire_num == c_fire_num, numYrs - 1, numYrs),
    # it counts as period with a fire if the fire didn't 
    # occured in the first year (which we're removing),
    # and it isn't the last period (i.e. in the last period fire has
    # already incremented up)
    nfire_cwf = case_when(
      c_fire_num < max_c_fire_num ~ 1, 
      # this is the case where a fire occurred in the first year (1986), 
      # which we're not counting
      # c_fire_num <= max_c_fire_num &  min_c_fire_num == 1 & c_fire_num > 1 ~ 1,
      TRUE ~ 0
    )) %>% 
  # filtering out rows where a fire occurred in the last year
  # this would be double counting b/ it is already counted in
  # as part of the preceding period
  filter(numYrs > 0) 

numYrs_df4 <- numYrs_df3 %>% 
  select(numYrs, cell_num, nfire_cwf, c_fire_num_orig)

# checks
check <- numYrs_df4 %>% 
  group_by(cell_num) %>% 
  summarize(totalYrs = sum(numYrs)) %>% 
  pull(totalYrs) %>% 
  unique()

# all grid cells should have total of 33 yrs of data
# (1987-2019)
stopifnot(check == 33)

# combine -----------------------------------------------------------------
# combine response and predictor vars

df_byNFire1 <- numYrs_df4 %>% 
  left_join(rap_df3, by = c("cell_num" = "cell_num", 
                            "c_fire_num_orig" = "c_fire_num")) %>% 
  left_join(df_daymet1, by = "cell_num")

# there shouldn't be any missing values in this
# final dataframe
check <- df_byNFire1 %>% 
  drop_na()
stopifnot(nrow(check) == nrow(df_byNFire1))

# creating columns and renaming as needed for downstream
# scripts.
df_byNFire2 <- df_byNFire1 %>% 
  select(-c_fire_num_orig) %>% 
  # observed fire probability in any given period
  # note--here some of the observed fire probabilities will 
  # be high because the denominator (numYrs can be 1, ie 100% observed
  # fire probability)
  mutate(cwf_prop = nfire_cwf/numYrs,
         herbAGB = afgAGB + pfgAGB,
         # including for legacy code reasons
         occur_cwf = factor(nfire_cwf > 0, c("TRUE", "FALSE")))

# so structure is the same as created in other scripts
# (paint method used in GEE)
dfs_byNFire3 <- list('paint' = df_byNFire2)

# *check ------------------------------------------------------------------

# checking if the sum of the number of fires per cell matches that 
# outputted directly from gee. If this throws an error THERE IS a problem 

stopifnot(cwf_nfire_df1$cell_num %in% df_byNFire2$cell_num,
          df_byNFire2$cell_num %in% cwf_nfire_df1$cell_num)

check <- df_byNFire2 %>% 
  group_by(cell_num) %>% 
  summarise(total_nfire = sum(nfire_cwf)) %>% 
  left_join(cwf_nfire_df1, by = 'cell_num') %>% 
  # difference between the datasets (should be 0)
  mutate(diff = total_nfire - value)

stopifnot(check$diff == 0)

