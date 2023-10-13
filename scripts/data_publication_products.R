# Martin Holdrege

# Script started 3/9/2023

# Purpose: create files for science base data publication. 
# first create a cell number 'index' raster, where each cell
# is the cell number which is used as an identifier in various dataframes
# and also a table of data (predictor and response variables used in models)

# STop--this script is currently in flux, the onlything that has
# been updated is the cell_nums raster


# dependencies ------------------------------------------------------------


library(terra)
library(tidyverse)


# read in data ------------------------------------------------------------

# use one of the climate rasters as a template
r <- rast("data_processed/daymet/daymet_climYearly3yrAvg_1986-2019_1000m_sagebrush-biome-mask_v2.tif")[[1]]

# the final model, described in the manuscript submitted to fire ecology
#mod <- readRDS("models/glm_binomial_models_byNFire_v2_bin20_cwf_A-P_A2-T2_A-Pr.RDS")[[1]]

# to do-- read in data file

# create cellNum raster ---------------------------------------------------

r <- r[[1]]

cellNums <- r
cellNums[!is.na(r)] <- terra::cells(r)
plot(cellNums)


# create dataframe for output ----------------------------------------------

# df1 <- df_byNFire2_hmod
# df1$predicted_prob <- predict(mod, newdata = df1, type = "response")
# 
# df2 <- df1 %>% 
#   select(-herbAGB, -occur_cwf, -weight, -cwf_prop) %>% 
#   rename(nfire = nfire_cwf) %>% 
#   select(cell_num, cell_size, nfire, predicted_prob, everything())


# *summary stats -----------------------------------------------------------
# column summary stats for terry
# summaries <- df2 %>% 
#   pivot_longer(cols = everything(),
#                names_to = "column_name") %>% 
#   group_by(column_name) %>% 
#   summarize(min = min(value, na.rm = TRUE),
#             max = max(value, na.rm = TRUE))

# output data -------------------------------------------------------------

writeRaster(cellNums, "data_processed/data_publication/cell_nums.tif",
            overwrite = TRUE)

# write_csv(df2, "data_processed/data_publication/fire_climate_vegetation.csv")
# 
# 
# write_csv(summaries, "data_processed/data_publication/data_pub_col_summaries.csv")