# Martin Holdrege

# Script started 3/9/2023

# Purpose: create a cell number 'index' raster, where each cell
# is the cell number which is used as an identifier in various dataframes


# dependencies ------------------------------------------------------------

library(terra)


# read in data ------------------------------------------------------------

# use one of the climate rasters as a template
r <- rast("data_processed/daymet/daymet_climYearlyAvg_1986-2019_1000m_sagebrush-biome-mask_v1.tif")


# create cellNum raster ---------------------------------------------------

r <- r[[1]]

cellNums <- r
cellNums[!is.na(r)] <- terra::cells(r)
plot(cellNums)

writeRaster(cellNums, "data_processed/cell_nums.tif",
            overwrite = TRUE)
