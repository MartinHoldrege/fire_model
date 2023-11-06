# Purpose: create a cell number 'index' raster, where each cell
# is the cell number which is used as an identifier in various dataframes
# this tif is used in other downstream scripts and is also part of the data
# publication

library(terra)

# read in data ------------------------------------------------------------

# use one of the climate rasters as a template
r <- rast("data_processed/daymet/daymet_climYearly3yrAvg_1986-2019_1000m_sagebrush-biome-mask_v2.tif")[[1]]


# create cellNum raster ---------------------------------------------------

cellNums <- r
cellNums[!is.na(r)] <- terra::cells(r)
plot(cellNums)


# create dataframe for output ----------------------------------------------

# output data -------------------------------------------------------------

writeRaster(cellNums, "data_processed/data_publication/cell_nums.tif",
            overwrite = TRUE)