# Purpose: create a cell number 'index' raster, where each cell
# is the cell number which is used as an identifier in various dataframes
# this tif is used in other downstream scripts and is also part of the data
# publication

library(terra)

# read in data ------------------------------------------------------------

# use one of the climate rasters as a template
r <- rast("data_processed/daymet/daymet_climYearly3yrAvg_1986-2019_1000m_sagebrush-biome-mask_v2.tif")[[1]]

# cell numbers actually in the final dataset
vec_cell_nums <- unique(readr::read_csv('data_processed/fire-clim-veg_3yrAvg_v2.csv')$cell_num)

# create cellNum raster ---------------------------------------------------

cellNums <- r
cellNums[!is.na(r)] <- terra::cells(r)
# plot(cellNums)
names(cellNums) <- 'cell_num'
x <- as.data.frame(cellNums)$cell_num
missing <-  x[!x %in% vec_cell_nums] # cell numbers missing from the main data set (so don't actually want to include)

cellNums[cellNums %in% missing] <- NA
# create dataframe for output ----------------------------------------------

# output data -------------------------------------------------------------

writeRaster(cellNums, "data_processed/data_publication/cell_nums.tif",
            overwrite = TRUE)