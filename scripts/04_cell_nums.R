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
# clean up crs ----------------------------------------------------------
# string of crs (not actually a different crs but cleaner string)
# directly from an NLCD dataset on science base
crs <- "PROJCRS[\"Albers Conical Equal Area\",\n    BASEGEOGCRS[\"WGS 84\",\n        DATUM[\"World Geodetic System 1984\",\n            ELLIPSOID[\"WGS 84\",6378137,298.257223563,\n                LENGTHUNIT[\"metre\",1]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        ID[\"EPSG\",4326]],\n    CONVERSION[\"Albers Equal Area\",\n        METHOD[\"Albers Equal Area\",\n            ID[\"EPSG\",9822]],\n        PARAMETER[\"Latitude of false origin\",23,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8821]],\n        PARAMETER[\"Longitude of false origin\",-96,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8822]],\n        PARAMETER[\"Latitude of 1st standard parallel\",29.5,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8823]],\n        PARAMETER[\"Latitude of 2nd standard parallel\",45.5,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8824]],\n        PARAMETER[\"Easting at false origin\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8826]],\n        PARAMETER[\"Northing at false origin\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8827]]],\n    CS[Cartesian,2],\n        AXIS[\"(E)\",east,\n            ORDER[1],\n            LENGTHUNIT[\"meters\",1]],\n        AXIS[\"(N)\",north,\n            ORDER[2],\n            LENGTHUNIT[\"meters\",1]]]"

cellNums2 <- project(cellNums, crs)
compareGeom(cellNums2, cellNums) # confirm didn't actually change anything
# output data -------------------------------------------------------------

writeRaster(cellNums2, "data_processed/data_publication/cell_nums.tif",
            overwrite = TRUE, gdal = c("of=COG"))

# more full name for data publication
writeRaster(cellNums2, "data_processed/data_publication/cell_numbers.tif",
            overwrite = TRUE, gdal = c("of=COG"))

