# Martin Holdrege

# Started 6/2/2021

# Purpose convert json file to shapefile so that it can
# be ingested into GEE


# dependencies -----------------------------------------------------------
library(sf)
library(tidyverse)

# read in file ------------------------------------------------------------

x1 <- read_sf("data_raw/combined_wildland_fire_dataset/Fire_Feature_Data_ArcMap10x.gdb/Fire_Feature_Data_v10.gdb")


# process -----------------------------------------------------------------

x2 <- x1 %>% 
  filter(str_detect(Assigned_Fire_Type, "Wildfire"),
         Fire_Year >= 1984, Fire_Year <= 2019) 

# convert and save --------------------------------------------------------

sf::write_sf(x2, 'data_processed/fire_probability/usgs_combined_wildland_fire/usgs_combined_wildland_fire_filtered.shp')


