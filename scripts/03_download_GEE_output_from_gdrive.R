# Martin Holdrege

# Script started Feb 28, 2022

# Purpose: download rasters that have been exported to google drive
# in the 01_compile_data.js script


# dependencies ------------------------------------------------------------

library(googledrive)
library(stringr)
library(tidyverse)

# get file paths drive --------------------------------------------------------

# sagebrush biome masked data
files_biome <- drive_ls(path = "cheatgrass_fire",
                        pattern = "(sagebrush-biome-mask)|(sw2sim-extent)")
files_biome

# download  ---------------------------------------------------------------

files_biome <- files_biome %>% 
  mutate(modifiedTime = map_chr(drive_resource, function(x) x$modifiedTime)) %>% 
  # if multiple files with the same
  # name only download the newer one
  group_by(name) %>% 
  filter(modifiedTime == max(modifiedTime))
# * daymet ------------------------------------------------------------------

daymet_files <- files_biome %>%
  filter(str_detect(name, '^daymet')) 

for (i in 1:nrow(daymet_files)) {
  drive_download(file = daymet_files$id[i], 
                 path = file.path("data_processed/daymet", daymet_files$name[i]),
                 overwrite = TRUE)
}

# * fires -----------------------------------------------------------------

fire_files <- files_biome %>%
  filter(str_detect(name, 'fires'))

for (i in 1:nrow(fire_files)) {
  drive_download(file = fire_files$id[i], 
                 path = file.path("data_processed/fire_probability", fire_files$name[i]),
                 overwrite = TRUE)
}


# * RAP ---------------------------------------------------------------------

rap_files <- files_biome %>%
  filter(str_detect(name, '^RAP'))

for (i in 1:nrow(rap_files)) {
  drive_download(file = rap_files$id[i], 
                 path = file.path("data_processed/RAP", rap_files$name[i]),
                 overwrite = TRUE)
}

# * old data downloads below ----------------------------------------------

# files masked to pastick et al fire probability data set
files_daymet_pastick <- drive_ls(path = 'gee', 
                                 pattern = '^daymet.*pastick-etal')
files_daymet_pastick
files_rap_pastick <- drive_ls(path = 'gee', 
                              pattern = 'RAP.*pastick-etal')


# daymet data
for (i in 1:nrow(files_daymet_pastick)) {
  row <- files_daymet_pastick[i, ]
  drive_download(file = row$id, 
                 path = file.path("data_processed/daymet", row$name),
                 overwrite = TRUE)
}

# RAP data
for (i in 1:nrow(files_rap_pastick)) {
  row <- files_rap_pastick[i, ]
  drive_download(file = row$id, 
                 path = file.path("data_processed/RAP", row$name),
                 overwrite = TRUE)
}

# fire probability
fire_name <- 'LT_Wildfire_Prob_85to19_v1-0_1000m.tif'
drive_download(fire_name,
               path = file.path("data_processed/fire_probability", fire_name),
               overwrite = TRUE)

# mtbs--fire count per pixel
file <- 'mtbs-ifph-comb_fires-per-pixel_1985-2019_1000m_pastick-etal-mask_v1.tif'
drive_download(file,
               path = file.path("data_processed/fire_probability", file),
               overwrite = TRUE)

# fsim burn probability
file <- 'fsim_burn-prob_1000m_pastick-etal-mask_v1.tif'
drive_download(file,
               path = file.path("data_processed/fire_probability", file),
               overwrite = TRUE)


# AIM data
file <- 'AIM-sagebrush-sites_with-climate-and-fire_1985-2019_v1.csv'
drive_download(file,
               path = file.path("data_processed/AIM", file),
               overwrite = TRUE)

