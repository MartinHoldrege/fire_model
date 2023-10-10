# Martin Holdrege

# Script started Feb 28, 2022

# Purpose: download rasters that have been exported to google drive
# in the 01_compile_pred-vars_ann.js script


# dependencies ------------------------------------------------------------

library(googledrive)
library(stringr)
library(tidyverse)

# get file paths drive --------------------------------------------------------

# sagebrush biome masked data
files_biome <- drive_ls(path = "cheatgrass_fire",
                        pattern = "sagebrush-biome-mask")
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


# * RAP ---------------------------------------------------------------------

rap_files <- files_biome %>%
  filter(str_detect(name, '^RAP'))

for (i in 1:nrow(rap_files)) {
  drive_download(file = rap_files$id[i], 
                 path = file.path("data_processed/RAP", rap_files$name[i]),
                 overwrite = TRUE)
}


# * human modification ----------------------------------------------------

hMod_files <- files_biome %>%
  filter(str_detect(name, '^HM_US')) 

for (i in 1:nrow(hMod_files)) {
  drive_download(file = hMod_files$id[i], 
                 path = file.path("data_processed/human_mod", hMod_files$name[i]),
                 overwrite = TRUE)
}
