# Martin Holdrege

# Script started Feb 28, 2022

# Purpose: download rasters that have been exported to google drive
# in the 01_compile_data.js script


# dependencies ------------------------------------------------------------

library(googledrive)

# get file paths drive --------------------------------------------------------

# files masked to pastick et al fire probability data set
files_daymet_pastick <- drive_ls(path = 'gee', 
                                 pattern = '^daymet.*pastick-etal')
files_daymet_pastick
files_rap_pastick <- drive_ls(path = 'gee', 
                              pattern = 'RAP.*pastick-etal')

files_rap_pastick
# download  ---------------------------------------------------------------

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
