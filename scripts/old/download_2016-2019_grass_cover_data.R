# Martin Holdrege

# Script started 2/23/2022

# Purpose:
# to download data from Patrick et al 2021 (https://doi.org/10.1029/2020AV000298)
# which describes the relationship between wildfire probability and  
# annual grass cover, perennial cover, and summer rainfall.
# Patrick et al made all the data available on ScienceBase, and so this
# script downloads that data.

# the 2016-2019 grass cover data set didn't end up being useful,
# so currently this script isn't needed. 

# dependencies ------------------------------------------------------------

library(sbtools) # for downloading data from science base
library(tidyverse)

# annual grass cover data set ---------------------------------------------

# https://www.sciencebase.gov/catalog/item/5edfe54382ce7e579c7088f4
doi <- '10.5066/P9XT1BV2'
x <- query_sb_doi(doi)
item_id <- x[[1]]$id
files <- item_list_files(item_id)

files2 <- files %>% 
  # only want files that give annual estimate of grass cover
  filter(str_detect(fname, "Annual estimate"))

folder <- "./data_raw/annual_grass_cover_2016-2019"

item_file_download(item_id, names = files2$fname,
                   destinations = file.path(folder, files2$fname))


# * unzip files -----------------------------------------------------------

zipped_files <- list.files(folder, pattern = ".zip$", full.names = TRUE)

map(zipped_files, unzip, exdir = folder) # unzip to folder

file.remove(list = paste0(zipped_files)) # delete the zipped folders

folder_pattern <- '^Annual estimate of grass cover in the western US \\d{4}$'
data_files <- list.files(folder, 
                         pattern = folder_pattern,
                         recursive = TRUE,
                         full.names = TRUE)

# move the files up a level
file.rename(from = data_files,
            to = file.path(folder, basename(data_files)))

# the folder that are now empty 
empty_folders <- list.files(folder, folder_pattern, full.names = TRUE)

# now deleting the empty folders--note that this is a bit 
# dangerous b/ the code doesn't actually check that the folders are now empty
unlink(empty_folders, recursive = TRUE)

# rename tif files to remove file names--so can be ingested into GEE
old_names <- list.files(folder, ".tif$")
file.rename(file.path(folder, old_names),
            file.path(folder, str_replace_all(old_names, " ", "_")))
# fire probability data set -----------------------------------------------
# Fractional estimates of exotic annual grass cover in dryland ecosystems of 
# western United States (2016 – 2019)

# I'm having issues automating the download so
# manually downloading from here: 
# https://www.sciencebase.gov/catalog/item/6023017dd34e31ed20c87329

