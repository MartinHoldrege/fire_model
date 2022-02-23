# Martin Holdrege

# Script started 2/23/2022

# Purpose:
# to download data in Patrick et al 2021 (https://doi.org/10.1029/2020AV000298)
# which describes the relationship between wildfire probability and  
# annual grass cover, perennial cover, and summer rainfall.
# Patrick et all made all the data available on ScienceBase, and so this
# script downloads that data.

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

folder <- "data_raw/annual_grass_cover_2016-2019/"

item_file_download(item_id, names = files2$fname,
                   destinations = file.path(folder, files2$fname))


