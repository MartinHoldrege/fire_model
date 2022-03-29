#!/bin/bash

# Martin Holdrege
# Script started 2/18/2022

# Purpose: ingest landsat burned area shapefiles from gcs into GEE

# to run this script from the command line (if working directory
# is the project directory) use:
# ./scripts/upload_lba2gee.sh

conda activate ee # so earthengine commandline program will work

# may also need to run, to re-authenticate:
#earthengine authenticate

# Where the assets should go
asset_folder="projects/gee-guest/assets/cheatgrass_fire/LBA/"

# where assets are located (needs to be a google cloud storage bucket)
gs_folder="gs://mholdrege/LBA/"

# example name of file for code testing
file="LBA_CU_1984_20200415_C01_V01_BF_labeled.shp"

# this prints what the command will look like
# double check that this is correct
echo earthengine upload table --asset_id=${asset_folder}${file::-4} ${gs_folder}${file}

# note: {file::-4}, removes the last 4 characters (i.e. .tif), b/ "." can't be in asset names

# cd to folder with the names of the files that have been put on the cloud
cd ./data_raw/LBA_from_Pastick/;

# loop through each file name (using name expansion
# to get names of interest), to upload the shapefiles
for file in echo LBA_*.shp;
do 
    echo $file;
    earthengine upload table --asset_id=${asset_folder}${file::-4} ${gs_folder}${file}; 
done
cd ../..; # reset wd (not sure if necessary)
