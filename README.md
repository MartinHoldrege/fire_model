# fire_model
Determining the relationship (i.e., fitting a GLM) between wildfire and  annual forb and grass biomass, perennial forb and grass biomass, mean annual temperature, mean annual precipitation, proportion summer precitation. Using data from across the sagebrush biome. 

# Scripts
Earthengine and R code can be found the the scripts folder. 

# Data

Data will be made available on ScienceBase

## `data_raw` folder

--`/Interagency_Fire_Perimeter_History_All_Years`
This folder contains a shapefile of fire perimeters, downloaded from:
https://data-nifc.opendata.arcgis.com/datasets/nifc::interagency-fire-perimeter-history-all-years/about

# Models

naming:

`_ann_` in the file name means the first set of models where annual data
was used (3 year means for each year instead of means across the whole time period)

`_annf_` models fit to annual data where fire occurrence was recalculated
as based on the fraction of the pixel that burned, instead of just the centroid (i.e. this data is less noisy)

`_annf2_` New transformations added to allow model to be more flexible. These were log10(x + 0.001)
and the 2nd order polynomial of the log10 transform and sqrt transform. e^2 and x^2 transforms
were removed because they're never used (and to reduce computation)

`_annf3_` The constant added to log10 transformations was changed stay as 0.001 for PSP but 1 for all
other variables (mostly so transform wasn't so aggressive for very tiny amounts of annuals, and perennials,
impact is incosequintial for other variables)
