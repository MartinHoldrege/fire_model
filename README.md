# wildfire model

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10637599.svg)](https://doi.org/10.5281/zenodo.10637599)

This repository includes code for determining the relationship (i.e., fitting a GLM) between wildfire and  annual forb and grass biomass, perennial forb and grass biomass, mean  temperature, annual precipitation, proportion summer precipitation. Using data from across the sagebrush biome. Predictor variables are three year running running averages. 

Go to the 'zenodo' branch of this repository to see the necessary scripts to reproduce the main figures and results
presented in Holdrege et al. (2024) "Wildfire probability estimated from recent climate and fine fuels across the big sagebrush region", published in Fire Ecology.

The main branch of this repository includes various additional scripts (inlcuding for grabbing data via earth engine) that will not be reproducible. The scripts in the 'zenodo' branch,
however, can be run after downloading two data files (see instructions in readme of that branch). 

# Overview

## Scripts

R code can be found in the `/scripts` folder (functions are stored in the `/src` folder). This branch of the repo also contains the .js earth engine scripts. 

Note: all .Rmd documents are written to use the project directory as the working
directory. If you're trying to knit one of the documents and it's not working, in rstudio click the down arrow next to 'knit' then 'knit directory' and select 'project directory')

## Data

Data used in this analysis is available on ScienceBase:

Holdrege, M.C., Schlaepfer, D.R., and Bradford, J.B., 2024, Observed wildfire frequency, modeled wildfire probability, climate, and fine fuels across the big sagebrush region in the western United States: U.S. Geological Survey data release, https://doi.org/10.5066/P9EFC6YC.

Again, go to the zenodo link above, or to the zenodo branch of this repo to run the code using the two data files in the this data relases. 

# Models

Notes on naming convetions

Model used for the first submission:

glm_binomial_models_byNFire_v2_bin20_cwf_A-P_A2-T2_A-Pr.RDS

subsequent naming:

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

final model suffix: `_annf3_A-P_entire` (This is the model used for the resubmission). 
