# fire_model

This repository includes code for determining the relationship (i.e., fitting a GLM) between wildfire and  annual forb and grass biomass, perennial forb and grass biomass, mean  temperature, annual precipitation, proportion summer precipitation. Using data from across the sagebrush biome. Predictor variables are three year running running averages. 

This repository contains the scripts necessary to reproduce the main figures and results
presented in Holdrege et al. (2024) "Wildfire probability estimated from recent climate and fine fuels across the big sagebrush region", published in Fire Ecology.

# Overview

## Scripts

R code can be found in the `/scripts` folder (functions are stored in the `/src` folder)

Note: all .Rmd documents are written to use the project directory as the working
directory. If you're trying to knit one of the documents and it's not working, in rstudio click the down arrow next to 'knit' then 'knit directory' and select 'project directory')

## Data

Data used in this analysis is available on ScienceBase:

Holdrege, M.C., Schlaepfer, D.R., and Bradford, J.B., 2024, Observed wildfire frequency, modeled wildfire probability, climate, and fine fuels across the big sagebrush region in the western United States: U.S. Geological Survey data release, https://doi.org/10.5066/P9EFC6YC.

# How to reproduce the analysis

## Download data

First, two data files should be downloaded from https://doi.org/10.5066/P9EFC6YC. 

The two data files are:

`cell_numbers_ids.tif` (a raster providing cell numbers that match the tabular data)

`Wildfire_Climate_Biomass_Data.csv` which includes columns for fire occurrence,
and climate and vegetation predictor variables (see the data release for detailed 
metadata). Note this data file also includes the predicted wildfire probability (which is predicted by 
the model created in the `02_model_entire-dataset.R` script)

Put those two files (with names unchanged) in the `/data_processed/data_publication`
folder. 

## Run the code

All scripts use relative paths. Open the `fire_model.Rproj` file in with rstudio to correctly
set the working directory.

Scripts are numbered, and scripts with higher numbers rely on output from scripts
with lower numbers. You can run the individual scripts one by one, since some
scripts are very memory intensive, consider restarting R, or clearing the environment
prior to running the next. 

### Descriptions of individuals scripts



`01_models_biome-mask_fire-prob_ann.Rmd`: Goes through model selection
to find the best transformations for each variable. Does this on a sample of 5 million observations.

`02_model_entire-dataset.R`: Uses the model formula determined by the
`01_models_biome-mask_fire-prob_ann.Rmd` script, and fits the model to the entire dataset. 

`03_cross-validation_env-block_ann.rmd`: Does cross validation by separately fitting
the model to individual environmental folds. Does not create any files needed by 
downstream scripts. 

`03_model_sensitivity.R`: Creates figures showing model sensitivity to adjustments in
the predictor variables, also creates a .tif file of mean predictor (and predicted) values
which is read in by subsequent scripts. Creates Fig 2 and Fig 6 in the manuscript. 

`04_figures_pdp_vip_quant.R`: Creates Figures 3-5, in the manuscript which are
partial dependence plots, 'quantile' plots, and 'filtered quantile' plots. 
Also creates a variable importance plots. This script is memory intensive. 

`04_maps_pred-vars_pub-qual.R`: Creates map of means of predictor variables (Fig. 1 in manuscript)

`04_summary_stat_tables.Rmd`: Creates tables of summary statistics. 

`main.R`: runs all the other scripts. However, currently not all figures
are properly saved when running this script (instead of running the individual
scripts). Note this script is useful to see how to automatically render the
rmarkdown files using specified input parameters. 

### Folders

This repository contains multiple empty folders (i.e., `models`, and `/figures` and folders
therein), which are where the scripts put output. 

## Session information

The following provides the version of R, and package versions used:

`> sessionInfo()
R version 4.3.2 (2023-10-31 ucrt)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19045)

Matrix products: default


locale:
[1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8    LC_MONETARY=English_United States.utf8 LC_NUMERIC=C                          
[5] LC_TIME=English_United States.utf8    

time zone: America/Denver
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] stars_0.6-0        sf_1.0-12          abind_1.4-5        gridExtra_2.3      pdp_0.8.1          GGally_2.1.2       knitr_1.42         terra_1.7-18       ggtext_0.1.2      
[10] lubridate_1.9.2    forcats_1.0.0      stringr_1.5.0      dplyr_1.1.1        purrr_1.0.1        readr_2.1.4        tidyr_1.3.0        tibble_3.2.1       ggplot2_3.4.1     
[19] tidyverse_2.0.0    vip_0.3.2          RColorBrewer_1.1-3 dtplyr_1.3.1       patchwork_1.1.2   

loaded via a namespace (and not attached):
 [1] tidyselect_1.2.0   farver_2.1.1       fastmap_1.1.1      reshape_0.8.9      butcher_0.3.3      digest_0.6.31      timechange_0.2.0   lifecycle_1.0.3    magrittr_2.0.3    
[10] compiler_4.3.2     rlang_1.1.0        sass_0.4.6         tools_4.3.2        utf8_1.2.3         yaml_2.3.7         data.table_1.14.8  labeling_0.4.2     classInt_0.4-9    
[19] bit_4.0.5          plyr_1.8.8         xml2_1.3.4         KernSmooth_2.23-22 withr_2.5.0        grid_4.3.2         ggh4x_0.2.3        fansi_1.0.4        e1071_1.7-13      
[28] colorspace_2.1-0   scales_1.2.1       iterators_1.0.14   cli_3.6.1          rmarkdown_2.21     crayon_1.5.2       generics_0.1.3     rstudioapi_0.14    tzdb_0.3.0        
[37] commonmark_1.9.0   proxy_0.4-27       DBI_1.1.3          cachem_1.0.7       splines_4.3.2      parallel_4.3.2     ggplotify_0.1.0    yulab.utils_0.0.6  vctrs_0.6.1       
[46] Matrix_1.6-1.1     jsonlite_1.8.4     gridGraphics_0.5-1 hms_1.1.3          bit64_4.0.5        foreach_1.5.2      jquerylib_0.1.4    units_0.8-1        lwgeom_0.2-11     
[55] glue_1.6.2         codetools_0.2-19   cowplot_1.1.1      stringi_1.7.12     gtable_0.3.3       munsell_0.5.0      pillar_1.9.0       htmltools_0.5.5    R6_2.5.1          
[64] vroom_1.6.1        evaluate_0.21      lattice_0.21-9     markdown_1.5       gridtext_0.1.5     bslib_0.4.2        class_7.3-22       Rcpp_1.0.10        nlme_3.1-163      
[73] mgcv_1.9-0         xfun_0.39          pkgconfig_2.0.3   `

