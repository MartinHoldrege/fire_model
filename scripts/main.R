# Purpose: run all scripts in succession so that objects are created that are needed
# by the next script. This fits the model and creates all the main figures in the manuscript (Holdrege et al. (2024) Fire Ecology)
# Here we render rmarkdown files, and changing input params as desired. 
# Rmarkdown files are then saved in scripts/copies

# Note I'm sourcing the scripts in temporary environments so there aren't
# problems with what packages are loaded (scripts were developed to be run 
# as stand alone (assuming that the necessary files, created by previous scripts,
# are available))

# Started: October 12, 2023

# Author: Martin Holdrege

# dependencies ------------------------------------------------------------

library(magrittr)

# params ------------------------------------------------------------------

date <- lubridate::today() %>% 
  as.character() %>% 
  stringr::str_replace_all("-", "")

knit_root_dir <- getwd() # project directory

# functions ---------------------------------------------------------------

#' render logistic regression model code (fit to annual data)
#'
#' @param s string that defines the model/interactions
#' @param inter interactions to include
#' @param sample_group which sample to fit the model to (at the moment 1-5 should
#' work)
#' @param test_run is this a test run
#' @param save_figs show some figs created in the script be saved externally
render_glm = function(s, inter, sample_group,
                      test_run = FALSE, save_figs = FALSE,
                      hmod = FALSE) {
  rmarkdown::render(
    "scripts/01_models_biome-mask_fire-prob_ann.Rmd",
    knit_root_dir = knit_root_dir,
    params = list(
      test_run = test_run,
      save_figs = save_figs,
      s = s,
      hmod = hmod,
      inter = inter,
      sample_group = sample_group
    ),
    # descriptor,  annf2:ann (annual data), f = fractional cover used, 
    # 2 = second version where new transformations tested and annuals transformed based on the main effect transformation
  
    output_file = paste0("01_models_biome-mask_fire-prob_annf3_", date, 
                         stringr::str_replace(s, "_annf3", ""),
                         "_g", sample_group, ".html"),
    output_dir = file.path(knit_root_dir, 'scripts/copies')
  )
}

#' render cross validation code(fit to annual data)
#'
#' @param s string that defines the model/interactions
#' @param test_run is this a test run
render_cv = function(s, test_run = FALSE) {
  rmarkdown::render(
    "scripts/03_cross-validation_env-block_ann.Rmd",
    knit_root_dir = knit_root_dir,
    params = list(
      s = s,
      test_run = test_run
    ),
    # descriptor,  annf2:ann (annual data), f = fractional cover used, 
    # 2 = second version where new transformations tested and annuals transformed based on the main effect transformation
    
    output_file = paste0("03_cross-validation_env-block_", date, s, ".html"),
    output_dir = file.path(knit_root_dir, 'scripts/copies')
  )
}
# render docs ------------------------------------------------------------

# * glm -------------------------------------------------------------------

# fit a glm to the data (subset of 5 million data points), 
# does computationally intensive model selection, puts a object in models/
# that is used by downstream scripts
# note this can be run with hmod = TRUE to do the model selection with human
# modification included as an additional predictor (analysis presented in the 
# appendix. Note that to then fit the entire model with hmod the 02_model_entire-dataset.R
# script would need to be run, but change the path in the script to point to the correct
# model object)
render_glm(s = "_annf3_A-P",
           inter = c('afgAGB:MAP' = "afgAGB:MAP"),
           sample_group = 1,
           test_run = FALSE)

# note instead of running render_glm() you can also just go into the
# 01_models_biome-mask_fire-prob_ann.Rmd file and knit it

# fit model to the entire data set ----------------------------------------
tmpEnv <- new.env()

# fits the model formula determined in the previous step to the entire 
# dataset, and puts the model objects in models/

source('scripts/02_model_entire-dataset.R', local = tmpEnv) 
rm(list = ls(envir = tmpEnv), envir = tmpEnv)

# cv --------------------------------------------------------------------
# conduct cross validation
# this can only be run after (02_model_entire-dataset.R)
# note the cross validation code is very memory intensive, just to test the code
# on a subset of the data set test_run to TRUE, I'm 
# commenting out this line of code here, because it is not critical for 
# reproducing the main results. 

# render_cv(s = '_annf3_A-P_entire',
#           test_run = FALSE)


# model sensitivity  ----------------------------------------------------
tmpEnv <- new.env()

# does analysis of model sensitivity (to perturbations in predictions)
# creates maps (for appendix) and histograms (Figure 6), also creates a tiff of average predictor
# values that is used in downstream scripts

source('scripts/03_model_sensitivity.R', local = tmpEnv) 
rm(list = ls(envir = tmpEnv), envir = tmpEnv)

# map of predictor variables ------------------------------------------

tmpEnv <- new.env()
# creates Figure 1 in the manuscript 
source('scripts/04_maps_pred-vars_pub-qual.R', local = tmpEnv) 
rm(list = ls(envir = tmpEnv), envir = tmpEnv)

# pdp and quantile figs ------------------------------------------

tmpEnv <- new.env()

# This script creates (among others) Figs 3, 4, & 5 in the manuscript
# (partial dependence plots, 'quantile' plots and 'filtered quantile' plots)
# Note this is a very memory intensive script

# adjust the n_pdp and n_quant parameters in this script to change how 
# large of subsamples of the data figures are based on. 
source('scripts/04_figures_pdp_vip_quant.R', local = tmpEnv) 
rm(list = ls(envir = tmpEnv), envir = tmpEnv)


# summary statistics ------------------------------------------------------
# rmarkdown doc that calculates summary statistics used in various places
# in the manuscript

rmarkdown::render(
  "scripts/04_summary_stat_tables.Rmd",
  knit_root_dir = getwd(),
  output_file = "04_summary_stat_tables.html",
  output_dir = file.path(getwd(), 'scripts/copies'))

