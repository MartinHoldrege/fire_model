/*
Purpose: look at the trends in annual/perennial biomass over time 
in pixels that burned and didn't burn in each of the 5 folds

Author: Martin Holdrege

Script Started Sept. 29, 2023
*/

// parameters -------------------------------------------------

// dependencies -----------------------------------------------

var fns = require('users/mholdrege/cheatgrass_fire:src/ee_functions.js');
var preds = require('users/mholdrege/cheatgrass_fire:scripts/01_compile_pred-vars.js');

// read in data ----------------------------------------------

// folds (raster created in 06_cross-validation_env-block_byNFire.rmd)
var fold1 = ee.Image(fns.path + 'cv_folds_v1');

// image collection (1 image per year) of biomass 
var bio1 = preds.bioMasked;



