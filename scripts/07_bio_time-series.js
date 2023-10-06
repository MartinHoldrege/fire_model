/*
Purpose: look at the trends in annual/perennial biomass over time 
in pixels that burned and didn't burn in each of the 5 folds

Author: Martin Holdrege

Script Started Sept. 29, 2023
*/

// parameters -------------------------------------------------
var resolution = 1000; 

// dependencies -----------------------------------------------

var fns = require('users/mholdrege/cheatgrass_fire:src/ee_functions.js');

// predictor variable datasets
var preds = require('users/mholdrege/cheatgrass_fire:scripts/01_compile_pred-vars.js');

// fire datasets
var f = require('users/mholdrege/cheatgrass_fire:scripts/01_compile_fire_data.js');

// read in data ----------------------------------------------

// folds (raster created in 06_cross-validation_env-block_byNFire.rmd)
var fold1 = ee.Image(fns.path + 'cv_folds_v1');

// image collection (1 image per year) of biomass 
var bio1 = preds.bioMasked;

var fire1 = f.cwfFiresPerPixelM;

// preparing grouping image -------------------------------------
var didBurn = fire1.gte(1);

// group index: folds are values 1-5 (then multipied by 10), and fire is yes(1) and no(0)
// so 51 is fold 5 and burned, 30 is fold 3 and didn't burn, etc. 
var group1 = fold1
  .multiply(ee.Image(10))
  .add(fire1);
 

// Map.addLayer(group1.eq(50), {min: 0, max: 1, palette: ['white', 'black']}, 'group 50');
// CONTINUE HERE

