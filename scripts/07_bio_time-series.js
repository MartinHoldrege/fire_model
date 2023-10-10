/*
Purpose: look at the trends in annual/perennial biomass over time 
in pixels that burned and didn't burn in each of the 5 folds

Author: Martin Holdrege

Script Started Sept. 29, 2023
*/

// parameters -------------------------------------------------
var resolution = 30; 
var testRun = false;
// dependencies -----------------------------------------------

var fns = require('users/mholdrege/cheatgrass_fire:src/ee_functions.js');
// functions from the RR repo (for summarizing over years)
var rr = require("users/mholdrege/newRR_metrics:src/functions.js");

// predictor variable datasets
var preds = require('users/mholdrege/cheatgrass_fire:scripts/01_compile_pred-vars.js');

// fire datasets
var fire = require('users/mholdrege/cheatgrass_fire:scripts/01_compile_fire_data.js');

// Mask of the sagebrush region
var m = require("users/mholdrege/cheatgrass_fire:scripts/00_biome_mask.js");
var mask = m.mask;
var region = m.region;

// read in data ----------------------------------------------

// folds (raster created in 06_cross-validation_env-block_byNFire.rmd)
var fold1 = ee.Image(fns.path + 'cv_folds_v1').toInt();

// image collection (1 image per year) of biomass 
var bio1 = preds.bioMasked;

// modifications for test runs --------------------------------

if (testRun){
  var region = 
    ee.Geometry.Polygon(
        [[[-114.35831243496158, 41.63432829317286],
          [-114.35831243496158, 40.54135368551528],
          [-112.98502141933658, 40.54135368551528],
          [-112.98502141933658, 41.63432829317286]]], null, false);
        
  var resolution = 10000;        
}

// preparing grouping image -------------------------------------
var didBurn = fire.cwfFiresPerPixelM.gte(1);

// group index: folds are values 1-5 (then multipied by 10), and did burn is yes(1) and no(0)
// so 51 is fold 5 and burned, 30 is fold 3 and didn't burn, etc. 
var group1 = fold1
  .multiply(ee.Image(10))
  .add(didBurn)
  .rename('group');

// preparing biomass data ---------------------------------------

var bio2 = bio1.map(function(x) {
  return ee.Image(x).select(['afgAGB', 'pfgAGB'])
    .addBands(group1)
    .updateMask(mask);
});

var years = bio2.aggregate_array('year').map(function(x) {
  return ee.String(x);
});

if(testRun) {
  var years = years.slice(0, 2);
}

// mean yearly biomass ----------------------------------------

var meanByYearAfg = rr.mapOverYears(bio2, 'afgAGB', 'group', years, region, resolution);
var meanByYearPfg = rr.mapOverYears(bio2, 'pfgAGB', 'group', years, region, resolution);

var meanByYearComb = meanByYearAfg.merge(meanByYearPfg);

if(testRun) {
  print('merged', meanByYearComb);
}

// writing output ------------------------------------------------------------------

var yearStart = years.reduce('min').getInfo();
var yearEnd = years.reduce('max').getInfo();
var fileName = 'mean-ann-cover_by-fold-burn-status_' + yearStart + "_" + yearEnd + "_" + resolution + "m";
if(testRun) {
  fileName = 'testRun_' + fileName;
}

Export.table.toDrive({
  collection: meanByYearComb,
  description: fileName,
  folder: 'cheatgrass_fire',
  fileFormat: 'CSV'
});
  




