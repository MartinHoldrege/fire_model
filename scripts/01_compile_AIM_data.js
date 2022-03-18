/************************************************
 * 
 * AIM veg and climate data
 * 
 * Author: Martin Holdrege
 * 
 * Purpose: Pull in AIM vegetation monitoring 
 * sites, and export that data along with the
 * daymet climate normals for those sites. 
 * 
 ************************************************
 */
 
  // the daymet data has 1km res, so I don't think it makes sense to go smaller
 var resolution = 1000;
// read in data -------------------------------------

// climate normals calculated from daymet data
var clim = require("users/mholdrege/cheatgrass_fire:scripts/00_daymet_summaries.js");

// misc. useful functions
var util = require("users/mholdrege/cheatgrass_fire:src/ee_functions.js");

// AIM vegetation data
var aim1 = ee.FeatureCollection('BLM/AIM/v1/TerrADat/TerrestrialAIM');


// filter data ----------------------------------------
// keep sites with sagebrush
var aim2 = aim1.filter(ee.Filter.gt('SagebrushCover_AH', 0));

print('example aim point', aim2.first());
Map.addLayer(aim2, {}, "Aim sites w/ sagebrush", false);

Map.addLayer(aim1.filter(ee.Filter.eq('SagebrushCover_AH', 0)), 
  {}, "Aim sites w/o sagebrush", false);
  
// Add daymet climate data to AIM data------------------

/**
 * Function to Extract climate information for each feature, and
 * rename properties
 * 
 * image --image of climate data
 * 
 * fc-- feature collection to reduce to
 * 
 * string--string to paste to propert names being created
 * 
*/
var addClimateToFc = function(image, fc, string) {
  // Spring climate
  var newNames = image.bandNames().map(function(x) {
    return ee.String(x).cat(string);
  });
  var out = ee.Image(image.rename(newNames)).reduceRegions({
      collection: fc, 
      reducer:ee.Reducer.mean(), 
      scale: resolution
  });
  return out;
};

// adding Spring climate data to AIM sites
var aim3 = addClimateToFc(clim.climSpringAvg, aim2, 'Spring');

// adding Summer climate data to AIM sites
var aim4 = addClimateToFc(clim.climSummerAvg, aim3, 'Summer');

// yearly climate data
var aim5 = addClimateToFc(clim.climYearlyAvg, aim4, 'Yearly');


// rename
var test = aim5.first();
print(test);





