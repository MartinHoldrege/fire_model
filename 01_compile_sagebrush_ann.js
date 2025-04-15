/*
Martin Holdrege

Script started April  15, 2025

Purpose--create raster of 3 year average of sagebrush cover,
this is for quantile mapping in the grazing_effects project. 
Quantile mapping was implemented using data from this project (i.e.
based on the Holdrege et al. 2024 science base data release for this paper). 
Here creating a comparable dataset for sagebrush cover, to keep methods
the same, this is the same workflow as used in the 01_compile_pred-vars_ann.js
script

Data is masked to the extent of the sagebrush biome
*/

// User defined variables -------------------------------------

// whether to run the code the exports the files
var run = false; 
// date range
var startYear = 1986;
//var startYear = 2016; // short time period for testing
var endYear = 2019;
var startDate = ee.Date.fromYMD(startYear, 1, 1);
var endDate = ee.Date.fromYMD(endYear, 12, 31); 

var resolution = 1000; // this is the resolution of the daymet product (i.e. which
// is coarser than the RAP data)

// read in data -------------------------------------------------

var path = 'projects/usgs-gee-drylandecohydrology/assets/cheatgrass_fire/';
// read in annual grass data

// functions
// fire data 
var f= require("users/mholdrege/cheatgrass_fire:scripts/01_compile_fire_data.js");

// Mask of the sagebrush region
var m = require("users/mholdrege/cheatgrass_fire:scripts/00_biome_mask.js");
var mask = m.mask;
var region = m.region;


Map.addLayer(region, {}, 'region', false);

/************************************************
 * 
 * Prepare fire data
 * (combined wildland fire datase)
 * 
 ************************************************
 */

// annual fire presence/abbs
var cwfByYr = ee.ImageCollection(f.cwfImageByYearM)
  .map(function(x) {
    // adding a year property (for later linking of collections)
    var image = ee.Image(x);
    var year = ee.Number.parse(ee.Date(image.get('system:time_start')).format('YYYY'));
    return image.rename('burned').set('year', year);
  });

/************************************************
 * 
 * Prepare vegetation data
 * 
 ************************************************
 */
 

// RCMAP sagebrush cover---------------------------------------


var sage1 = ee.ImageCollection('USGS/NLCD_RELEASES/2023_REL/RCMAP/V6/COVER')
  .select('rangeland_sagebrush');

// mask 
var sage2 = sage1.map(function(x) {
  var year = ee.Number.parse(ee.Image(x).get('system:index'));
  return ee.Image(x)
    .updateMask(mask)
    // converting year property to numeric
    .set('year', year);
});


// biomass data--3 year averages ----------------------------------------------

// b/ want to have a 3 year average the first year of data will be 
// two years after the first year of available biomass data
var sageMShort = sage2.filter(ee.Filter.rangeContains('year', startYear +2 , endYear));

// 3 year averages
// mapping over image collection not list because may help reduce memory problems (?)
var sageM3Avg = sageMShort.map(function(x) {
  var b3 = ee.Image(x); // biomass in the '3rd' year
  var y3 = ee.Number(b3.get('year'));
  var y2 = y3.subtract(1);
  var y1 = y3.subtract(2);
  
  var f1 = cwfByYr.filter(ee.Filter.eq('year', y1)).first(); // fire in 1st year
  var f2 = cwfByYr.filter(ee.Filter.eq('year', y2)).first(); // fire in 2nd year
  var f3 = cwfByYr.filter(ee.Filter.eq('year', y3)).first(); // fire in 3rd year
  
  // creating masks
  
  var m2 = f2.eq(ee.Image(0)); // were there were no fires in the 2nd year
  var m1 = f1.add(f2).eq(ee.Image(0)); // were there were no fires first year or the following year
  
  // applying masks to the biomass data 
  var b1 = sage2.filter(ee.Filter.eq('year', y1))
    .first()
    .updateMask(m1);

  var b2 = sage2.filter(ee.Filter.eq('year', y2))
    .first()
    .updateMask(m2);
    
  // average biomass across 3 years except if fires occured in yrs 1 or 2 than the pixels
  // in the fire year (and following year if fire occured in yr 1) are masked out
  var sageAvg = ee.ImageCollection.fromImages([b3, b2, b1])
    .mean()
    .copyProperties(b3);
    
  return sageAvg;
});

// appending year to band names and converting to single image
var sageM3AvgImage = sageM3Avg
  .map(function(x) {
    var image = ee.Image(x);
    var oldNames = image.bandNames();
    var yr = ee.String(image.get('year'));
    var newNames = oldNames.map(function(x) {
      return ee.String(x).cat(ee.String('_')).cat(yr);
    });
    return image.rename(newNames);
  })
  .toBands();
  
var sageM3AvgImage = sageM3AvgImage.regexpRename('^\\d+_', '');

print(sageM3AvgImage)


/************************************************
 * 
 * Export data
 * 
 * 
 ************************************************
 */


 if (run) {
 // export files to drive
 
var crs = fns.crs;
var maskString = '_sagebrush-biome-mask_v2';

// sagebrush biome mask
Export.image.toDrive({
  image: bioM3AvgImage.toFloat(),
  description: 'RCMAPV6_sagebrush-cover_' + startYear + '-' + endYear + '_3yrAvg_' + resolution + 'm' + maskString,
  folder: 'cheatgrass_fire',
  maxPixels: 1e13, 
  scale: resolution,
  region: region,
  crs: crs,
  fileFormat: 'GeoTIFF'
});


}

