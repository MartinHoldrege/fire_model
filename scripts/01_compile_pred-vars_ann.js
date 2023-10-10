/*
Martin Holdrege

Script started 2/25/2022

Purpose--rasters of predictor variables (climate,
annual herbacious biomass, perennial herbacious biomass).
Then output this data all in the same projection/scale, for the same grid cells, to be 
used to create a relationship between fire and and the predictor variabls, for use
in the cheatgrass/fire module of STEPWAT2.

Unlike the original script (01_compile_pred-vars.js) here biomass of of annuals and perennials
is calculated for every year, by averaging that year's biomass and the previous 3 years. 
Except in the case when a fire occurred in the previous two years then averages did not
inlcude the year of the fire or before that fire. 

Data is masked to the extent of the sagebrush biome
*/

// User defined variables -------------------------------------

// whether to run the code the exports the files
var run = false; 
// date range
//var startYear = 1986;
var startYear = 2018; // short time period for testing
var endYear = 2019;
var startDate = ee.Date.fromYMD(startYear, 1, 1);
var endDate = ee.Date.fromYMD(endYear, 12, 31); 

var resolution = 1000; // this is the resolution of the daymet product (i.e. which
// is coarser than the RAP data)

// read in data -------------------------------------------------

var path = 'projects/usgs-gee-drylandecohydrology/assets/cheatgrass_fire/';
// read in annual grass data

// functions
var fns = require("users/mholdrege/cheatgrass_fire:src/ee_functions.js");

// Climate (daymet) data
var clim = require("users/mholdrege/cheatgrass_fire:scripts/00_daymet_summaries.js");

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
 

// rap biomass data ---------------------------------------
// aboveground biomass of annual and perennial herbacious plants
// (looking at the RAP website, looks like this isn't available for shrubs)

// This section of code below i copied from here:
// https://code.earthengine.google.com/e48f56f7d2c16d53e09c4b8246104465

// Start copied code--

/* 

  Conversion of partitioned NPP (annuals forbs and grasses, perennial forbs and
  grasses) to aboveground biomass as described in Jones et al. (2021).

  Inputs: partitioned NPP, mean annual temperature

  Authors: Nathaniel Robinson, Matthew Jones, Brady Allred

  Contact:
    Matthew Jones (matt.jones@umontana.edu)
    Brady Allred (brady.allred@umontana.edu)
  
  Jones, M.O., N.P. Robinson, D.E. Naugle, J.D. Maestas, M.C. Reeves, R.W.
  Lankston, and B.W. Allred. 2021. Annual and 16-Day Rangeland Production
  Estimates for the Western United States. Rangeland Ecology & Management
  77:112–117. http://dx.doi.org/10.1016/j.rama.2021.04.003

*/

var npp = ee.ImageCollection("projects/rangeland-analysis-platform/npp-partitioned-v3")
  .select(['afgNPP', 'pfgNPP'])
  .filterDate(startDate,  endDate)
  .filterBounds(region);

var biomass = npp
  .map(fns.biomassFunction)
  .map(function(x) {
    return ee.Image(x).toFloat(); // to avoid incompatible datatypes error
  });

// add 2019 perennial forb and grass biomass (pfgAGB) to map
// bamako palette, from 'users/gena/packages:palettes'
var bamakoReverse = [ "00404D", "084449", "0F4845", "154C41", "1C513C",
  "235538", "2B5A34", "325F2F", "3A652A", "436A25", "4C7020", "56771A",  
  "617E14", "6C840E", "7A8B06", "878E03", "969206", "A89A14", "B9A525", 
  "CBB33A", "D9BF4F", "E3C961", "EDD375", "F6DC86", "FFE599" ].reverse();
  
Map.addLayer(biomass.filterDate('2019').select('pfgAGB'), 
  {min: 0, max: 4000, palette: bamakoReverse}, 
  'perennials2019', false);

// --- end copied code

//print('npp', npp);
//print('biomass', biomass);

// mask and calculate median
var bioM = biomass.map(function(x) {
  var year = ee.Number.parse(ee.Image(x).get('year'));
  return ee.Image(x)
    .updateMask(mask)
    .select(['pfgAGB', 'afgAGB'])
    // converting year property to numeric
    .set('year', year);
});


// biomass data--3 year averages ----------------------------------------------

// b/ want to have a 3 year average the first year of data will be 
// two years after the first year of available biomass data
var yearsShort = ee.List.sequence(startYear -2, endYear);


// 3 year averages
var bioM3AvgL = yearsShort.map(function(x) {
  var y3 = ee.Number(x);
  var y2 = y3.subtract(1);
  var y1 = y3.subtract(2);
  
  var f1 = cwfByYr.filter(ee.Filter.eq('year', y1)).first(); // fire in 1st year
  var f2 = cwfByYr.filter(ee.Filter.eq('year', y2)).first(); // fire in 2nd year
  var f3 = cwfByYr.filter(ee.Filter.eq('year', y3)).first(); // fire in 2nd year
  
  // creating masks
  var m1 = f1.eq(ee.Image(0)); // where there were no fires first year
  var m2 = f1.add(f2).eq(ee.Image(0)); // where there were no fires first or 2nd year
  
  // applying masks to the biomass data 
  var b1 = bioM.filter(ee.Filter.eq('year', y1))
    .first()
    .updateMask(m1);

  var b2 = bioM.filter(ee.Filter.eq('year', y2))
    .first()
    .updateMask(m2);
    
  var b3 = bioM.filter(ee.Filter.eq('year', y3))
    .first();
  
  // average biomass across 3 years except if fires occured in yrs 1 or 2 than the pixels
  // in the fire year (and following year if fire occured in yr 1) are masked out
  var bAvg = ee.ImageCollection.fromImages([b3, b2, b1])
    .mean()
    .addBands(f3)
    .copyProperties(b3);
    
  return bAvg;
});

// appending year to band names and converting to single image
var bioM3AvgImage = ee.ImageCollection(bioM3AvgL)
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
  
var bioM3AvgImage = bioM3AvgImage.regexpRename('^\\d+_', '');

print(bioM3AvgImage)
/************************************************
 * 
 * Prepare human modification data
 * 
 ************************************************
 */
 
// at the moment looks like I only have access to the 2019 human modification file
var hMod = ee.Image('users/DavidTheobald8/HM/HM_US_v3_dd_' + '2019' + '_90_60ssagebrush');
//Map.addLayer(hMod, {}, 'humanMod', false);

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


// export to drive 
// human modification dataset
Export.image.toDrive({
  image: hMod,
  description: 'HM_US_v3_dd_2019_60ssagebrush_' + resolution + 'm' + maskString,
  folder: 'cheatgrass_fire',
  maxPixels: 1e13, 
  scale: resolution,
  region: region,
  crs: crs,
  fileFormat: 'GeoTIFF'
});


// sagebrush biome mask
Export.image.toDrive({
  image: bioM3AvgImage,
  description: 'RAP_afgAGB-pfgAGB-fire_' + startYear + '-' + endYear + '_3yrAvg_' + resolution + 'm' + maskString,
  folder: 'cheatgrass_fire',
  maxPixels: 1e13, 
  scale: resolution,
  region: region,
  crs: crs,
  fileFormat: 'GeoTIFF'
});


// daymet data

var s =  '_' + startYear + '-' + endYear + '_' + resolution + 'm';

var climList = [clim.climYearlyAvg, clim.climSummerAvg, clim.climSpringAvg];
var climDescription = ['climYearlyAvg', 'climSummerAvg', 'climSpringAvg'];

for (var i = 0; i < climList.length; i++) {
  Export.image.toDrive({
    image: climList[i].updateMask(mask),
    description: 'daymet_' + climDescription[i] + s + maskString,
    folder: 'cheatgrass_fire',
    maxPixels: 1e13, 
    scale: resolution,
    region: region,
    crs: crs,
    fileFormat: 'GeoTIFF'
  });
  
}

}

