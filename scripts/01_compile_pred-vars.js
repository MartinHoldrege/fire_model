/*
Martin Holdrege

Script started 2/25/2022

Purpose--rasters of predictor variables (climate,
annual herbacious biomass, perennial herbacious biomass, and shrub cover).
Then output this data all in the same projection/scale, for the same grid cells, to be 
used to create a relationship between fire and and the predictor variabls, for use
in the cheatgrass/fire module of STEPWAT2

Data is masked to the extent of the sagebrush biome
*/

// User defined variables -------------------------------------

var createCharts = false; //whether to create timeseries charts
// whether to run the code the exports the files
var run = false; 
// date range
var startYear = 1986;
//var startYear = 2018; // short time period for testing
var endYear = 2019;
var startDate = ee.Date.fromYMD(startYear, 1, 1);
var endDate = ee.Date.fromYMD(endYear, 12, 31); 

var resolution = 1000; // this is the resolution of the daymet product (i.e. which
// is coarser than the RAP data)

// visualization params ----------------------------------------
var fireVis = {min: 0, max: 100, palette: ['white', 'red']};
var coverVis = {min: 0, max: 100, palette: ['white', 'green']}; 



// read in data -------------------------------------------------

var path = 'projects/usgs-gee-drylandecohydrology/assets/cheatgrass_fire/';
// read in annual grass data

// functions
var fns = require("users/mholdrege/cheatgrass_fire:src/ee_functions.js");

// Climate (daymet) data
var clim = require("users/mholdrege/cheatgrass_fire:scripts/00_daymet_summaries.js");

// Mask of the sagebrush region
var m = require("users/mholdrege/cheatgrass_fire:scripts/00_biome_mask.js");
var mask = m.mask;
var region = m.region;


Map.addLayer(region, {}, 'region', false);

// rap data 
// rangeland analysis platform, for cover data
var rap1 = ee.ImageCollection('projects/rangeland-analysis-platform/vegetation-cover-v3')
  .filterDate(startDate,  endDate)
  .map(function(image) {
    return ee.Image(image).toFloat();
  }); // to avoid incompatible data types on export

  /************************************************
 * 
 * extent of stepwat2 simulations
 * 
 *  region of the stepwat2 change rasters
   extracted in R by running
  r <- rast("../SEI/data_raw/stepwat_change_rasters/CheatgrassFire_Cheatgrass_ChangePropHistoricalMax_RCP45_2030-2060_CanESM2.tif")
  r@ptr$extent$as.points() %>% print(digits = 20)
 * 
 ************************************************
 */
 
// reading upscaled stepwat upscaled data to get extent, resolution, and projection
// note--the specific image doesn't matter, they should have all the same attributes
var sw2Image = ee.Image('projects/usgs-gee-drylandecohydrology/assets/SEI/stepwat_change_rasters/ClimateOnly_Sagebrush_ChangePropHistoricalMax_RCP85_2070-2100_inmcm4');

var sw2Projection = sw2Image.projection().crs().getInfo();
var sw2Resolution = sw2Image.projection().nominalScale().getInfo();  
var sw2Region = ee.Geometry.Polygon(
        [[[-127.974999999999994, 29.233333333000004],
          [-127.974999999999994, 51.600000000000001],
          [-98.733333333999994, 51.600000000000001],
          [-98.733333333999994, 29.233333333000004]]], sw2Projection, false);

Map.addLayer(region, {}, 'biome region', false);
Map.addLayer(sw2Region, {}, "sw2Sim region", false);

/************************************************
 * 
 * Prepare vegetation data
 * 
 ************************************************
 */
 
// rap cover data -------------------------------------------


Map.addLayer(mask, {palette: ['black', 'white']}, 'mask', false);

// masking out non-sagebrush
var rap2 = rap1
  .filterBounds(region)
  .map(function(image){
    return ee.Image(image).updateMask(mask);
  });

var rapMed = rap2.median();
var rapMedUnmasked = rap1.filterBounds(sw2Region).median();
var rapMaxUnmasked = rap1.filterBounds(sw2Region).max();

//print('RAP', rapMed);

Map.addLayer(rapMed.select('AFG'), coverVis, 'Annuals', false);
Map.addLayer(rapMed.select('PFG'), coverVis, 'Perennials', false);
Map.addLayer(rapMed.select('SHR'), coverVis, 'Shrubs', false);
Map.addLayer(rapMaxUnmasked.select('SHR'), coverVis, 'Max Shrub cover', false);

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
var bioMasked = biomass.map(function(x) {
    return ee.Image(x).updateMask(mask);
});
var bioMed = bioMasked.median();
var bioMedUnmasked = biomass.median();
var bioMaxUnmasked = biomass.max();

Map.addLayer(bioMed.select('pfgAGB'), 
  {min: 0, max: 4000, palette: bamakoReverse}, 
  'perennials median', false);
Map.addLayer(bioMed.select('afgAGB'), 
  {min: 0, max: 4000, palette: bamakoReverse}, 
  'annuals median', false);
  
// Examine RAP time series ----------------------------------------------

if (createCharts) {
var createChart = function(image, title, vAxis){
  var out = ui.Chart.image.series(image, region, ee.Reducer.mean(), resolution)
    .setOptions({
      title: title,
      vAxis: {title: vAxis},
      hAxis: {title: 'year', format: 'YYYY'},
      trendlines: {0: {
        color: 'CC0000'
      }}
    });
  return out;
};

// herbacious biomass charts
var pfts = ['afgAGB', 'pfgAGB'];
for (var i = 0; i < pfts.length; i++) {
  var image = bioMasked.select(pfts[i]);
  print(createChart(image, pfts[i], pfts[i]));
}

// shrub cover chart
print(createChart(rap2.select('SHR'), 'Shrub cover', '% cover'));

  
}
/************************************************
 * 
 * Prepare human modification data
 * 
 ************************************************
 */
 
// at the moment looks like I only have access to the 2019 human modification file
var hMod = ee.Image('users/DavidTheobald8/HM/HM_US_v3_dd_' + '2019' + '_90_60ssagebrush');
Map.addLayer(hMod, {}, 'humanMod', false)


/************************************************
 * 
 * Export data
 * 
 * pastick-etal-mask in the file names, just means that this output data was masked
 * to the extend of the pastick fire probability dataset. 
 * 
 ************************************************
 */
 
 // export files so that this script can be sourced from other scripts
exports.bioMasked = bioMasked;
 
 
 if (run) {
 // export files to drive
 
var crs = 'EPSG:4326';
var maskString = '_sagebrush-biome-mask_v1';
var extentStringSw2 = '_sw2sim-extent_v1';

// rap data

var rapOut = bioMed.select(['afgAGB', 'pfgAGB'])
  .addBands(rapMed.select('SHR').rename('shrCover')); //Shrub cover
  
// rap data for soilwat2 extent, primaryily for assessing the relationship
// between stepwat simulated biomass and RAP data
var rapOutSw2 = bioMaxUnmasked
  .select(['afgAGB', 'pfgAGB'])
  .rename(['afgAGBMax', 'pfgAGBMax'])
  .addBands(
    bioMedUnmasked
      .select(['afgAGB', 'pfgAGB'])
      .rename(['afgAGBMed', 'pfgAGBMed'])
  )
  .addBands(rapMedUnmasked.select('SHR').rename('shrCoverMed'))
  .addBands(rapMaxUnmasked.select('SHR').rename('shrCoverMax'));

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



// RAP data (unmasked) for the whole extent of stepwat2 upscaling
Export.image.toDrive({
  image: rapOutSw2,
  description: 'RAP_afgAGB-pfgAGB-shrCover_' + startYear + '-' + endYear + '_med-max_' + resolution + 'm' + extentStringSw2,
  folder: 'cheatgrass_fire',
  maxPixels: 1e13, 
  scale: sw2Resolution,
  region: sw2Region,
  crs: sw2Projection,
  fileFormat: 'GeoTIFF'
});



// sagebrush biome mask
Export.image.toDrive({
  image: rapOut,
  description: 'RAP_afgAGB-pfgAGB-shrCover_' + startYear + '-' + endYear + '_median_' + resolution + 'm' + maskString,
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
  
  // output for the extent of stepwat2 simulations
  // for use with upscaled stepwat2 simulation data
  Export.image.toDrive({
    image: climList[i],
    description: 'daymet_' + climDescription[i] + s + '_sw2sim-extent_v1',
    folder: 'cheatgrass_fire',
    maxPixels: 1e13, 
    scale: sw2Resolution,
    region: sw2Region,
    crs: sw2Projection,
    fileFormat: 'GeoTIFF'
  });
}


}

