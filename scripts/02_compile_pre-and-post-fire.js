
/*
  Purpose: Seperately calculate biomass based on number of fires that
  have occured
  
  Script started: 6/3/2022
  
  Author: Martin Holdrege

  Rational: biomass (especially of annuals), may change depending on if there has
  been a fire so don't want to average biomass before and after a fire. 

  Step 1:
  Create image collection of the cumulative number of fires up to that year
  By looping through each year and and summing from year 1 to that year (i.e. summing yes/no
  fire rasters).
  
  step 2:  Number of years images. 
  Create an image with multiple bands, each band is the number of years 
  with the given number of cumulative fires.
   e.g. the band fires_1 shows the number of years after the first
  fire (year of burn inclusive) but before the 2nd fire.
    
  Step 3: Calculate mean RAP biomass based on number of fires
  
  Seperately calculate mean biomass for before and after fires
  I.e. one biomass layer provides mean biomass when there have been 0 fires (i.e. fire_0),
  another when there has been 1 fire (but not 2) (i.e. fire_1),
  another for when there has been 2 fires, but not 3 (i.e. fire_3), etc. 
  
  Step 4: save the data
*/


// user defined variables
var resolution = 1000; // set to 1000 but for now to speed computation

var startYear = 1986; // although the fire data goes to 1984, the RAP data only goes to 1986

var endYear = 2019;

// region
var mask = require("users/mholdrege/cheatgrass_fire:scripts/00_biome_mask.js");
var region = mask.region; 
 
// for testing setting the region to a small area (this region does contain fires)
/*
var region = 
    ee.Geometry.Polygon(
        [[[-116, 41],
          [-116, 40],
          [-114, 40],
          [-114, 41]]], null, false);
*/

Map.addLayer(region, {}, 'region', false);

// function
var fns = require("users/mholdrege/cheatgrass_fire:src/ee_functions.js");

// read in fire data
var fire = require("users/mholdrege/cheatgrass_fire:scripts/01_compile_fire_data.js");

// list of whether or not a fire occured in a given pixel, each year, masked to the biome
// USGS combined wildland fire dataset
var cwfImageByYearM = fire.cwfImageByYearM;

//read in RAP biomass data
var pred = require("users/mholdrege/cheatgrass_fire:scripts/01_compile_pred-vars.js");

var years = ee.List.sequence(startYear, endYear);

Map.addLayer(ee.Image(cwfImageByYearM.get(0)), 
  {min: 0, max: 1, palette: ['white', 'black']}, startYear + 'fires', false);

/* 
Step 1:

Create image collection of the cumulative number of fires up to that year

*/

var cwfImageByYearMCol = ee.ImageCollection(cwfImageByYearM);

// cumulative number of fires
// because iterating over years, the output will be properly ordered
var cwfCumByYear0 = years.map(function(yr) {
  
  var start = ee.Date.fromYMD(startYear, 1, 1);
  var end = ee.Date.fromYMD(yr, 12, 31);
  var filteredCollection = cwfImageByYearMCol
    .filterDate(start, end);
    
  var out = ee.Image(filteredCollection.sum());
  return out;
});

// setting the start time of each of the images
var cwfCumByYear = cwfCumByYear0.zip(years).map(fns.setTimeStart);

var cwfCumByYearCol = ee.ImageCollection(cwfCumByYear);

// this map should look identical to the 'cwf fires per pixel' map that is loaded
// when 'require' above is run. Here this is the cumulative number of fires that have occured by 2019


/*
  step 2:  Number of years images. 
  Create an image with multiple bands, each band is the number of years 
  with the given number of cumulative fires.
   e.g. the band fires_1 shows the number of years after the first
  fire (year of burn inclusive) but before the 2nd fire. Pixels where the value would be 0, are
  masked out
*/

// cumulative fires in the last year (i.e., this is the total number of fires over the time period)
var cwfLastYear = ee.Image(cwfCumByYear.get(-1));
Map.addLayer(cwfLastYear, {min: 0, max:5 , palette: ['white', 'black']}, 'cwf cumulative fires', false);

// max number of fires to have occurred in any grid cell
var maxFires =   cwfLastYear.reduceRegion({
    reducer: ee.Reducer.max(),
    geometry: region,
    scale: resolution, // to speed computation for now
    maxPixels: 1e12,
    bestEffort: true
  });
  
var maxFires = ee.Number(maxFires.get('first')); // convert to a number

var numFireSeq = ee.List.sequence(ee.Number(0), maxFires);
//print(numFiresSeq);


var sumFireYrs = function(numFire) {
  var numYrs = cwfCumByYearCol.map(function(image) {
    // cells that have a cumulative number of fires equal to numFire are set to 1
    // everything else is masked out
    var out = ee.Image(image).eq(ee.Number(numFire)).selfMask();
    return out;
  }).sum(); // sum across the years
  
  var bandString = ee.String('fire_').cat(ee.Number(numFire).format('%.0f'));
  // rename the band of the image to note how many fires the year count
  //refers to for example fires_1 means this is the number of years after the first
  // fire (year of burn inclusive) but before the 2nd fire
  return numYrs.rename(bandString);
};


//print(numYrs2)
var numYrsList = numFireSeq.map(sumFireYrs);

var numYrsImage0 = ee.ImageCollection(numYrsList).toBands(); // convert to single image

// rename bands so don't have leading image numbers
var newBandNames = numYrsImage0
  .bandNames()
  .map(function(x) {
    return ee.String(x).replace('.+_fire_', 'fire_');
  });
  
var numYrsImage = numYrsImage0.rename(newBandNames);
//print(numYrsImage.bandNames());
Map.addLayer(numYrsImage.select('fire_2'), 
  {min:0, max: 36, palette: ['white', 'black']}, 'fire_2', false);

/*
Step 3: Calculate mean RAP biomass based on number of fires

Seperately calculate mean biomass for before and after fires
I.e. one biomass layer shows biomass when there have been 0 fires (i.e. fire_0),
another when there has been 1 fire (but not 2) (i.e. fire_1),
another for when there has been 2 fires, but not 3 (i.e. fire_3), etc. 
*/

// making sure RAP data is properly ordered
var bioMasked2 = pred.bioMasked.select(['afgAGB', 'pfgAGB'])
  .sort('year');
var bioMList1 = bioMasked2.toList(999);

// these list need to be the same length and in the same order
print('RAP data length', bioMList1.length());
print('fire data length', cwfCumByYear.length());

// list of lists, where each element contains a list (for the given year)
// with one image of the cumulative num of fires, the other is biomass for that year
var cwfBioList = cwfCumByYear.zip(bioMList1);

var calcBioByNumFire = function(numFire) {
  
  // list of  biomass images, where pixels that haven't
  // had numFire cumulative fires have been masked
  var bioFireMaskedList = cwfBioList.map(function(x) {
    // first element is the cwf cumulative fire image
    var cwfImage = ee.Image(ee.List(x).get(0));
    var bioImage = ee.Image(ee.List(x).get(1)); //2nd is rap biomass
    
    var fireMask = cwfImage.eq(ee.Number(numFire));
    
    // masking out pixels that haven't had numFire of cumulative fires
    var bioFireMasked = bioImage.updateMask(fireMask);
    
    return bioFireMasked;
  });
  
  // mean across years of pixels that have had numFire cumulative fires
  var out = ee.ImageCollection(bioFireMaskedList).mean();
  
  // rename band names so that they lead with the cumulative
  // number of firest (e.g. pfgAGB becomes fire_1_pfgAGB)
  var oldNames = out.bandNames();

  var newNames = oldNames.map(function(x) {
    var out = ee.String('fire_')
      .cat(ee.Number(numFire).format('%.0f'))
      .cat(ee.String("_"))
      .cat(ee.String(x));
    return out;
  });

  return out.rename(newNames); 
};


var bioByNumFireList = numFireSeq.map(calcBioByNumFire);

// collapse into a single image
var bioByNumFire0 = ee.ImageCollection(bioByNumFireList).toBands(); 

// rename bands so don't have leading image numbers
var bioNewBandNames = bioByNumFire0
  .bandNames()
  .map(function(x) {
    return ee.String(x).replace('.+_fire_', 'fire_');
  });
  
var bioByNumFire = bioByNumFire0.rename(bioNewBandNames);

Map.addLayer(bioByNumFire.select('fire_3_afgAGB'), 
  {min: 0, max: 100, palette: ['white', 'green']}, 'afgAGB 3 fires', false);
  

/*
  Output data
*/

var crs = 'EPSG:4326';
var maskString = '_sagebrush-biome-mask_v1';
//var maskString = '_test-region';

// RAP biomass - sagebrush biome mask
Export.image.toDrive({
  image: bioByNumFire,
  description: 'RAP_afgAGB-pfgAGB_byNFire_' + startYear + '-' + endYear + '_mean_' + resolution + 'm' + maskString,
  folder: 'cheatgrass_fire',
  maxPixels: 1e13, 
  scale: resolution,
  region: region,
  crs: crs,
  fileFormat: 'GeoTIFF'
});

// Number of years with given number of cumulative fire
// (from step 2)
Export.image.toDrive({
  image: bioByNumFire,
  description: 'cwf_numYrsCumulativeFires_' + startYear + '-' + endYear + '_' + resolution + 'm' + maskString,
  folder: 'cheatgrass_fire',
  maxPixels: 1e13, 
  scale: resolution,
  region: region,
  crs: crs,
  fileFormat: 'GeoTIFF'
});





