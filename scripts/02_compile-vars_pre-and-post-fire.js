
/*
pseudo code:

collection 1
Create image collection of cumumulative number of fires up to a given year
By looping through each year and and suming from year 1 to that year (i.e. summing yes/no
fire rasters).

collection 2
get raster of number of fires before first fire
  filter for sites with 1 or more fires, sum the number of pixels with cumulative value of 1
  (i.e. use .eq(1) or whatever, and summing. Repeat this for num years before 2nd fire, etc.
  
Step 3--RAP data
prior to first fire = multiply each yearly RAP layer by 1 if it is pre fire, 0 otherwise (based on
collection), the sum across those rasters, and then divide by collection 2. 

Step 4--repeat step 3 for 2nd, 3rd, etc fire. each time masking out cells that have less then the
target number of fires--to reduce computation. 

Step5--output rasters--they will be pfgAGB prior to first fire, prior to 2nd fire (if it existed),
and prior to 


*/


// read in fire data
var fire = require("users/mholdrege/cheatgrass_fire:scripts/01_compile_fire_data.js");

// list of whether or not a fire occured in a given pixel, each year, masked to the biome
// USGS combined wildland fire dataset
var cwfImageByYearM = fire.cwfImageByYearM;

var startYear = fire.startYear;

var endYear = fire.endYear

var years = ee.List.sequence(startYear, endYear);

Map.addLayer(ee.Image(cwfImageByYearM.get(0)), {min: 0, max: 1, palette: ['white', 'black']}, startYear + 'fires', false)

/* 
Step 1:

Create image collection of the cumulative number of fires up to that year

*/

var cwfImageByYearMCol = ee.ImageCollection(cwfImageByYearM);

// cumulative number of fires
var cwfCumByYear = years.map(function(yr) {
  
  var start = ee.Date.fromYMD(startYear, 1, 1);
  var end = ee.Date.fromYMD(yr, 12, 31);
  var filteredCollection = cwfImageByYearMCol
    .filterDate(start, end);
    
  var out = ee.Image(filteredCollection.sum());
  return out;
});

// this map should look identical to the 'cwf fires per pixel' map that is loaded
// when 'require' above is run. Here this is the cumulative number of fires that have occured by 2019
Map.addLayer(ee.Image(cwfCumByYear.get(35)), {min: 0, max:5 , palette: ['white', 'black']}, 'cwf cumulative fires', false)


