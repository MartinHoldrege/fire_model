
/*
pseudo code:

collection 1
Create image collection of cumumulative number of fires up to a given year
By looping through each year and and suming from year 1 to that year (i.e. summing yes/no
fire rasters).

collection 2
get raster of number of years before first fire
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


// user defined variables
var resolution = 100000 // set to 1000 but for now to speed computation

// region
var mask = require("users/mholdrege/cheatgrass_fire:scripts/00_biome_mask.js")
//var region = mask.region; 
 
// for testing setting the region to a small area
var region = 
    /* color: #d63000 */
    /* displayProperties: [
      {
        "type": "rectangle"
      }
    ] */
    ee.Geometry.Polygon(
        [[[-119.67368848912835, 43.63807917713379],
          [-119.67368848912835, 40.77431886708674],
          [-114.42222364537835, 40.77431886708674],
          [-114.42222364537835, 43.63807917713379]]], null, false);


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


/*
step 2

*/
// cumulative fires in the last year (i.e., this is the total number of fires over the time period)
var cwfLastYear = ee.Image(cwfCumByYear.get(-1))
Map.addLayer(cwfLastYear, {min: 0, max:5 , palette: ['white', 'black']}, 'cwf cumulative fires', false);

var maxFires =   cwfLastYear.reduceRegion({
    reducer: ee.Reducer.max(),
    geometry: region,
    scale: resolution, // to speed computation for now
    maxPixels: 1e12,
    bestEffort: true
  });
  
var maxFires = ee.Number(maxFires.get('first'))
var numFiresSeq = ee.List.sequence(1, maxFires)
print(maxFires);

