
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
var resolution = 1000; // set to 1000 but for now to speed computation

var startYear = 1986; // althought the fire data goes to 1984, the RAP data only goes to 1986

var endYear = 2019

// region
var mask = require("users/mholdrege/cheatgrass_fire:scripts/00_biome_mask.js")
//var region = mask.region; 
 
// for testing setting the region to a small area
var region = 
    ee.Geometry.Polygon(
        [[[-116, 41],
          [-116, 40],
          [-114, 40],
          [-114, 41]]], null, false);

Map.addLayer(region, {}, 'region', false)

// function
var fns = require("users/mholdrege/cheatgrass_fire:src/ee_functions.js")

// read in fire data
var fire = require("users/mholdrege/cheatgrass_fire:scripts/01_compile_fire_data.js");

// list of whether or not a fire occured in a given pixel, each year, masked to the biome
// USGS combined wildland fire dataset
var cwfImageByYearM = fire.cwfImageByYearM;

//read in RAP biomass data
var pred = require("users/mholdrege/cheatgrass_fire:scripts/01_compile_pred-vars.js");

var years = ee.List.sequence(startYear, endYear);

Map.addLayer(ee.Image(cwfImageByYearM.get(0)), {min: 0, max: 1, palette: ['white', 'black']}, startYear + 'fires', false)

/* 
Step 1:

Create image collection of the cumulative number of fires up to that year

*/

var cwfImageByYearMCol = ee.ImageCollection(cwfImageByYearM);

// cumulative number of fires
var cwfCumByYear0 = years.map(function(yr) {
  
  var start = ee.Date.fromYMD(startYear, 1, 1);
  var end = ee.Date.fromYMD(yr, 12, 31);
  var filteredCollection = cwfImageByYearMCol
    .filterDate(start, end);
    
  var out = ee.Image(filteredCollection.sum());
  return out;
});

var cwfCumByYear = cwfCumByYear0.zip(years).map(fns.setTimeStart);

var cwfCumByYearCol = ee.ImageCollection(cwfCumByYear);

// this map should look identical to the 'cwf fires per pixel' map that is loaded
// when 'require' above is run. Here this is the cumulative number of fires that have occured by 2019


/*
  step 2:
  create an image with multiple bands, for each band it is the number of years with
  that number of fires e.g. the band fires_1 shows the number of years after the first
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

var numYrs2 = sumFireYrs(0); // testing

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
Step 3:

*/

var bioMasked2 = pred.bioMasked.select(['afgAGB', 'pfgAGB'])
  .sort('year');// making sure these are sorted
var bioMList1 = bioMasked2.toList(99);
print(bioMList1.length())
print(cwfCumByYear.length())
