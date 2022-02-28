/*
Martin Holdrege

Script started 2/25/2022

Purpose--compile fire probability rasters, and rasters of predictor variables (climate,
annual herbacious cover/biomass, perennial herbacious cover/biomass, and shrub biomass).
Then output this data all in the same projection/scale, for the same grid cells, to be 
used to create a relationship between fire and and the predictor variabls, for use
in the cheatgrass/fire module of STEPWAT 2

TO DO: annual and perennial grasses/forb biomass can be calculated from RAP (see below)
For shrubs should use the shrub biomass layer. 
Note when ouputing the rasters--scale to resolution of the climate data
and use an equal area projection (I think this could be better
so analyses don't weight some grid-cells more)
*/

// User defined variables -------------------------------------

// date range
var startYear = 1985;
//var startYear = 2018; // short time period for testing
var endYear = 2019;
var startDate = ee.Date.fromYMD(startYear, 1, 1);
var endDate = ee.Date.fromYMD(endYear, 12, 31); 

var resolution = 1000; // this is the resolution of the daymet product (i.e. which
// is corser than the other dataset used)

// visualization params ----------------------------------------
var fireVis = {min: 0, max: 100, palette: ['white', 'red']};
var coverVis = {min: 0, max: 100, palette: ['white', 'green']}; 


// read in data -------------------------------------------------

var path = 'projects/gee-guest/assets/cheatgrass_fire/';
// read in annual grass data

// fire probability
// data from downloaded from: https://doi.org/10.5066/P9ZN7BN8
// modelled fire probability 1985-2019

var fire1 = ee.Image(path + 'fire_probability/LT_Wildfire_Prob_85to19_v1-0');

Map.addLayer(ee.Image(0), {palette: ['white']}, 'blank bankground', false);
Map.addLayer(fire1, fireVis, 'fire probability', false);

// bounding box of the fire data (copied from the metadata of the fire dataset)
var region = ee.Geometry({
  'type': 'Polygon',
  'coordinates':
  [[[-121.87704, 47.84742], // NW corner
  [-104.87570, 36.90701], // SE corner
  [-110, 40]]],// just another point (inside the bounding box), so creates a triangle, so get get bounds
  'proj': fire1.projection(),
}).bounds();

Map.addLayer(region, {}, 'region', false);


// rap data 
// rangeland analysis platform, for cover data
var rap1 = ee.ImageCollection('projects/rangeland-analysis-platform/vegetation-cover-v3')
  .filterDate(startDate,  endDate)
  .filterBounds(region);

/************************************************
 * 
 * Prepare vegetation data
 * 
 ************************************************
 */
 
 
// rap cover data -------------------------------------------

var mask = fire1.mask(); // 0's are areas where the fire data set is masked, 1's are unmasked
Map.addLayer(mask, {palette: ['black', 'white']}, 'mask', false);

// masking out non-sagebrush
var rap2 = rap1.map(function(image){
  return ee.Image(image).updateMask(mask);
});

var rapMed = rap2.median();

print('RAP', rapMed);

Map.addLayer(rapMed.select('AFG'), coverVis, 'Annuals', false);
Map.addLayer(rapMed.select('PFG'), coverVis, 'Perennials', false);
Map.addLayer(rapMed.select('SHR'), coverVis, 'Shrubs', false);


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
var mat = ee.ImageCollection("projects/rangeland-analysis-platform/gridmet-MAT");


// biomass conversion function
// input: two band image (afgNPP, pfgNPP) from projects/rangeland-analysis-platform/npp-partitioned-v2
// output: three band image, aboveground biomass (afgAGB, pfgAGB, herbaceousAGB)
var biomassFunction = function(image) {
    
    var year = ee.Date(image.get('system:time_start')).format('YYYY');
    var matYear = mat.filterDate(year).first();
    var fANPP = (matYear.multiply(0.0129)).add(0.171).rename('fANPP'); // fraction of NPP to allocate aboveground
    
    var agb = image.multiply(0.0001) // NPP scalar 
                .multiply(2.20462) // KgC to lbsC
                .multiply(4046.86) // m2 to acres
                .multiply(fANPP)  // fraction of NPP aboveground
                .multiply(2.1276) // C to biomass
                .rename(['afgAGB', 'pfgAGB'])
                .copyProperties(image, ['system:time_start'])
                .set('year', year);

    var herbaceous = ee.Image(agb).reduce(ee.Reducer.sum()).rename(['herbaceousAGB']);
    
    agb = ee.Image(agb).addBands(herbaceous);

    return agb;
};

var biomass = npp.map(biomassFunction);

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
var bioMed = biomass.median()
  .updateMask(mask);
  
Map.addLayer(bioMed.select('pfgAGB'), 
  {min: 0, max: 4000, palette: bamakoReverse}, 
  'perennials median', false);
Map.addLayer(bioMed.select('afgAGB'), 
  {min: 0, max: 4000, palette: bamakoReverse}, 
  'annuals median', false);


/************************************************
 * 
 * Prepare Daymet climate data
 * 
 ************************************************
 */

// Annual temp and precipitation ****************

var daymet = ee.ImageCollection("NASA/ORNL/DAYMET_V4")
  .filterBounds(region)
  .filterDate(startDate, endDate)
  // set mask
  .map(function(image) {
    return image.updateMask(mask);
  });


// Not sure if there is a problem with speed using using select (string), 
// inside a map() call if the string is a client side string,
// so doing the select here
var daymetP = daymet.select('prcp');

var daymetT = daymet.select(['tmax', 'tmin']);

// make a list with years
var years = ee.List.sequence(startYear, endYear);

//  avg temp, and total ppt for each year
var climYearlyList = years.map(function(y) {
  // Precip for each day of year for the given year
  var filteredP = daymetP.filter(ee.Filter.calendarRange(y, y, 'year'));
  // Temp for each day of year for the given year
  var filteredT = daymetT.filter(ee.Filter.calendarRange(y, y, 'year'));
  var precip = filteredP.sum(); // total ppt for the year
  var temp = filteredT.mean(); // mean of min/max temp for the year
   // casting to double so datatype of temp and precp same. otherwise can't export to drive
  var out = ee.Image(precip).addBands(temp.toDouble());
  return out;
});


// avg yearly ppt and temp across years
var climYearlyAvg = ee.ImageCollection(climYearlyList)
  .mean();

Map.addLayer(climYearlyAvg.select('prcp'),
  {min: 50, max: 700, palette: ['white', 'blue']}, 'Annual ppt', false);
  
 Map.addLayer(climYearlyAvg.select('tmax'),
  {min: 3, max: 30, palette: ['blue', 'red']}, 'Annual tmax'); 
// Summer temp and precip ******************************

// This function builds a function that calculates seasonal climate for a given
// month range
var createSeasonClimFun = function(startMonth, endMonth) {
  var outFun = function(y) {
    var filteredP = daymetP.filter(ee.Filter.calendarRange(y, y, 'year'))
      .filter(ee.Filter.calendarRange(startMonth, endMonth, 'month'));
    var filteredT = daymetT.filter(ee.Filter.calendarRange(y, y, 'year'))
      .filter(ee.Filter.calendarRange(startMonth, endMonth, 'month'));
    var precip = filteredP.sum(); // total ppt for the year
    var temp = filteredT.mean(); // mean of min/max temp for the year
    var out = ee.Image(precip).addBands(temp.toDouble());
  return out;
  };
  return outFun;
};

// function to calculate summer climate (June-Aug)
var calcSummerClim = createSeasonClimFun(ee.Number(6), ee.Number(8));

//  avg temp, and total ppt for each year
var climSummerList = years.map(calcSummerClim);

// avg summer ppt and temp across years
var climSummerAvg = ee.ImageCollection(climSummerList)
  .mean();
  
// Spring temp and precip ******************************
  
// function to calculate springr climate (march - may)
var calcSpringClim = createSeasonClimFun(ee.Number(3), ee.Number(5));

//  avg temp, and total ppt for each year
var climSpringList = years.map(calcSpringClim);

// avg spring ppt and temp across years
var climSpringAvg = ee.ImageCollection(climSpringList)
  .mean();

// print('summer climate', climSummerAvg);
/************************************************
 * 
 * Export data
 * 
 ************************************************
 */
 
var crs = 'EPSG:4326';

// rap data

var rapOut = bioMed.select(['afgAGB', 'pfgAGB'])
  .addBands(rapMed.select('SHR').rename('shrCover')); //Shrub cover

// export to drive 
Export.image.toDrive({
  image: rapOut,
  description: 'RAP_afgAGB-pfgAGB-shrCover_' + startYear + '-' + endYear + '_median_' + resolution + 'm_v1',
  folder: 'gee',
  maxPixels: 1e13, 
  scale: resolution,
  region: region,
  crs: crs,
  fileFormat: 'GeoTIFF'
});

// daymet data

var s =  '_' + startYear + '-' + endYear + '_' + resolution + 'm_v1';

var climList = [climYearlyAvg, climSummerAvg, climSpringAvg];
var climDescription = ['climYearlyAvg', 'climSummerAvg', 'climSpringAvg'];

for (var i = 0; i < climList.length; i++) {
  Export.image.toDrive({
    image: climList[i],
    description: 'daymet_' + climDescription[i] + s,
    folder: 'gee',
    maxPixels: 1e13, 
    scale: resolution,
    region: region,
    crs: crs,
    fileFormat: 'GeoTIFF'
  });
}

