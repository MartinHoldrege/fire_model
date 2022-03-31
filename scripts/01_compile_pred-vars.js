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

// date range
var startYear = 1984;
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

var path = 'projects/gee-guest/assets/cheatgrass_fire/';
// read in annual grass data

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
  .filterBounds(region);
  

/************************************************
 * 
 * Prepare vegetation data
 * 
 ************************************************
 */
 
// rap cover data -------------------------------------------


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
// MH--function updated so that it calculates biomass as g/m^2
var biomassFunction = function(image) {
    
    var year = ee.Date(image.get('system:time_start')).format('YYYY');
    var matYear = mat.filterDate(year).first();
    var fANPP = (matYear.multiply(0.0129)).add(0.171).rename('fANPP'); // fraction of NPP to allocate aboveground
    
    var agb = image.multiply(0.0001) // NPP scalar 
                //.multiply(2.20462) // KgC to lbsC MH--i commented out these lines
                //.multiply(4046.86) // m2 to acres MH--i commented out these lines
                .multiply(1000) // MH--KgC to gC
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
var bioMasked = biomass.map(function(x) {
    return ee.Image(x).updateMask(mask);
});
var bioMed = bioMasked.median();

Map.addLayer(bioMed.select('pfgAGB'), 
  {min: 0, max: 4000, palette: bamakoReverse}, 
  'perennials median', false);
Map.addLayer(bioMed.select('afgAGB'), 
  {min: 0, max: 4000, palette: bamakoReverse}, 
  'annuals median', false);
  
// Examine RAP time series ----------------------------------------------

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


/************************************************
 * 
 * Export data
 * 
 * pastick-etal-mask in the file names, just means that this output data was masked
 * to the extend of the pastick fire probability dataset. 
 * 
 ************************************************
 */
 
 // export files to drive
 
var crs = 'EPSG:4326';
var maskString = '_sagebrush-biome-mask_v1';
// rap data

var rapOut = bioMed.select(['afgAGB', 'pfgAGB'])
  .addBands(rapMed.select('SHR').rename('shrCover')); //Shrub cover

// export to drive 
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

var s =  '_' + startYear + '-' + endYear + '_' + resolution + 'm' + maskString;

var climList = [clim.climYearlyAvg, clim.climSummerAvg, clim.climSpringAvg];
var climDescription = ['climYearlyAvg', 'climSummerAvg', 'climSpringAvg'];

for (var i = 0; i < climList.length; i++) {
  Export.image.toDrive({
    image: climList[i].updateMask(mask),
    description: 'daymet_' + climDescription[i] + s,
    folder: 'cheatgrass_fire',
    maxPixels: 1e13, 
    scale: resolution,
    region: region,
    crs: crs,
    fileFormat: 'GeoTIFF'
  });
}


