/**
 * Martin Holdrege
 * 
 * Purpose: Compile observed fire occurence data. The end products 
 * of interest are whether a given pixel burned in a given year
 * (this is a simplification of the 01_compile_fire_data_ann.js script)
 * 
 *convert usgs combined wildland fire dataset into a raster
 * 
 * 
*/


// User define Variables

// date range
// this data includes 1984
var startYear = 1986;

var endYear = 2020;

var resolution = 1000;

var createCharts = true; //whether to create timeseries charts
// logical--whether to run export images
var run = false;

// read in data -------------------------------------------------

var fns = require("users/mholdrege/cheatgrass_fire:src/ee_functions.js");

var path = 'projects/usgs-gee-drylandecohydrology/assets/cheatgrass_fire/';
// read in annual grass data

// Mask for the extent of the sagebrush biome
var m = require("users/mholdrege/cheatgrass_fire:scripts/00_biome_mask.js");
var mask = m.mask;
var region = m.region;

// combined wildland fire dataset
// data from https://www.sciencebase.gov/catalog/item/61aa537dd34eb622f699df81
// I filtered this to only include data from 1984-2019, and exclude prescribed fire
// interagency fire perimeter history
var cwf = ee.FeatureCollection(path + 'fire_perims/usgs_combined_wildland_fire')
  .filterBounds(region);

// landsat burned area algorithm (used as part of the input to the Pastick paper)
// More info here: https://samapriya.github.io/awesome-gee-community-datasets/projects/lba/
// These were shapefiles which Pastick sent me that I ingested into ee 

// create list of dates
var years = ee.List.sequence(startYear, endYear);

// convert numbers to strings
var yearsString = years.map(function(x) {
    var out = ee.String(x)
      .replace('\\.\\d$', ''); 
    return out;
  });


// functions etc -------------------------------------------------

var setTimeStart = fns.setTimeStart;

var fc2image = function(fc) {
  //cells are painted if the centroid falls within the polgyon boundary
  //if fire occured then convert cell to 1
  // so bandNames are the same as output of fc2imageReduceToImage
  return ee.Image(0).paint(fc, 1).rename('first'); 
};


// total area of interest (i.e. unmasked pixels)
var calcTotalArea = function(area, key) {
  var totalArea = ee.Number(
    ee.Image(area).reduceRegion({
      reducer: ee.Reducer.sum(),
      geometry: region,
      maxPixels: 1e12,
      scale: resolution
    }).get(key)
    );
  return totalArea;
};

// objects used in calcPercArea function:

var areaImage = mask.multiply(ee.Image.pixelArea());
var totalArea = calcTotalArea(areaImage, 'b1'); // total (unmasked) area

// calc percent of total area that has burned
// note that areaImage needs to be in scope for this
// to run. 
var calcPercArea = function(image){
  var area = ee.Image(image).gt(0).multiply(areaImage);
  var out = calcTotalArea(area, 'first')
    .divide(totalArea)
    .multiply(100); // convert to percent
  return out;
};


/**************************************************
 * 
 * USGS combined wildland fire dataset
 * (this is now considered the best dataset), the
 * other datasets here are more of a legacy from when I didn't know
 * about this dataset (which combined MTBS and a number of other datasets
 * 
 * ***********************************************
 */
 
var cwfByYear = years.map(function(year) {
  return cwf.filter(ee.Filter.eq('Fire_Yr', year));
});

// convert polygons to image (rasters)
var cwfImageByYear = cwfByYear.map(fc2image)
  .zip(years) // combine two lists into one (each element of list is a list w/ 2 elements)
  .map(setTimeStart); //

// total number of fires per pixel
var cwfFiresPerPixel = ee.ImageCollection(cwfImageByYear).sum().toDouble();

// mask
var cwfImageByYearM = cwfImageByYear.map(function(x) {
    return ee.Image(x).updateMask(mask);  
});

var cwfFiresPerPixelM = cwfFiresPerPixel.updateMask(mask);
Map.addLayer(cwfFiresPerPixelM, {min:0, max: 5, palette: ['white', 'black']}, 'cwf fires per pixel', false);


var cwfPercAreaByYear = cwfImageByYearM.map(calcPercArea);

// figures ---------------------------------------------

// creating time series of % burned area by year, for each
// data set

var arrayList = [cwfPercAreaByYear];

var titleList = ['% of area burned per year (CWF)'];
  
// Note here putting EE objects inside of javascript lists
// this normally isn't a good idea but seems to work 
// (and function didn't), because ui.chart is client side?

if (createCharts) {
  
for (var i = 0; i < arrayList.length; i++) {
  var chart = ui.Chart.array.values({
  array: arrayList[i],
  axis: 0,
  xLabels: years
}).setChartType('ScatterChart')
  .setOptions({
    title: titleList[i],
    hAxis: {title: 'Year', format: '####'},
    vAxis: {title: '% total area'},
    legend: { position: "none" },
    pointSize: 4,
    trendlines: {0: {
        color: 'CC0000'
      }},
    lineWidth: 1
  });
  print(chart);
}

}

// save files --------------------

// export for use in other scripts
exports.startYear = startYear;
exports.endYear = endYear;
exports.yearsString = yearsString;
exports.cwfImageByYearM = cwfImageByYearM; 
exports.cwfFiresPerPixelM = cwfFiresPerPixelM;

// which method was used to convert polygons to rasters
var method = '_paint_';

var s =  '_' + startYear + '-' + endYear + '_' + resolution + 'm_sagebrush-biome-mask' + method + 'v1';

if (run) { // set to true of want to export. 
  
Export.image.toDrive({
  image: allFiresPerPixelM,
  description: 'cwf-mtbs-ifph-lba_fires-per-pixel' + s,
  folder: 'cheatgrass_fire',
  maxPixels: 1e13, 
  scale: resolution,
  region: region,
  crs: fns.crs,
  fileFormat: 'GeoTIFF'
});

}



