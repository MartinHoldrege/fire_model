/**
 * Martin Holdrege
 * 
 * Purpose: Compile observe fire occurence data. The end products 
 * of interest are counts of how many times grid cells burned over
 * over a defined period of years
 * 
 * 1st--Compile the Monitoring Trends in Burn Severity (MTBS) data
 * into a raster dataset. Including a raster where, each cell is a count of the number
 * fires that have occured over the last decades. MTBS only shows
 * fires >1000 acres
 * 
 * 2nd--Do the same for the Interagency fire perimeter history dataset.
 * This dataset may not be as clean, with less data check
 *  compared to the MTBS data but it
 * does also include small fires. One concern is that it may
 * have more of a bias over time (e.g. if reporting standars have improved).
 * It was acquired from here:
 * https://data-nifc.opendata.arcgis.com/datasets/nifc::interagency-fire-perimeter-history-all-years/about
 * 
 * 3rd--for each year determine if a cell burned according to MTBS and/or 
 * the interagency fire perim dataset. Then sum that to get a count of number of years
 * burned. This could be helpful to get a more complete measure of the 
 * total amount of area burned. 
 * 
 * Note--consider carefully whether to call a pixel burned only if the pixel
 * center intersect the fire polygon, or if any part of it intersects the polygon.
 * This is more important for large (e.g. 1km pixels)
*/


// User define Variables

// date range
// this data includes 1984, but to make in comparable to other data for the time
// being I'm doing 1985
var startYear = 1985;

var endYear = 2019;

var resolution = 1000;

// logical--whether to run export and graph making code
var run = false; 

// read in data -------------------------------------------------

var path = 'projects/gee-guest/assets/cheatgrass_fire/';
// read in annual grass data

// Mask for the extent of the sagebrush biome
var m = require("users/mholdrege/cheatgrass_fire:scripts/00_biome_mask.js");
var mask = m.mask;
var region = m.region;

// Mtbs data (https://samapriya.github.io/awesome-gee-community-datasets/projects/mtbs/)
var mtbs_boundaries = ee.FeatureCollection("projects/sat-io/open-datasets/MTBS/burned_area_boundaries");
//  .filterBounds(region);
  
Map.addLayer(mtbs_boundaries, {}, 'mtbs', false);

// interagency fire perimeter history
var ifph = ee.FeatureCollection(path + 'fire_perims/InteragencyFirePerimeterHistory')
  .filterBounds(region);

Map.addLayer(ifph, {}, 'ifph', false);

// landsat burned area algorithm (used as part of the input to the Pastick paper)
// More info here: https://samapriya.github.io/awesome-gee-community-datasets/projects/lba/

var lba = ee.Image('users/keikonomura/fire/LBA_CU_2019_20200415_C01_V01_BP_L8');
// STOP--continue here--I need to figure out what the names of the images in this collection
// are. Extract the ones the names of the ones of interest (i.e. burn classification, no area)
// and read them into a list. 
print(lba);
var ic = ee.ImageCollection('users/keikonomura/fire');
print(ic);
var assetList = ee.data.listAssets('users/keikonomura/fire/');

print(assetList);

// functions etc -------------------------------------------------

// create list of dates
var years = ee.List.sequence(startYear, endYear);

// convert a feature collection (fc) to an image 
var fc2image = function(fc) {
    var fc2 = ee.FeatureCollection(fc)
      .map(function(feature) {
        return ee.Feature(feature).set('fire', 1);
      }); // setting a dummy variable to reduceToImage
    // Note reduceToImage will count any polygon that touches the fire polygon count as
    // burned
    var out = fc2.reduceToImage(['fire'], ee.Reducer.first())
      .unmask(0); // so that non-fire pixels are 0
   
    // alternatively use paint() cells are painted if the centroid falls within the polgyon boundary
    // return zero.paint(fc, 1); // if fire occured then convert cell to 1
    return out;
  };
  
// set the start date of an image to Jan 1 of a given year
// x (input) is a list with two items, the first is an 
// image the 2nd is the year 
var setTimeStart = function(x) {
    var year = ee.List(x).get(1);
    var startDate = ee.Date.fromYMD(year, 1, 1); 
    var image = ee.Image(ee.List(x).get(0))
      .set('system:time_start', startDate);
    return image;
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
 *  MTBS data
 * 
 * ***********************************************
 */

// map over years

// List where each element, is a feature collection of the fires that year
var boundariesByYear = years.map(function(year) {
  var startTime = ee.Date.fromYMD(year, 1, 1).millis();
  var endTime = ee.Date.fromYMD(year, 12, 31).millis();
  var out = mtbs_boundaries
  //filtering by ignition date
    .filter(ee.Filter.gte('Ig_Date', startTime))
    .filter(ee.Filter.lte('Ig_Date', endTime));
  return out;
});

var zero = ee.Image(0);

// convert each years feature collection to a binary image
var mtbsImageByYear = boundariesByYear.map(fc2image)
  .zip(years) // combine two lists into one (each element of list is a list w/ 2 elements)
  .map(setTimeStart); // setting the start date feature, as Jan 1, of the given year

print('mtbsImageByYear', mtbsImageByYear);

var mtbsFiresPerPixel = ee.ImageCollection(mtbsImageByYear).sum().toDouble();
Map.addLayer(mtbsFiresPerPixel, {min:0, max: 5, palette: ['white', 'black']}, 'fires per pixel', false);

// mask data 

var mtbsImageByYearM = mtbsImageByYear.map(function(x) {
    return ee.Image(x).updateMask(mask);  
});

var mtbsFiresPerPixelM = mtbsFiresPerPixel.updateMask(mask);

// Calculate burned area ------------------------------------------------------------


Map.addLayer(areaImage, {palette: ['white', 'black']}, 'area', false);

//print(occurenceByYear)
// % of total area that burned each year
var mtbsPercAreaByYear = mtbsImageByYearM.map(calcPercArea);


/**************************************************
 * 
 *  Interagency fire perimeter data
 * 
 * ***********************************************
 */

// convert numbers to strings
var yearsString = years.map(function(x) {
    var out = ee.String(x)
      .replace('\\.\\d$', ''); 
    return out;
  });
  
// List where each element, is a feature collection of the fires that year
var ifphByYear = yearsString.map(function(year) {
  return ifph.filter(ee.Filter.eq('FIRE_YEAR', year));
});

// convert polygons to image (rasters)
var ifphImageByYear = ifphByYear.map(fc2image)
  .zip(years) // combine two lists into one (each element of list is a list w/ 2 elements)
  .map(setTimeStart); // setting the start date feature, as Jan 1, of the given year
  

Map.addLayer(ee.Image(ifphImageByYear.get(0)), {palette: ['white', 'black']}, 'ifph single year', false);

// total number of fires per pixel
var ifphFiresPerPixel = ee.ImageCollection(ifphImageByYear).sum().toDouble();

Map.addLayer(ifphFiresPerPixel, {min:0, max: 5, palette: ['white', 'black']}, 'ifph fires per pixel', false);

// mask data 

var ifphImageByYearM = ifphImageByYear.map(function(x) {
    return ee.Image(x).updateMask(mask);  
});

var ifphFiresPerPixelM = ifphFiresPerPixel.updateMask(mask);

var ifphPercAreaByYear = ifphImageByYearM.map(calcPercArea);

/**************************************************
 * 
 *  Combine Monitoring trends in burn severity and
 *  Interagency fire perimeter data
 * 
 * ***********************************************
 */

var combImageByYear = ifphImageByYear.zip(mtbsImageByYear).map(function(x) {
  var ifphImage = ee.Image(ee.List(x).get(0)); // ifph fires (presence/absence for the given year)
  var imtbsImage = ee.Image(ee.List(x).get(1)); // mtbs fires (pres/abs) for the same year
  // pixels where ifph and/or imtbs dataset shows it burned. 
  var out = ifphImage.add(imtbsImage).gte(1);
  return out;
});

// sum across years
var combFiresPerPixel = ee.ImageCollection(combImageByYear).sum().toDouble();

// apply mask
var combImageByYearM = combImageByYear.map(function(x) {
    return ee.Image(x).updateMask(mask);  
});

var combFiresPerPixelM = combFiresPerPixel.updateMask(mask);

// % of area burned per year
var combPercAreaByYear = combImageByYearM.map(calcPercArea);

// figures ---------------------------------------------

// creating time series of % burned area by year, for each
// data set

var arrayList = [mtbsPercAreaByYear, ifphPercAreaByYear, combPercAreaByYear];

var titleList = ['% of area burned per year (MTBS)', 
  '% of area burned per year (IFPH)',
  '% of area burned per year (IFPH and MTBS combined)'];
  
// Note here putting EE objects inside of javascript lists
// this normally isn't a good idea but seems to work 
// (and function didn't), because ui.chart is client side?

if (run) {
  
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

// combining into a single image to export
var allFiresPerPixel = mtbsFiresPerPixel.rename('mtbs')
  .addBands(ifphFiresPerPixel.rename('ifph'))
  .addBands(combFiresPerPixel.rename('comb'));
  
// export for use in other scripts
exports.allFiresPerPixel = allFiresPerPixel; // not masked so can be used for other extents
exports.startYear = startYear;
exports.endYear = endYear;

var allFiresPerPixelM = allFiresPerPixel.updateMask(mask);


var crs = 'EPSG:4326';
var s =  '_' + startYear + '-' + endYear + '_' + resolution + 'm_sagebrush-biome-mask_v1';

if (run) { // set to true of want to export. 
  
Export.image.toDrive({
  image: allFiresPerPixelM,
  description: 'mtbs-ifph-comb_fires-per-pixel' + s,
  folder: 'cheatgrass_fire',
  maxPixels: 1e13, 
  scale: resolution,
  region: region,
  crs: crs,
  fileFormat: 'GeoTIFF'
});

}



