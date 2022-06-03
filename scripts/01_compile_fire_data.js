/**
 * Martin Holdrege
 * 
 * Purpose: Compile observed fire occurence data. The end products 
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
 * center intersect the fire polygon (i.e. paint()), or if any part of it intersects the polygon.
 * (i.e. reduceToPolygon)
 * This is more important for large (e.g. 1km pixels)
*/


// User define Variables

// date range
// this data includes 1984
var startYear = 1984;

var endYear = 2019;

var resolution = 1000;

// logical whether to use paint (if false use reduceToImage) when converting polygons
// to rasters
var usePaint = true; 

var createCharts = false; //whether to create timeseries charts
// logical--whether to run export images
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

// creating paths for each of the LBA assets
var preString = ee.String(path + 'LBA/LBA_CU_');
var postString = ee.String('_20200415_C01_V01_BF_labeled');
var lbaPaths = yearsString.map(function(x) {
  return preString.cat(ee.String(x)).cat(postString);
});

// Reading in LBA polygons

// not sure why this isn't working without getinfo()
var lbaByYear = lbaPaths.getInfo().map(function(x) {
  return ee.FeatureCollection(ee.String(x));
});
var lbaByYear = ee.List(lbaByYear);
//print('fc', ee.FeatureCollection(lbaByYear.get(0)).first())


// functions etc -------------------------------------------------

var fc2imageReduceToImage = function(fc) {
      var fc2 = ee.FeatureCollection(fc)
      .map(function(feature) {
        return ee.Feature(feature).set('fire', 1);
      }); // setting a dummy variable to reduceToImage
    // Note reduceToImage will count any polygon that touches the fire polygon count as
    // burned
    var out = fc2.reduceToImage(['fire'], ee.Reducer.first())
      .unmask(0);
    return out;
};

var fc2imagePaint = function(fc) {
  //cells are painted if the centroid falls within the polgyon boundary
  //if fire occured then convert cell to 1
  // so bandNames are the same as output of fc2imageReduceToImage
  return zero.paint(fc, 1).rename('first'); 
};

// convert a feature collection (fc) to an image 
var fc2image = function(fc) {
  // convert to an image using paint() or reduceToImage()
  var out = ee.Algorithms.If(usePaint, fc2imagePaint(fc), fc2imageReduceToImage(fc));
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

//print('mtbsImageByYear', mtbsImageByYear);

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

//print(mtbsImageByYearM)
/**************************************************
 * 
 *  Interagency fire perimeter data
 * 
 * ***********************************************
 */


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

/**************************************************
 * 
 * Landsat Burned Area data
 * 
 * ***********************************************
 */
 
 // List where each element, is a feature collection of the fires that year

// convert polygons to image (rasters)
var lbaImageByYear = lbaByYear.map(fc2image)
  .zip(years) // combine two lists into one (each element of list is a list w/ 2 elements)
  .map(setTimeStart); // setting the start date feature, as Jan 1, of the given year
  

Map.addLayer(ee.Image(lbaImageByYear.get(0)), {palette: ['white', 'black']}, 'lba single year', false);

// total number of fires per pixel
var lbaFiresPerPixel = ee.ImageCollection(lbaImageByYear).sum().toDouble();

Map.addLayer(lbaFiresPerPixel, {min:0, max: 5, palette: ['white', 'black']}, 'lba fires per pixel', false);

// mask data 

var lbaImageByYearM = lbaImageByYear.map(function(x) {
    return ee.Image(x).updateMask(mask);  
});

var lbaFiresPerPixelM = lbaFiresPerPixel.updateMask(mask);

var lbaPercAreaByYear = lbaImageByYearM.map(calcPercArea);


// figures ---------------------------------------------

// creating time series of % burned area by year, for each
// data set

var arrayList = [mtbsPercAreaByYear, ifphPercAreaByYear, 
                  lbaPercAreaByYear, cwfPercAreaByYear];

var titleList = ['% of area burned per year (MTBS)', 
  '% of area burned per year (IFPH)',
  '% of area burned per year (LBA)',
  '% of area burned per year (CWF)'];
  
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

// combining into a single image to export
var allFiresPerPixel = mtbsFiresPerPixel.rename('mtbs')
  .addBands(ifphFiresPerPixel.rename('ifph'))
  .addBands(lbaFiresPerPixel.rename('lba'))
  .addBands(cwfFiresPerPixel.rename('cwf'));
  
// export for use in other scripts
exports.allFiresPerPixel = allFiresPerPixel; // not masked so can be used for other extents
exports.startYear = startYear;
exports.endYear = endYear;
exports.cwfImageByYearM = cwfImageByYearM; 
exports.

var allFiresPerPixelM = allFiresPerPixel.updateMask(mask);


var crs = 'EPSG:4326';

// which method was used to convert polygons to rasters
if (usePaint) {
 var method = '_paint_';
} else {
 var method = '_reduceToImage_';
}

var s =  '_' + startYear + '-' + endYear + '_' + resolution + 'm_sagebrush-biome-mask' + method + 'v1';

if (run) { // set to true of want to export. 
  
Export.image.toDrive({
  image: allFiresPerPixelM,
  description: 'cwf-mtbs-ifph-lba_fires-per-pixel' + s,
  folder: 'cheatgrass_fire',
  maxPixels: 1e13, 
  scale: resolution,
  region: region,
  crs: crs,
  fileFormat: 'GeoTIFF'
});

}



