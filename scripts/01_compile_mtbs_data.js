/**
 * Martin Holdrege
 * 
 * Purpose: Compile the Monitoring Trends in Burn Severity (MTBS) data
 * into a raster dataset. Including a raster where, each cell is a count of the number
 * fires that have occured over the last decades. 
*/

// User define Variables

// date range
// this data includes 1984, but to make in comparable to other dfor the time
// being I'm doing 1985
var startYear = 1985;

var endYear = 2019;

var resolution = 1000;
// read in data -------------------------------------------------

var path = 'projects/gee-guest/assets/cheatgrass_fire/';
// read in annual grass data

// fire probability
// data from downloaded from: https://doi.org/10.5066/P9ZN7BN8
// modelled fire probability 1985-2019
// I'm must using this data for the mask
var mask = ee.Image(path + 'fire_probability/LT_Wildfire_Prob_85to19_v1-0')
  .mask();

// bounding box of the fire data (copied from the metadata of the Pastick fire dataset)
var region = ee.Geometry({
  'type': 'Polygon',
  'coordinates':
  [[[-121.87704, 47.84742], // NW corner
  [-104.87570, 36.90701], // SE corner
  [-110, 40]]],// just another point (inside the bounding box), so creates a triangle, so get get bounds
  'proj': mask.projection(),
}).bounds();

// Mtbs data (https://samapriya.github.io/awesome-gee-community-datasets/projects/mtbs/)
var mtbs_boundaries = ee.FeatureCollection("projects/sat-io/open-datasets/MTBS/burned_area_boundaries")
  .filterBounds(region);
  
Map.addLayer(mtbs_boundaries, {}, 'mtbs', false);

// testing (remove later)
var testDate = ee.Date(713775600000);
print(testDate.millis());
var test = mtbs_boundaries.filter(ee.Filter.eq('Ig_Date', testDate.millis()))
  .filter(ee.Filter.eq('Map_ID', 7920));

//Map.centerObject(test);
Map.addLayer(test, {}, "test", false);
print(ee.Number(testDate));
// end testing

// create list of dates

var years = ee.List.sequence(startYear, endYear);


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

var occurrenceByYear = boundariesByYear
  .map(function(fc) {
    // here cells are painted if the centroid falls within the polgyon boundary
    // (see: https://www.patreon.com/posts/vector-to-raster-51128079).
    // use reduceToImage() if want to count any cells that touch polygon
    return zero.paint(fc, 1); //.updateMask(mask); // if fire occured then convert cell to 1
  });

// setting the start date feature, as Jan 1, of the given year
// combine two lists into one (each element of list is a list w/ 2 elements)
var occurenceByYear = occurrenceByYear.zip(years)
  .map(function(x) {
    var year = ee.List(x).get(1);
    var startDate = ee.Date.fromYMD(year, 1, 1); 
    var image = ee.Image(ee.List(x).get(0))
      .set('system:time_start', startDate);
    return image;
  });


var occurrenceByYear = ee.ImageCollection(occurrenceByYear);

Map.addLayer(ee.ImageCollection(occurrenceByYear), {palette: ['white', 'black']}, 'single year', false);

var firesPerPixel = occurrenceByYear.sum();
Map.addLayer(firesPerPixel, {min:0, max: 5, palette: ['white', 'black']}, 'fires per pixel', false);

// mask data 

var occurenceByYearM = occurenceByYear.map(function(x) {
    return ee.Image(x).updateMask(mask);
});

var firesPerPixelM = firesPerPixel.updateMask(mask);

// Calculate burned area ------------------------------------------------------------

var areaImage = mask.multiply(ee.Image.pixelArea());
Map.addLayer(areaImage, {palette: ['white', 'black']}, 'area', false);

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
    

var totalArea = calcTotalArea(areaImage, 'b1'); // total (unmasked) area

//print(occurenceByYear)
// % of total area that burned each year
var percAreaByYear = occurenceByYearM.map(function(image){
  var area = ee.Image(image).gt(0).multiply(areaImage);
  var out = calcTotalArea(area, 'constant')
    .divide(totalArea)
    .multiply(100); // convert to percent
  return out;
});

var yearsString = years.map(function(x) {
    var out = ee.String(x)
      .replace('\\.\\d$', ''); 
    return out;
  });
  
// Combing year, and % of area burned into a dictionary
var percDict = ee.Dictionary.fromLists({
  keys: yearsString,
  values: percAreaByYear});
  
//print(percDict);

// figures ---------------------------------------------

// create figure of percent burned area by year 
var chart = ui.Chart.array.values({
  array: percDict.values(),
  axis: 0,
  xLabels: years
}).setChartType('ScatterChart')
  .setOptions({
    title: '% of area burned per year',
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
