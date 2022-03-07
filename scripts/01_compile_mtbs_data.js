// User define Variables

// date range
// this data includes 1984, but to make in comparable to other dfor the time
// being I'm doing 1985
var startYear = 1985;

var endYear = 2019;


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

// testing
Map.addLayer(ee.FeatureCollection(boundariesByYear.get(2)), {}, '2nd layer', false)
var test = ee.FeatureCollection(boundariesByYear.get(2));
Map.addLayer(zero.paint(test, 1), {min: 0, max: 1, palette: ['white', 'black']}, 'test paint', false);
// end testing

// CONTINUE here--the next step is to add an attribute of year to each
// image in the image collection (i.e. startTime), so that layers have time associated with 
// them--
var occurrenceByYear = boundariesByYear
  .map(function(fc) {
    return zero.paint(fc, 1); //.updateMask(mask); // if fire occured then convert cell to 1
  });

// setting the start date feature, as Jan 1, of the given year
var occurenceByYear = occurrenceByYear.zip(years) // combine two lists into one (each element of list is a list w/ 2 elements)
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

// Visualize data pixels


