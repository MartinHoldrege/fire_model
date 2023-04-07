/**
 * Purpose: Calculate the total area burned per year within the sagebrush
 * biome based on the usgs combined wildland fire dataset
 * 
 * Author: Martin Holdrege
 * 
 * Date started: April 7, 2023
*/



// parameters -------------------------------------------------------------------

var resolution = 30

// dependencies -----------------------------------------------------------------
var fire = require("users/mholdrege/cheatgrass_fire:scripts/01_compile_fire_data.js");
var m = require("users/mholdrege/cheatgrass_fire:scripts/00_biome_mask.js");
var region = m.region;

// read in fire data -------------------------------------------------------------
// list of whether or not a fire occured in a given pixel, each year, masked to the biome
// USGS combined wildland fire dataset
var cwfImageByYearM = fire.cwfImageByYearM;
print(cwfImageByYearM)


// calculate area burned ------------------------------------------------------------

var areaImage = m.mask.multiply(ee.Image.pixelArea());

var areaBurnedByYear = ee.FeatureCollection(cwfImageByYearM).map(function(image) {
  var areaBurnedImage = areaImage.multiply(ee.Image(image));
  
  // Extract the year from the time_start property
  var year = ee.Date(image.get('system:time_start')).get('year');
  
  // not worrying about the crs here--b/ using the area image (based on area of each pixel)
  // so should be accurate 
  var area = ee.Image(areaBurnedImage).reduceRegion({
      reducer: ee.Reducer.sum(),
      geometry: region,
      maxPixels: 1e12,
      scale: resolution
    }).get('b1');
    
  // Return a feature with both the year and area values
  return ee.Feature(null, 
  {'year': year, 
  // convert m2 to ha
  'area_ha': ee.Number(area).divide(10000)
  });

});

// output assets ----------------------------------------------------------------------

Export.table.toDrive({
  collection: areaBurnedByYear,
  description: 'area_burned_by_yr_cwf_' + resolution + 'm',
  folder: 'cheatgrass_fire',
  fileFormat: 'CSV'
});