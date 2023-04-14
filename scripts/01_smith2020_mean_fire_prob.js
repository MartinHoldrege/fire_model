// Purpose: calculate mean annual fire probabilyt from Smith et al 2022
// and export at the same resolution/mask that we're using elsewhere

// Author: Martin Holdrege

// Date started: April 14, 2023


// params

var resolution = 1000;
var crs = 'EPSG:4326';
// Defines Mask of the sagebrush region
var m = require("users/mholdrege/cheatgrass_fire:scripts/00_biome_mask.js");

// image collection. relative annual fire probability from the Smith et al. 2022 manuscript
// (https://doi.org/10.1016/j.rama.2022.07.005)
var ic = ee.ImageCollection("users/the1joesmith/fire/fire_probability_jan1")
  // filtering to the approximate year range I'm pulling data for for other parts of
  // the project
  .filter(ee.Filter.lte('year', 2019))
  // currently only 1988 available, but putting this filter here in case ic changes
  .filter(ee.Filter.gte('year', 1887))
  .map(function(x) {
    return ee.Image(x).updateMask(m.mask)
  }); 


var meanImage = ic.mean();
Map.addLayer(meanImage, {min:0, max:1, palette: ["blue", "red"]}, 'fire probability')

Export.image.toDrive({
    image: meanImage,
    description: 'smith2022_mean_annual_fire_prob_1988-2019',
    folder: 'cheatgrass_fire',
    maxPixels: 1e13, 
    scale: resolution,
    region:  m.region,
    crs: crs,
    fileFormat: 'GeoTIFF'
});