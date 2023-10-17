// Create a geotiff where each 1km grid cell contains the proportion of 
// area that burned in a given year


// dependencies -----------------------------------------------------------
var m = require("users/mholdrege/cheatgrass_fire:scripts/00_biome_mask.js");
var mask = m.mask;
var region = m.region;
// functions
var fns = require("users/mholdrege/cheatgrass_fire:src/ee_functions.js");

// read in the data ------------------------------------------------------------

// at 30 m res 1, means pixel burned 0 means not
// but averaged at higher levels in the pyramid so then it can be interpretted as proportion burned
// this can be confirmed by zooming in and out on the map and using the 'selector'


var burn = ee.Image('users/MartinHoldrege/cheatgrass_fire/cwf_annual-burned_30m');
Map.addLayer(burn.select('burned_1988'), {min: 0, max: 1, palette: ['red', 'blue']}, 'burn fraction');

// save the data --------------------------------------------------------------------
var scale = 1000
var maskString = '_sagebrush-biome-mask_v1'
Export.image.toDrive({
  image: burn.updateMask(mask),
  description: 'cwf_annual-proportion-burned_' + 1984 + '-' + 2019 + '_' + scale + 'm' + maskString,
  folder: 'cheatgrass_fire',
  maxPixels: 1e13, 
  scale: scale,
  region: region,
  crs: fns.crs,
  fileFormat: 'GeoTIFF'
});

