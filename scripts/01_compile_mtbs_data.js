
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


var test = mtbs_boundaries.select('Event_ID', 'WY4462410655519920814');
Map.centerObject(test);
//Map.addLayer(test, {}, "first")