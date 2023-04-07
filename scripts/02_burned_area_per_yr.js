
// parameters -------------------------------------------------------------------

var resolution = 10000

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

print()
var areaBurnedByYear = cwfImageByYearM.map(function(image) {
  var areaBurnedImage = areaImage.multiply(ee.Image(image));
  
  var area = // ee.Number(
    ee.Image(areaBurnedImage).reduceRegion({
      reducer: ee.Reducer.sum(),
      geometry: region,
      maxPixels: 1e12,
      scale: resolution
    })
//   );
  return area;
})

print(areaBurnedByYear);