/**
 * 
 *  Martin Holdrege
 * 
 * Script started: March 25, 2022
 * 
 * Purpose--create a mask of the sagebrush biome to be used in subsequent
 * scripts (and a polygon of the sagebrush biome).
 * Note--the input files I'm using are ones that David Theobald
 * used in his SEI scripts
 * 
 *  * This script should sourced in other scripts using e.g:
 * 
 * var mask = require("users/mholdrege/cheatgrass_fire:scripts/00_biome_mask.js")
*/ 

// outline of sagebrush biome
var biome = ee.FeatureCollection("projects/gee-guest/assets/SEI/US_Sagebrush_Biome_2019"); // provided by DT
var region = biome.geometry();

/// from USGS GAP land cover	
var LC = ee.Image("USGS/GAP/CONUS/2011");


// MH--remap converts selected values (which are turndra landcover) to 1, everything else becomes masked
// MH--unmask(0) replaces all masked values with 0.
// MH--eq(0), returning 1 for all cell values that are 0 (i.e. not tundra), 0 otherwise (i.e. flipping the 0 to 1 and 1 to 0)
var tundra = LC.remap([149,151,500,501,502,503,504,505,506,507,549,550,551],[1,1,1,1,1,1,1,1,1,1,1,1,1])
  .unmask(0).eq(0);

var rangeMask = ee.Image('users/chohnz/reeves_nlcd_range_mask_union_with_playas'); // mask from Maestas, Matt Jones

// MH -- here 1's are rangelands, and everything else is masked out
var rangeMaskx = rangeMask.eq(0) // MH returns 1 for ranglelands (ie pixels  that are not water bodies, forests etc._
  .multiply(tundra) // MH here 0 is tundra, so turning tundra cells to 0 by multiplications
  .selfMask() // MH uses the raster values as the mask. i.e. all 0s (not rangelands), are masked out
  .clip(biome);


// Export file so they can be used 
exports.mask = rangeMaskx;
exports.region = region;