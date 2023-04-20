/********************************************************
 * 
 * Calculate Q values for annuals and perennials
 * Unlike for SEI calculations, the q curves here are applied 
 * to the raw (unsmoothed) RAP cover values
 * 
 * Author: Martin Holdrege
 * 
 * Date started 4/20/2023
 * 
*/


// User-defined variables.

var yearEnd = 2020  // this value is changed to make multi-year runs, e.g., 2017-2020 would= 2020
var yearStart = yearEnd - 3 // inclusive, so if -3 then 2017-2020, inclusive

var SEI = require("users/mholdrege/SEI:src/SEIModule.js"); // functions and other objects

var WAFWAecoregions = SEI.WAFWAecoregions; // polygons outlining the 3 regions

// load data ----------------------------------------------------------------------

// this currently loads v2, you may want to use v3
var ic = ee.ImageCollection('projects/rangeland-analysis-platform/vegetation-cover-v2') //

var rap = ic.filterDate(yearStart + '-01-01',  yearEnd + '-12-31').mean()

// apply ratio to rap & nlcd data
var rapAnnualG = rap.select('AFGC')
  .divide(100.0);

var rapPerennialG = rap.select('PFGC')
  .divide(100.0);


/**
 * Compute Q values
 * 
 * convert  % cover to quality using HSI curves
 * Note that remap values for HSI are grouped ecoregion specific: 1st column=Great Basin, 2nd column: Intermountain, 3rd column: Great Plains
 */
   
var Q2 = ee.Image(0.0).float();
var Q3 = ee.Image(0.0).float();

var lstEcoregionIds = ['00000000000000000000','00000000000000000001','00000000000000000002']; // GB, IM, Pl

// Looping through ecoregions
for (var e=1; e<=lstEcoregionIds.length; e++) {
  var ecoregion = WAFWAecoregions.filter(ee.Filter.eq('system:index', lstEcoregionIds[e-1])); //

// Q values for annuals
// raw2HSI is a function that converts cover to Q curve
// inputs are the cover layer, a nested list (lstAnnualG2Q in this case), and the ecoregion (e)
  var Q3x = SEI.raw2HSI(rapAnnualG, SEI.lstAnnualG2Q, e)
    .clip(ecoregion).unmask(0.0); // places that are not in the ecoregion become 0
  var Q3 = Q3.max(Q3x); 
  
  // Q values for perennials
  var Q2x = SEI.raw2HSI(rapPerennialG, SEI.lstPerennialG2Q, e)
    .clip(ecoregion).unmask(0.0);
  var Q2 = Q2.max(Q2x);

}

Map.addLayer(Q2.updateMask(SEI.mask), {min:0, max:1, palette: ["white", "black"]}, 'Perennials Q values');
Map.addLayer(Q3.updateMask(SEI.mask), {min:0, max:1, palette: ["white", "black"]}, "Annuals Q values");

 