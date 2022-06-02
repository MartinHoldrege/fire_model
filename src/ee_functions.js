/**
 * The ee module contains misc javascript functions for use
 * in other javascript earth engine scripts in this repository.
 * Module created by Martin Holdrege
 * @module src/ee_functions.js
 */


/**
 * Generic Function to remove a property from a feature
 * 
 * function copied from here: https://gis.stackexchange.com/questions/321724
 * /removing-property-from-feature-or-featurecollection-using-google-earth-engine
 * 
 * @param {ee.Feature} feat the feature to remove feature from
 * @parm {string) property The name of the property to remove
 * @return {ee.Feature}
 * 
*/
var removeProperty = function(feat, property) {
  var properties = feat.propertyNames()
  var selectProperties = properties.filter(ee.Filter.neq('item', property))
  return ee.Feature(feat).select(selectProperties)
}

exports.removeProperty = removeProperty; // not exporting above b/ fun called below

/**
 * Function to rename a property of a feature
 * 
 * @param {ee.Feature} feat the feature to rename the property in
 * @parm {string) newName new name to use for property
 * @parm {string) oldName old name of property 
 * @return {ee.Feature}
 * 
*/
exports.renameProperty = function(feat, newName, oldName) {
  var feat2 = feat.set(newName, feat.get(oldName));
  var out = removeProperty(feat2, oldName);
  return out;
};

/* 
  Function to convert npp to aboveground biomass

  Conversion of partitioned NPP (annuals forbs and grasses, perennial forbs and
  grasses) to aboveground biomass as described in Jones et al. (2021).

  Inputs: partitioned NPP, mean annual temperature

  Authors: Nathaniel Robinson, Matthew Jones, Brady Allred

  Contact:
    Matthew Jones (matt.jones@umontana.edu)
    Brady Allred (brady.allred@umontana.edu)
  
  Jones, M.O., N.P. Robinson, D.E. Naugle, J.D. Maestas, M.C. Reeves, R.W.
  Lankston, and B.W. Allred. 2021. Annual and 16-Day Rangeland Production
  Estimates for the Western United States. Rangeland Ecology & Management
  77:112–117. http://dx.doi.org/10.1016/j.rama.2021.04.003
 * @param {ee.Image} two band image (afgNPP, pfgNPP) from projects/rangeland-analysis-platform/npp-partitioned-v2
 * @parm {ee.Image) mean annual temperature from "projects/rangeland-analysis-platform/gridmet-MAT"
 * @parm {string) oldName old name of property 
 * @return {ee.Image} biomass as g/m^2 (I (MH) updated this function to use these units
*/
exports.biomassFunction = function(image) {
    var mat = ee.ImageCollection("projects/rangeland-analysis-platform/gridmet-MAT");
    var year = ee.Date(image.get('system:time_start')).format('YYYY');
    var matYear = mat.filterDate(year).first();
    var fANPP = (matYear.multiply(0.0129)).add(0.171).rename('fANPP'); // fraction of NPP to allocate aboveground
    
    var agb = ee.Image(image).multiply(0.0001) // NPP scalar 
                //.multiply(2.20462) // KgC to lbsC MH--i commented out these lines
                //.multiply(4046.86) // m2 to acres MH--i commented out these lines
                .multiply(1000) // MH--KgC to gC
                .multiply(fANPP)  // fraction of NPP aboveground
                .multiply(2.1276) // C to biomass
                .rename(['afgAGB', 'pfgAGB'])
                .copyProperties(image, ['system:time_start'])
                .set('year', year);
               

    var herbaceous = ee.Image(agb).reduce(ee.Reducer.sum()).rename(['herbaceousAGB']);
    
    agb = ee.Image(agb)
      .addBands(herbaceous);

    return agb;
};

