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

