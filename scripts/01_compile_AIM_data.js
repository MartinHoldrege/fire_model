/************************************************
 * 
 * AIM veg and climate data
 * 
 * Author: Martin Holdrege
 * 
 * Purpose: Pull in AIM vegetation monitoring 
 * sites, and export that data along with the
 * daymet climate normals for those sites. 
 * 
 ************************************************
 */
 
  // the daymet data has 1km res, so I don't think it makes sense to go smaller
 var resolution = 1000;
// read in data -------------------------------------

// climate normals calculated from daymet data
var clim = require("users/mholdrege/cheatgrass_fire:scripts/00_daymet_summaries.js");

// AIM vegetation data
var aim1 = ee.FeatureCollection('BLM/AIM/v1/TerrADat/TerrestrialAIM');


// filter data ----------------------------------------
// keep sites with sagebrush
var aim2 = aim1.filter(ee.Filter.gt('SagebrushCover_AH', 0));

print('example aim point', aim2.first());
Map.addLayer(aim2, {}, "Aim sites w/ sagebrush", false);

Map.addLayer(aim1.filter(ee.Filter.eq('SagebrushCover_AH', 0)), 
  {}, "Aim sites w/o sagebrush", false);

var aim3 = ee.Image(clim.climSpringAvg).reduceRegions({
    collection: aim2, 
    reducer:ee.Reducer.mean(), 
    scale: resolution
});

// Generic Function to remove a property from a feature
// function from:
// https://gis.stackexchange.com/questions/321724
// /removing-property-from-feature-or-featurecollection-using-google-earth-engine
var removeProperty = function(feat, property) {
  var properties = feat.propertyNames()
  var selectProperties = properties.filter(ee.Filter.neq('item', property))
  return ee.Feature(feat).select(selectProperties)
}

// Rename the property of a feature
// feat--ee.Feature
// newName and oldName --should be strings
var renameProperty = function(feat, newName, oldName) {
  var feat2 = feat.set(newName, feat.get(oldName));
  var out = removeProperty(feat2, oldName);
  return out;
};


var test = aim3.first();
print(renameProperty(test, "tminSpring", "tmin"));

/*
var greens = ee.List([
  '#00441B', '#00682A', '#37A055', '#5DB96B', '#AEDEA7', '#E7F6E2', '#F7FCF5'
]);
var reds = ee.List([
  '#67000D', '#9E0D14', '#E32F27', '#F6553D', '#FCA082', '#FEE2D5', '#FFF5F0'
]);

function normalize(value, min, max) {
  return value.subtract(min).divide(ee.Number(max).subtract(min));
}

function setColor(feature, property, min, max, palette) {
  var value = normalize(feature.getNumber(property), min, max)
                  .multiply(palette.size())
                  .min(palette.size().subtract(1))
                  .max(0);
  return feature.set({style: {color: palette.get(value.int())}});
}

var fc = ee.FeatureCollection('BLM/AIM/v1/TerrADat/TerrestrialAIM');
var woodyHeightStyle = function(f) {
  return setColor(f, 'WoodyHgt_Avg', 0, 100, greens);
};
var bareSoilStyle = function(f) {
  return setColor(f, 'BareSoilCover_FH', 0, 100, reds);
};

var treeHeight = fc.filter('WoodyHgt_Avg > 1').map(woodyHeightStyle);
var bareSoil = fc.filter('BareSoilCover_FH > 1').map(bareSoilStyle);

Map.addLayer(bareSoil.style({styleProperty: 'style', pointSize: 3}));
Map.addLayer(treeHeight.style({styleProperty: 'style', pointSize: 1}));

Map.setCenter(-110, 40, 6);

var test =  fc.first();
print(test);
*/
