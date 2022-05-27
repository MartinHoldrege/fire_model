/*
Purpose: export high resolution ee image asset (of 3 class SEI, under future conditions)
to tif, to be uploaded to
science base

Script started: 5/27/2022

Author: Martin Holdrege
*/

var path = 'projects/gee-guest/assets/SEI/'; // path to where most assets live

// image created by 01_SEI_future.js script
var Q5sc3Med = ee.Image(path + 'v11/forecasts/SEIv11_2017_2020_30_ClimateOnly_RCP85_2030-2060_median_20220215');

// region of interest
var biome = ee.FeatureCollection(path + 'US_Sagebrush_Biome_2019'); // defines the study region
var region = biome.geometry();

Map.addLayer(Q5sc3Med, {min:1,max:3, palette: ["black","grey","white"]}, '3 class future', false);

// exported files needs to have USGS albers equal area projection
// https://spatialreference.org/ref/sr-org/usa-contiguous-albers-equal-area-conic-usgs-version-landfire/
// The usgs version wasn't recognized, so goin with the regular version
//var wkt = 'PROJCS["USA_Contiguous_Albers_Equal_Area_Conic_USGS_version",GEOGCS["GCS_North_American_1983",DATUM["D_North_American_1983",SPHEROID["GRS_1980",6378137.0,298.257222101]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Albers"],PARAMETER["False_Easting",0.0],PARAMETER["False_Northing",0.0],PARAMETER["Central_Meridian",-96.0],PARAMETER["Standard_Parallel_1",29.5],PARAMETER["Standard_Parallel_2",45.5],PARAMETER["Latitude_Of_Origin",23.0],UNIT["Meter",1]]';

//var proj_usgs = ee.Projection(wkt);

// export to drive


// export to drive (30m esolution)
//'SEIv11_2017_2020_30_ClimateOnly_RCP85_2030-2060_median_20200527'
Export.image.toDrive({
  image: Q5sc3Med,
  description: 'SEIv11_2017_2020_30_ClimateOnly_RCP85_2030-2060_median_20200527',
  folder: 'gee',
  maxPixels: 1e13, 
  scale: 30,
  region: region,
  crs: 'EPSG:5070',
  fileFormat: 'GeoTIFF'
});