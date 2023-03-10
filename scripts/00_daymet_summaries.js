/************************************************
 * 
 * Prepare Daymet climate data
 * 
 * Author: Martin Holdrege
 * 
 * Purpose: compile daymet data, for use in downstream
 * scripts, specifically calculate average yearly, summer, and
 * spring tmax, tmin and precip.
 * 
 ************************************************
 */
 
 // user define variables:
 
 // date range
var startYear = 1986;
//var startYear = 2018; // short time period for testing
var endYear = 2019;
var startDate = ee.Date.fromYMD(startYear, 1, 1);
var endDate = ee.Date.fromYMD(endYear, 12, 31); 

// Annual temp and precipitation ****************

var daymet = ee.ImageCollection("NASA/ORNL/DAYMET_V4")
  .filterDate(startDate, endDate);


// Not sure if there is a problem with speed using using select (string), 
// inside a map() call if the string is a client side string,
// so doing the select here
var daymetP = daymet.select('prcp');

var daymetT = daymet.select(['tmax', 'tmin']);

var daymetT = daymetT.map(function(image) {
  var tsum = image.select('tmax')
    .add(image.select('tmin'));
  
  var tavg = tsum.divide(2)
    .rename('tavg');
  var out = image.addBands(tavg);
  return out;
});
// print(daymetT.first());
// Map.addLayer(daymetT.first().select('tmax'),{}, 'tmax');
// Map.addLayer(daymetT.first().select('tavg'), {},'tavg');
// make a list with years
var years = ee.List.sequence(startYear, endYear);

//  avg temp, and total ppt for each year
var climYearlyList = years.map(function(y) {
  // Precip for each day of year for the given year
  var filteredP = daymetP.filter(ee.Filter.calendarRange(y, y, 'year'));
  // Temp for each day of year for the given year
  var filteredT = daymetT.filter(ee.Filter.calendarRange(y, y, 'year'));
  var precip = filteredP.sum(); // total ppt for the year
  var temp = filteredT.mean(); // mean of min/max temp for the year
   // casting to double so datatype of temp and precp same. otherwise can't export to drive
  var out = ee.Image(precip).addBands(temp.toDouble());
  return out;
});


// avg yearly ppt and temp across years
var climYearlyAvg = ee.ImageCollection(climYearlyList)
  .mean();

Map.addLayer(climYearlyAvg.select('prcp'),
  {min: 50, max: 700, palette: ['white', 'blue']}, 'Annual ppt', false);
  
 Map.addLayer(climYearlyAvg.select('tmax'),
  {min: 3, max: 30, palette: ['blue', 'red']}, 'Annual tmax', false); 
// Summer temp and precip ******************************

// This function builds a function that calculates seasonal climate for a given
// month range (i.e. what Hadley Wickham calls a function factory)
var createSeasonClimFun = function(startMonth, endMonth) {
  var outFun = function(y) {
    var filteredP = daymetP.filter(ee.Filter.calendarRange(y, y, 'year'))
      .filter(ee.Filter.calendarRange(startMonth, endMonth, 'month'));
    var filteredT = daymetT.filter(ee.Filter.calendarRange(y, y, 'year'))
      .filter(ee.Filter.calendarRange(startMonth, endMonth, 'month'));
    var precip = filteredP.sum(); // total ppt for the year
    var temp = filteredT.mean(); // mean of min/max temp for the year
    var out = ee.Image(precip).addBands(temp.toDouble());
  return out;
  };
  return outFun;
};

// function to calculate summer climate (June-Aug)
var calcSummerClim = createSeasonClimFun(ee.Number(6), ee.Number(8));


//  avg temp, and total ppt for each year
var climSummerList = years.map(calcSummerClim)
  //calculating proportion of total ppt that falls in summer
  .zip(climYearlyList)
  .map(function(x) {
    var imageSummer = ee.Image(ee.List(x).get(0));
    var imageYearly = ee.Image(ee.List(x).get(1));
    // proportion of ppt that falls in summer
    var prcpProp = imageSummer.select('prcp')
      .divide(imageYearly.select('prcp'))
      .rename('prcpProp');
    var out = imageSummer.addBands(prcpProp);
    return out;
  });

// avg summer ppt and temp across years
var climSummerAvg = ee.ImageCollection(climSummerList)
  .mean();
  
//print(climSummerAvg);
Map.addLayer(climSummerAvg.select('prcpProp'), {min: 0, max: 0.7, palette: ['red', 'white', 'blue']}, 'prcpPropSummer', false);
// Spring temp and precip ******************************
  
// function to calculate springr climate (march - may)
var calcSpringClim = createSeasonClimFun(ee.Number(3), ee.Number(5));

//  avg temp, and total ppt for each year
var climSpringList = years.map(calcSpringClim);

// avg spring ppt and temp across years
var climSpringAvg = ee.ImageCollection(climSpringList)
  .mean();

// print('summer climate', climSummerAvg);


 // export files so that this script can be sourced as a module
 // and these objects accessed
 
exports.climSpringAvg = climSpringAvg;
exports.climSummerAvg = climSummerAvg;
exports.climYearlyAvg = climYearlyAvg;
