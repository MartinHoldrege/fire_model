# Martin Holdrege

# Purpose: provide a basemap (for the sagebrush biome)
# to be used in other scripts

library(tmap)

tmap_mode("plot")
# extend bounding box
bbox <- tmaptools::bb(x = c(-124.5, 33.18511, -100.76560, 49),
                      current.projection = 'EPSG:4326')

# * base map --------------------------------------------------------------

base <- tmap_options( # increase number of pixels plotted
  max.raster = c(plot = 1e10, view = 1e6) 
)+
  tm_shape(spData::us_states, bbox = bbox) +
  tm_borders() +
  tm_layout(
    legend.outside = TRUE,
    legend.text.size = 0.8,
    main.title.size = 0.8,
    frame = FALSE,
    legend.position = c('center', 'top')) 
#base

basemap <- function(legend.text.size = 0.8,
                    legend.title.size = 0.8,
                    main.title.size = 0.8) {
  
  out <-tmap_options(max.raster = c(plot = 1e10, view = 1e6))+
    tm_shape(spData::us_states, bbox = bbox) +
    tm_borders() +
    tm_layout(
      legend.outside = TRUE,
      legend.text.size = legend.text.size,
      legend.title.size = legend.title.size,
      main.title.size = main.title.size,
      frame = FALSE,
      legend.position = c('center', 'top')) 
  out
}
