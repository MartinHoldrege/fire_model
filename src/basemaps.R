# Martin Holdrege

# Purpose: provide a basemap (for the sagebrush biome)
# to be used in other scripts

library(tmap)
library(spData, quietly = TRUE) # for us_states polygon

tmap_mode("plot")
# extend bounding box
bbox <- tmaptools::bb(x = c(-123.60661, 33.18511, -100.76560, 50.10668), ext = 1.15,
                      current.projection = 'EPSG:4326')

# * base map --------------------------------------------------------------

base <- tmap_options( # increase number of pixels plotted
  max.raster = c(plot = 1e10, view = 1e6) 
)+
  tm_shape(us_states, bbox = bbox) +
  tm_borders() +
  tm_layout(
    legend.outside = TRUE,
    legend.text.size = 0.75,
    main.title.size = 0.75,
    frame = FALSE,
    legend.position = c('center', 'top')) 

