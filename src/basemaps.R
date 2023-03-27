# Martin Holdrege

# Purpose: provide a basemap (for the sagebrush biome)
# to be used in other scripts

library(tmap)

tmap_mode("plot")
# extend bounding box
bbox <- tmaptools::bb(x = c(-124.5, 33.18511, -102, 49),
                      current.projection = 'EPSG:4326')
bbox2 <- bbox
bbox2['xmin'] <- -130
bbox3 <- bbox
bbox3['xmin'] <- -129.5

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
                    main.title.size = 0.8,
                    layout = TRUE # whether to include layout options
                    ) {
  
  out <-tmap_options(max.raster = c(plot = 1e10, view = 1e6))+
    tm_shape(spData::us_states, bbox = bbox) +
    tm_borders() 
  
  if(layout) {
    out <- out +  
      tm_layout(
      legend.outside = TRUE,
      legend.text.size = legend.text.size,
      legend.title.size = legend.title.size,
      main.title.size = main.title.size,
      frame = FALSE,
      legend.position = c('center', 'top')) 
  }

  out
}


# basemap for ggplot maps
basemap_g <- function(bbox) {
  
  # bounding box
  xlim = c(bbox[c('xmin', 'xmax')])
  ylim = c(bbox[c('ymin', 'ymax')])
 
   states <- st_as_sf(spData::us_states)
  
  list(
    geom_sf(data = states, fill = NA, color = 'black'),
    coord_sf(xlim = xlim,
             ylim = ylim,
             expand = FALSE),
    theme_void()
   )
}


# basemap for placing histograms ------------------------------------------

# basemap for when putting a histogram on the map (placed on the
# map off the california coast)
basemap_hist <- function(add_poly = TRUE, legend.position = c('LEFT', 'bottom')) {
  
  # to cover background (SW california), where histogram is going
  poly <- tibble(
    lon = c(-127, -116),
    lat = c(33, 35.5)
  ) %>% 
    sf::st_as_sf(coords = c("lon", "lat"), 
             crs = 4326) %>% 
    sf::st_bbox() %>% 
    sf::st_as_sfc()
  

  out <- tmap_options( # increase number of pixels plotted
    max.raster = c(plot = 1e10, view = 1e6) 
    )+
  tm_shape(spData::us_states) +
  tm_borders() +
  tm_layout(
    legend.outside = FALSE,
    legend.text.size = 0.6,
    legend.title.size = 0.8,
    main.title.size = 0.8,
    title.position = c("left", "top"),
    legend.height = 1,
    legend.width = 1,
    legend.hist.width = 0.5,
    frame = FALSE,
    legend.position = legend.position,
    inner.margins=c(.04,.08, .08, .01)) 
  
 if(add_poly) {
    out <- out +
      tm_shape(poly) +
      tm_fill('white') # white background behind legend
  }
    
  out
}


