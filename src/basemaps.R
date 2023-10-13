# Martin Holdrege

# Purpose: provide a basemap (for the sagebrush biome)
# to be used in other scripts


library(tmap)

crs <- "PROJCRS[\"IMAGINE GeoTIFF SupportERDAS IMAGINE 2018  16.5.0.596Projection = Albers Conical Equal Area\",\n    BASEGEOGCRS[\"WGS 84\",\n        DATUM[\"World Geodetic System 1984\",\n            ELLIPSOID[\"WGS 84\",6378137,298.257223563,\n                LENGTHUNIT[\"metre\",1]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        ID[\"EPSG\",4326]],\n    CONVERSION[\"Albers Equal Area\",\n        METHOD[\"Albers Equal Area\",\n            ID[\"EPSG\",9822]],\n        PARAMETER[\"Latitude of false origin\",23,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8821]],\n        PARAMETER[\"Longitude of false origin\",-96,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8822]],\n        PARAMETER[\"Latitude of 1st standard parallel\",29.5,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8823]],\n        PARAMETER[\"Latitude of 2nd standard parallel\",45.5,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8824]],\n        PARAMETER[\"Easting at false origin\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8826]],\n        PARAMETER[\"Northing at false origin\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8827]]],\n    CS[Cartesian,2],\n        AXIS[\"easting\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]],\n        AXIS[\"northing\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]]]"

tmap_mode("plot")
# extend bounding box
bbox <- sf::st_bbox(c(xmin = -2240000, 
                       xmax = -454000,
                       ymin = 1257000, ymax = 3162000))
bbox_g <- bbox
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
basemap_g <- function(bbox = NULL) {
  
  if (is.null(bbox)) {
    bbox <- bbox_g
  }
  
  # bounding box
  xlim = c(bbox[c('xmin', 'xmax')])
  ylim = c(bbox[c('ymin', 'ymax')])
 
   states <- st_as_sf(spData::us_states) %>% 
     sf::st_transform(crs)
  
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


