

# dependencies ------------------------------------------------------------

source('scripts/04_create_biome-mask_dataframe_1986.R')
source('src/basemaps.R')
source("src/fig_params.R")
source("src/general_functions.R")
library(RColorBrewer)
library(sf)

# extract rasters --------------------------------------------------------


names(rast_rap1)

names(rasts_clim1)

r_MAT <- rasts_clim1$Yearly$tavg

r_MAP <- rasts_clim1$Yearly$prcp

r_prcpPropSum <- rasts_clim1$Summer$prcpProp


# base map ----------------------------------------------------------------
tmap_mode("plot")

base <- basemap_hist()

# Maps --------------------------------------------------------------------



# * RAP -------------------------------------------------------------------


tm_afgAGB <- tm_shape(rast_rap1[["afgAGB"]], bbox = bbox2) +
  tm_raster(breaks = breaks_bio2,
            labels = label_creator(breaks_bio2),
            palette = palette_bio2,
            title = lab_bio0,
            legend.hist = TRUE) +
  base +
  tm_layout(main.title = paste(fig_letters[4], "Annual forb and grass aboveground biomass"))

tm_afgAGB
# perennials
tm_pfgAGB <- tm_shape(rast_rap1[["pfgAGB"]], bbox = bbox2) +
  tm_raster(breaks = breaks_bio1,
            labels = label_creator(breaks_bio1),
            palette = palette_bio1,
            title = lab_bio0,
            legend.hist = TRUE) +
  base +
  tm_layout(main.title = paste(fig_letters[5], "Perennial forb and grass aboveground biomass"))


tm_pfgAGB
# * climate ---------------------------------------------------------------

# making breaks at the low points between the the 3 humps in the prcpPropSum
# histogram, so the first hump is red, second purple, 3rd blue

first <- seq(min(dfs_biome0[[1]]$prcpPropSum, na.rm = TRUE), 0.165, 
             length.out = 4)
second <- seq(0.165, 0.265, length.out = 4)
third <- seq(0.265, max(dfs_biome0[[1]]$prcpPropSum, na.rm = TRUE),
             length.out = 4)

breaks_prop <- unique(c(first, second, third))

breaks_map <- c(0, 50, 100, 150, 200, 300, 400, 500, 800,
                2500)
met1 <- tm_shape(rasts_clim1$Yearly[["prcp"]], bbox = bbox2) +
  tm_raster(breaks = breaks_map,
            labels = label_creator(breaks_map),
            palette = brewer.pal(length(breaks_map) -1, 'YlGnBu'),
            title = 'MAP (mm)',
            legend.hist = TRUE)  +
  base +
  tm_layout(main.title = paste(fig_letters[2], "Mean annual precipitation"))

met1

# MAT
breaks_met <- c(-5, seq(2, 18, by = 2), 30)
labels_met <- label_creator(breaks_met)
labels_met[1] <- paste("<", breaks_met[2])
met2 <- tm_shape(rasts_clim1$Yearly[["tavg"]], bbox = bbox2) +
  tm_raster(title = 'MAT (\u00B0C)', # deg C
            palette = '-RdYlBu',
            breaks = breaks_met,
            labels = labels_met,
            legend.hist = TRUE,
            midpoint = 6)  +
  base+
  tm_layout(main.title = paste(fig_letters[1], "Mean annual temperature"))
met2
met3 <- tm_shape(rasts_clim1$Summer[["prcpProp"]], bbox = bbox2) +
  tm_raster(title = 'Proportion',
            breaks = breaks_prop,
            palette = cols_prop,
            legend.hist = TRUE)  +
  base+
  tm_layout(main.title = paste(fig_letters[3], 'Proportion of precipitation that falls in summer (Jun-Aug)'))
met3


# * empty map -------------------------------------------------------------
# for blank spot on the map
blank_map <- tm_shape(spData::us_states) +
  tm_borders(col = 'white') +
  tm_layout(frame = FALSE)
# * combine -----------------------------------------------------------------


# left column is met, right column is biomass
jpeg("figures/maps_climate/maps_RAP-and-climate-with-hist_v3.jpeg", units = 'in', res = 600,
     height = 8.5, width = 7.5)
tmap_arrange( met2, tm_afgAGB, met1, tm_pfgAGB, met3,  nrow = 3)
dev.off()




