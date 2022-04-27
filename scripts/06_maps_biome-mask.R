# Martin Holdrege

# Script started March 31, 2022

# Purpose: Create maps of spatial probability and fire occurrence datasets
# (i.e. response variables), and of predictor variables (e.g., RAP
# biomass cover datasets). This is for data masked to the extent of 
# the sagebrush biome.
# additionally create maps of predicted fire probability based on
# model objects created in 05_models_biome-mask_fire-prob.Rmd


# dependencies ------------------------------------------------------------

# this script is useful because it reads in the rasters used below
source('scripts/04_create_biome-mask_dataframe.R')
source("src/general_functions.R")
source("src/fig_params.R")
library(tmap)
library(spData, quietly = TRUE) # for us_states polygon
library(RColorBrewer)

# * fire data -------------------------------------------------------------

# number of observed fires per pixel, MTBS data
# monitoring trends in burn severity, 
# ifph data (interagency fire perimeter history),
# and MTBS and IFPH combined, and lba
rasts_fPerPixel # rasters for both paint and reduceToImage methods


# * rap data --------------------------------------------------------------

rast_rap1


# * daymet ----------------------------------------------------------------

rasts_clim1


# * model objects ---------------------------------------------------------

# created in 05_models_biome-mask_fire-prob.Rmd
# (this object is very large ~2Gb)
glm_mods1 <- readRDS("models/glm_binomial_models_v1.RDS")

# predicted fire probability ----------------------------------------------
# predictions from glms, predict on original data that includes NA (i.e
# masked gridcells)
glm_mods2 <- glm_mods1
formula <- glm_mods1$formula # string of the model formula
glm_mods2$formula <- NULL

glm_preds1 <- map(glm_mods2, predict, newdata = df_biome0,
                  type = "response")

glm_preds_df <- bind_cols(glm_preds1) %>% 
  pivot_longer(cols = everything(),
               names_to = 'model',
               values_to = 'probability') %>% 
  drop_na()

empty <- rast_rap1[[1]]
empty[] <- NA
# filling empty raster w/ predicted values
rasts_pred1 <- map(glm_preds1, function(x) {
  empty[] <- x
  empty
})
rasts_pred2 <- rast(rasts_pred1)

# maps --------------------------------------------------------------------

tmap_mode("plot")
# extend bounding box
bbox <- tmaptools::bb(x = raster::raster(rasts_fPerPixel[[1]]), ext = 1.15) 

# * base map --------------------------------------------------------------

base <- tmap_options( # increase number of pixels plotted
  max.raster = c(plot = 1e10, view = 1e6) 
)+
  tm_shape(us_states) +
  tm_borders() +
  tm_layout(
    legend.outside = TRUE,
    legend.text.size = 0.75,
    main.title.size = 0.75,
    frame = FALSE,
    legend.position = c('center', 'top')) 



# * fire ------------------------------------------------------------------
# ** Observed fire occurrence --------------------------------------------------

breaks <- c(-0.5, 0.5, 1.5, 2.5, 3.5, 4.5, 6.5, 37)
labels <-  c(0, 1, 2, 3, 4, 6, '>=6')
palette <- c('grey', RColorBrewer::brewer.pal(6, "YlOrRd"))

base_raster <- function() {
  tm_raster(title = "N fires",
            breaks = breaks,
            labels = labels,
            palette = palette) +
    base
}

yr_string <- "# of fires over 36 years"
maps_fire <- map2(rasts_fPerPixel, names(rasts_fPerPixel), function(r, name) {
  
  tm2 <- tm_shape(r[['mtbs']], bbox = bbox) +
    base_raster() +
    tm_layout(main.title = paste0("MTBS, ", yr_string, "( ", name, " method)"))
  
  tm3 <- tm_shape(r[['ifph']], bbox = bbox) +
    base_raster() +
    tm_layout(main.title = paste0("IFPH, ", yr_string, "( ", name, " method)"))
  
  tm4 <- tm_shape(r[['comb']], bbox = bbox) +
    base_raster() +
    tm_layout(main.title = paste0("IFPH and MTBS combined\n",
                                  yr_string, "( ", name, " method)"))
  
  tm5 <- tm_shape(r[['lba']], bbox = bbox) +
    base_raster() +
    tm_layout(main.title = paste0("LBA,", yr_string, "( ", name, " method)"))
  
  list(tm2, tm3, tm4, tm5)

})


# ** predicted fire probability (sagebrush biome) -------------------------

# *** histograms ----------------------------------------------------------

pred_hist <- ggplot(glm_preds_df, aes(probability)) +
  geom_histogram(bins = 300) +
  facet_wrap(~model) +
  coord_cartesian(xlim = c(0, 0.05)) +
  labs(caption = "xlim restricted",
       subtitle = "predicted yearly fire probability for all sagebrush biome pixels, for each GLM")



# ***maps -----------------------------------------------------------------

pred_maps <- map(names(rasts_pred2), function(lyr) {
  out <- tm_shape(rasts_pred2[[lyr]], bbox = bbox) +
    tm_raster(breaks = c(seq(0, 0.02, .002), 0.2),
              title = 'fire probability') +
    base +
    tm_layout(main.title = paste("predicted fire probability from the",
                                 lyr, "model\n", formula),
              title.size = 0.75)
  
  out
})

# ** combine into multi panel map ------------------------------------------

pdf("figures/maps_fire_prob/fire_prob_biome-mask_v2.pdf",
    width = 8, height = 7)
  tmap_arrange(maps_fire[[1]], ncol = 2) # observed
  tmap_arrange(pred_maps[1:4]) # predicted
  tmap_arrange(maps_fire[[2]], ncol = 2)
  tmap_arrange(pred_maps[-(1:4)])
  pred_hist
dev.off()


# * examine cover ---------------------------------------------------------
# the cover dataset has some values that are x.5% values, but
# many more are whole numbers
# looking at the spatial pattern here (pattern not overly concerning)
# due to this issue (which creates a weird ) histograms/percentiles
# therefore the data was rounded in 04_create_biome-mask_dataframe.R
r <- rast_rap1[["shrCover"]]
x <- values(r)
r_integer <- r
r_integer[r%%1!= 0] <- NA
plot(r_integer)

r_frac <- r
r_frac[r_frac %% 1 == 0] <- NA
plot(r_frac)

# * RAP maps ----------------------------------------------------------------
# rangeland analysis platform biomass and cover data

title <- "\nMedian values (1984-2019)"

breaks_bio1 <- c(0, 10, 20, 50, 100, 200, 300)
palette_bio1 <- RColorBrewer::brewer.pal(length(breaks_bio1), 'YlGn')
breaks_bio2 <- c(0, 5, 10, 20, 30, 50, 100, 200)
palette_bio2 <- brewer.pal(length(breaks_bio1), 'YlGn')
# annuals
tm_rap1 <- tm_shape(rast_rap1[["afgAGB"]], bbox = bbox) +
  tm_raster(breaks = breaks_bio2,
            palette = palette_bio2,
            title = lab_bio0) +
  base +
  tm_layout(main.title = paste("Annual forb and grass biomass", 
                               title))

# perennials
tm_rap2 <- tm_shape(rast_rap1[["pfgAGB"]], bbox = bbox) +
  tm_raster(breaks = breaks_bio1,
            palette = palette_bio1,
            title = lab_bio0) +
  base +
  tm_layout(main.title = paste("Perennial forb and grass biomass", 
                               title))

# shrubs
breaks_cov1 <- c(0,2, 5, 10, 15, 20, 30, 50, 70)
palette_cov1 <- brewer.pal(length(breaks_cov1), 'YlGn')

tm_rap3 <- tm_shape(rast_rap1[["shrCover"]], bbox = bbox) +
  tm_raster(breaks = breaks_cov1,
            palette = palette_cov1,
            title = "% Cover") +
  base +
  tm_layout(main.title = paste("Shrub cover", 
                               title))


# ** histograms -----------------------------------------------------------
bins = 100
h0 <- ggplot(dfs_biome2[[1]])
h1 <- h0+
  geom_histogram(aes(afgAGB), bins = bins)
h2 <- h0+
  geom_histogram(aes(pfgAGB), bins = bins)
h3 <- h0+
  geom_histogram(aes(shrCover), 
                 breaks = seq(0, ceiling(max(dfs_biome2[[1]]$shrCover)), 1)) +
  labs(caption = "Cover rounded to the integer")

# save maps & histograms
pdf("figures/maps_veg/RAP_bio-cover_biome-mask_v1.pdf",
    width = 8, height = 6)
tmap_arrange(tm_rap1, tm_rap2, tm_rap3, nrow = 2)
gridExtra::grid.arrange(h1, h2, h3, ncol = 2)
dev.off()


# * daymet data -----------------------------------------------------------

met1 <- tm_shape(rasts_clim1$Yearly[["prcp"]], bbox = bbox) +
  tm_raster(breaks = c(0, 50, 100, 150, 200, 300, 400, 500, 700, 1000,
                       2500),
            palette = 'Blues',
            title = 'MAP (mm)')  +
  base +
  tm_layout(main.title = 'Precipitation (1984 - 2019)')

met2 <- tm_shape(rasts_clim1$Yearly[["tavg"]], bbox = bbox) +
  tm_raster(title = 'MAT (deg C)',
            palette = 'Reds')  +
  base+
  tm_layout(main.title = 'Temperature (1984 - 2019)')

met3 <- tm_shape(rasts_clim1$Summer[["prcpProp"]], bbox = bbox) +
  tm_raster(title = 'Proportion',
            palette = 'Blues')  +
  base+
  tm_layout(main.title = 'Proportion of precipitation that \nfalls in summer (Jun-Aug) ')


# ** histograms -----------------------------------------------------------

bins <-  500
h4 <- h0+
  geom_histogram(aes(MAP), bins = bins) +
  coord_cartesian(xlim = c(0, 1000)) +
  labs(caption = "xlim restricted")

h5 <- h0+
  geom_histogram(aes(MAT - 273.15), bins = bins) +
  labs(x = "MAT (deg C)")

h6 <- h0+
  geom_histogram(aes(prcpPropSum), bins = bins)

# save maps & histograms
pdf("figures/maps_climate/climate_biome-mask_v1.pdf",
    width = 8, height = 6)
  tmap_arrange(met1, met2, met3, nrow = 2)
  gridExtra::grid.arrange(h4, h5, h6, ncol = 2)
dev.off()
