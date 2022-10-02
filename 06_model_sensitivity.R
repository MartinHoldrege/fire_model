# Martin Holdrege

# started 10/1/2022

# Purpose: Assess sensitivity of the fire model to changes in predictor
# variables, and create maps of the output for later use.




# dependencies ------------------------------------------------------------

# this script is useful because it reads in the rasters used below
# and dataframes to predict on (note this is not the 
# data used to fit the byNfire model)
source('scripts/04_create_biome-mask_dataframe_1986.R')
source("src/basemaps.R")
source("src/fig_params.R")
source("src/general_functions.R")


# * fire data -------------------------------------------------------------

# number of observed fires per pixel, cwf (combined wildand fire dataset) 
# from now on just using the cwf dataset which is the best
rast_fPerPixel <- rasts_fPerPixel$paint

# * rap data --------------------------------------------------------------

rast_rap1

# * daymet ----------------------------------------------------------------

# using these raster so that the climate spans the same time period
# as the RAP & fire data that we're using for the nFire model
rasts_clim1



# * dataframe -------------------------------------------------------------

df1 <- dfs_biome0$paint
# * model objects ---------------------------------------------------------

# (these objects are very large ~2Gb)

# glm models fit to resampled/balanced data

# here the bin_string refers to how many bins each predictor variable
# was split into before resampling
bin_string <- "bin20"
glm_mods_resample1 <- readRDS(
  paste0("models/glm_binomial_models_resample_v3_", bin_string, "_cwf_.RDS"))

mod1 <- glm_mods_resample1$paint_cwf

# predicted fire probability ----------------------------------------------


# model predictions 
# predicting on the 'old' data where there is just 1 row per pixel
# regardless of whether there was a fire--otherwise you have multiple
# predictions per pixel which is a bit harder to deal with
mod_pred1 <- predict(mod1, newdata = df1,
                        type = "response")


empty <- rast_rap1[[1]]
empty[] <- NA

# filling empty raster w/ predicted values
rast_pred1 <- empty
rast_pred1[] <- mod_pred1


# observed & predicted map ------------------------------------------------

# simple two panel map showing observed and predicted, for the results
# section


# ** Observed fire occurrence --------------------------------------------------

breaks <- c(-0.5, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 37)
labels <-  c(0, 1, 2, 3, 4, 5, '>5')
breaks <- c(-0.5, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 37)
labels <-  c(0, 1, 2, 3, 4, 5, '>5')
prop_change <- c(round(0:5/36, 3), ">0.139")
labels <- paste0(labels, " (", prop_change, ")")
palette <- c('grey', RColorBrewer::brewer.pal(7, "YlOrRd")[-1])


tm_obs <- tm_shape(rast_fPerPixel, bbox = bbox) +
  tm_raster(title = "N fires (probability)",
            breaks = breaks,
            labels = labels,
            palette = palette) +
  basemap(legend.text.size = 0.65) +
  tm_layout(main.title = paste(fig_letters[1], "Number of observed fires (1987-2019)"))

tm_obs


# ** predicted ------------------------------------------------------------

breaks_prob <- c(seq(0, 0.021, .003), 0.2)
tm_pred1 <- tm_shape(rast_pred1, bbox = bbox) +
  tm_raster(title = "Probability",
            breaks = breaks_prob,
            labels = label_creator(breaks_prob)) +
  basemap(legend.text.size = 0.55) +
  tm_layout(main.title = paste(fig_letters[2], "Predicted annual fire probability"))
tm_pred1

jpeg("figures/maps_fire_prob/cwf_observed_predicted_pub-qual_v1.jpeg", units = 'in', res = 600,
     height = 2.8, width = 7)
tmap_arrange(tm_obs, tm_pred1,  ncol = 2)
dev.off()

