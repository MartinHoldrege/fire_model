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
library(RColorBrewer)

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
# was split into before resampling, and the by NFire means that
# data was split into before/after fires for training
bin_string <- "bin20"
glm_mods_resample1 <- readRDS(
  paste0("models/glm_binomial_models_byNFire_v2_", bin_string, "_cwf.RDS"))

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


# * Observed fire occurrence --------------------------------------------------

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


# * predicted ------------------------------------------------------------

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


# altered preds ------------------------------------------------------------

# altering the predictor variables to gauge sensitivity of the model 
# to changes

# names of the different alteration 'treatments'
alter_names <- c("mat_warm", # mid level amount of warming
                 "mat_hot",  # high level of warming
                 "map_minus", # reduction in MAP
                 "map_plus", # increase in MAP
                 "prop_minus", # reduction in prcpPropSum
                 "prop_plus") # increase in prcpPropSum
dfs_alter1 <- vector('list', length(alter_names))
for (i in 1:length(dfs_alter1)) {
  dfs_alter1[[i]] <- df1
}
names(dfs_alter1) <- alter_names

warm <- 2
hot <- 5
dfs_alter1$mat_warm$MAT <- df1$MAT + warm
dfs_alter1$mat_hot$MAT <- df1$MAT + hot

map_change <- 0.2
dfs_alter1$map_minus$MAP <- df1$MAP*(1-map_change)
dfs_alter1$map_plus$MAP <- df1$MAP*(1 + map_change)

prop_change <- 0.2
dfs_alter1$prop_minus$prcpPropSum <- df1$prcpPropSum*(1-prop_change)
dfs_alter1$prop_plus$prcpPropSum <- df1$prcpPropSum*(1+prop_change)


# * predictions -----------------------------------------------------------

dfs_alter2 <- map(dfs_alter1, function(df) {
  out <- df
  out$pred <- predict(mod1, newdata = df, type = "response")
  out
})

# rasters of predicted values for given alteration 
# and the delta compared to no alteration
rasts_alter1 <- map(dfs_alter2, function(df) {
  pred <- empty
  
  # raster of predicted values for the given alteration
  pred[] <- df$pred
  
  # difference between the altered prediction and the original
  # data prediction 
  delta <- pred - rast_pred1
  
  list(pred = pred, delta = delta)
})


# * 6 panel map -----------------------------------------------------------

lab_delta <- "Probability change"
legend.text.size <- 0.55

delta_titles0 <- c(
  "mat_warm" = paste0("+", warm, "°C MAT"),
  "mat_hot" = paste0("+", hot, "°C MAT"),
  "map_minus" = paste0("-",map_change*100, "% MAP"),
  "map_plus" = paste0("+",map_change*100, "% MAP"),
  "prop_minus" = paste0("-",prop_change*100, "% prcpPropSum"),
  "prop_plus" = paste0("+",prop_change*100, "% prcpPropSum")
)

delta_titles <- paste(fig_letters[1:length(delta_titles0)], delta_titles0)

stopifnot(names(rasts_alter1) == names(delta_titles)) # make sure elements refer to the same alteration

# combining delta rasters into one rasters w/ multiple layers
rast_delta1 <- map2(rasts_alter1, names(rasts_alter1), function(x, name) {
  r <- x$delta
  names(r) <- name
  r
}) %>% 
  rast()


tm_shape(rast_delta1, bbox = bbox) +
  tm_raster(title = lab_delta,
            breaks = breaks_delta,
            palette = cols_delta,
            midpoint = 0) +
  tm_layout(panel.labels = delta_titles,
            panel.label.bg.color = 'white')+
  tm_facets(ncol =2) +
  basemap(legend.text.size = legend.text.size)
jpeg("figures/maps_sensitivity/delta-prob_clim-vars_v1.jpeg", units = 'in', res = 600,
     height = 8.5, width = 7.5)
  tm_shape(rast_delta1, bbox = bbox) +
    tm_raster(title = lab_delta,
              breaks = breaks_delta,
              palette = cols_delta,
              midpoint = 0) +
    tm_layout(panel.labels = delta_titles,
              panel.label.bg.color = 'white')+
    tm_facets(ncol =2) +
    basemap(legend.text.size = legend.text.size)
dev.off()

# Other way to make the maps (more flexibility if needed): would need
# to create the legend seperately and than combine all together
# delta_maps <- map2(rasts_alter1, delta_titles, function(x, title) {
#   r <- x$delta
#   out <- tm_shape(r, bbox = bbox) +
#     tm_raster(title = lab_delta,
#               breaks = breaks_delta,
#               palette = cols_delta,
#               midpoint = 0) +
#     basemap(legend.text.size = legend.text.size) +
#     tm_layout(main.title = title)
#   
#   out
# })
# 
# tmap_arrange(delta_maps, ncol = 2)



