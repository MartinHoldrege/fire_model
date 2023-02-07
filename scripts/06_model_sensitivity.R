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


# params ------------------------------------------------------------------

s <- "_S-T_A2-T2_A-Pr"
# s <- "" # original model string

bin_string <- "bin20"

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
dfnona <- dfs_biome2$paint

# so all values are positive (for log link)
dfnona$afgAGB <- dfnona$afgAGB + 0.001

# * model objects ---------------------------------------------------------

# (these objects are very large ~2Gb)

# glm models fit to resampled/balanced data

# here the bin_string refers to how many bins each predictor variable
# was split into before resampling, and the by NFire means that
# data was split into before/after fires for training

glm_mods_resample1 <- readRDS(
  paste0("models/glm_binomial_models_byNFire_v2_", bin_string, "_cwf", s,
  ".RDS"))

mod1 <- glm_mods_resample1$paint_cwf

# model that includes hmod (human modification) as an additional
# predictor variable (object created in
# "scripts/05_models_biome-mask_fire-prob_byNFire_hmod.Rmd")

glm_mods_hmod <- readRDS(
  paste0("models/glm_binomial_models_byNFire_hmod_v1_", bin_string, "_cwf.RDS"))

hmod1 <- glm_mods_hmod$paint_cwf

# predicted fire probability ----------------------------------------------


# model predictions 
# predicting on the 'old' data where there is just 1 row per pixel
# regardless of whether there was a fire--otherwise you have multiple
# predictions per pixel which is a bit harder to deal with
mod_pred1 <- predict(mod1, newdata = df1,
                        type = "response")

hmod_pred1 <- predict(hmod1, newdata = df1,
                     type = "response")

empty <- rast_rap1[[1]]
empty[] <- NA

# filling empty raster w/ predicted values
rast_pred1 <- empty
rast_pred1[] <- mod_pred1

rast_pred_hmod1 <- empty
rast_pred_hmod1[] <- hmod_pred1

# functions ---------------------------------------------------------------
# functions used here that likel rely on objects in the global environment 
# so best just defined here

breaks_prob <- c(seq(0, 0.021, .003), 0.2)

# create map of fire probability
tm_create_prob_map <- function(r, legend.text.size = 0.55,
                               main.title = "", 
                               legend.title.size = 0.8,
                               main.title.size = 0.8, ...) {
  
  tm_shape(r, bbox = bbox) +
    tm_raster(title = "Probability (%)",
              breaks = breaks_prob,
              labels = label_creator(breaks_prob, convert2percent = TRUE),
              ...) +
    basemap(legend.text.size = legend.text.size, 
            legend.title.size = legend.title.size,
            main.title.size = main.title.size) +
    tm_layout(main.title = main.title)
}



# observed & predicted map ------------------------------------------------

# simple two panel map showing observed and predicted, for the results
# section


# * Observed fire occurrence --------------------------------------------------

breaks <- c(-0.5, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 37)
labels <-  c(0, 1, 2, 3, 4, 5, '>5')
prop_change <- c(round(0:5/36, 3)*100, ">13.9")
labels <- paste0(labels, " (", prop_change, ")")
palette <- c('grey', RColorBrewer::brewer.pal(7, "YlOrRd")[-1])


tm_obs <- tm_shape(rast_fPerPixel, bbox = bbox) +
  tm_raster(title = "N fires (probability [%])",
            breaks = breaks,
            labels = labels,
            palette = palette) +
  basemap(legend.text.size = 0.55,
          legend.title.size = 0.75) +
  tm_layout(main.title = paste(fig_letters[1], "Number of observed fires (1987-2019)"))


# * predicted ------------------------------------------------------------


tm_pred1 <-  tm_shape(rast_pred1*100, bbox = bbox) +
  tm_raster(title = "Probability (%)",
            breaks = breaks_prob*100,
            labels = label_creator(breaks_prob, convert2percent = TRUE),
            legend.hist = TRUE,
            palette = RColorBrewer::brewer.pal(11, "RdYlBu")[9:2]) +
  basemap(legend.text.size = 0.4, 
          legend.title.size = 0.75,
          main.title.size = 0.8) +
  tm_layout(main.title = paste(fig_letters[2], "Modelled annual fire probability"),
            legend.hist.width = 1,
            legend.hist.height = 0.4, 
            title.position = c("left", "top"),
            legend.height = 0.9,
            legend.width = 1)

tm_pred1
jpeg(paste0("figures/maps_fire_prob/cwf_observed_predicted_pub-qual_v3", s, 
            ".jpeg"), units = 'in', res = 600, height = 2.6, width = 7)
tmap_arrange(tm_obs, tm_pred1,  ncol = 2)
dev.off()



# altered preds  ------------------------------------------------------------

# altering the predictor variables to gauge sensitivity of the model 
# to changes

# names of the different alteration 'treatments'
alter_names <- c("mat_warm", # mid level amount of warming
                 "mat_hot",  # high level of warming
                 "map_minus", # reduction in MAP
                 "map_plus", # increase in MAP
                 "prop_minus", # reduction in prcpPropSum
                 "prop_plus", # increase in prcpPropSum
                 "afg_low", # annuals low biomass
                 "afg_mid", # mid biomass
                 "afg_hi", # high
                 "pfg_low", # perennials
                 "pfg_mid",
                 "pfg_hi"
                 )
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

# using fixed biomass levels for afg and pfg
# afg 
afg_low_change <- 25 # g/m^2
afg_mid_change <- 75
afg_hi_change <- 150

dfs_alter1$afg_low$afgAGB <- afg_low_change
dfs_alter1$afg_mid$afgAGB <- afg_mid_change
dfs_alter1$afg_hi$afgAGB <- afg_hi_change

# pfg
dfs_alter1$pfg_low$pfgAGB <- afg_low_change
dfs_alter1$pfg_mid$pfgAGB <- afg_mid_change
dfs_alter1$pfg_hi$pfgAGB <- afg_hi_change

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

# recalculating afg and pfg deltas so that they
# are relative to the low biomass level
rasts_alter1[str_subset(alter_names, "afg_")] <- 
  map(rasts_alter1[str_subset(alter_names, "afg_")], function(x) {
    x$delta <- x$pred - rasts_alter1$afg_low$pred
    x
  })

rasts_alter1[str_subset(alter_names, "pfg_")] <- 
  map(rasts_alter1[str_subset(alter_names, "pfg_")], function(x) {
    x$delta <- x$pred - rasts_alter1$pfg_low$pred
    x
  })


# * 6 panel map -----------------------------------------------------------

legend.text.size <- 0.55

delta_titles0 <- c(
  "mat_warm" = paste0("+", warm, "°C MAT"),
  "mat_hot" = paste0("+", hot, "°C MAT"),
  "map_minus" = paste0("-",map_change*100, "% MAP"),
  "map_plus" = paste0("+",map_change*100, "% MAP"),
  "prop_minus" = paste0("-",prop_change*100, "% Proportion summer ppt"),
  "prop_plus" = paste0("+",prop_change*100, "% Proportion summer ppt"),
  "afg_low"= paste0("Annual biomass (", afg_low_change, " g/m^2)"),
  "afg_mid"= paste0("Annual biomass (", afg_mid_change, " g/m^2)"),
  "afg_hi"= paste0("Annual biomass (", afg_hi_change, " g/m^2)"),
  "pfg_low"= paste0("Perennial biomass (", afg_low_change, " g/m^2)"),
  "pfg_mid"= paste0("Perennial biomass (", afg_mid_change, " g/m^2)"),
  "pfg_hi"= paste0("Perennial biomass (", afg_hi_change, " g/m^2)")
)

delta_titles <- paste(fig_letters[1:length(delta_titles0)], delta_titles0)
names(delta_titles) <- names(delta_titles0)
rasts_alter2 <- rasts_alter1

# make sure elements refer to the same alteration
stopifnot(names(rasts_alter2) == names(delta_titles))

# combining delta rasters into one rasters w/ multiple layers
rast_delta1 <- map2(rasts_alter2, names(rasts_alter2), function(x, name) {
  r <- x$delta
  names(r) <- name
  r
}) %>% 
  rast()

# combining predicted rasters into one rasters w/ multiple layers
rast_alter_pred1 <- map2(rasts_alter2, names(rasts_alter2), function(x, name) {
  r <- x$pred
  names(r) <- name
  r
}) %>% 
  rast()



jpeg(paste0("figures/maps_sensitivity/delta-prob_clim-vars_v1", s, ".jpeg"), 
            units = 'in', res = 600, height = 8.5, width = 7.5)
tm_shape(rast_delta1[[1:6]], bbox = bbox) +
  tm_raster(title = lab_delta,
            breaks = breaks_delta,
            labels = labels_delta,
            palette = cols_delta,
            midpoint = 0) +
  tm_layout(panel.labels = delta_titles,
            panel.label.bg.color = 'white')+
  tm_facets(ncol =2) +
  basemap(legend.text.size = legend.text.size)
dev.off()

jpeg(paste0("figures/maps_sensitivity/delta-prob_bio-vars_v1", s, 
            ".jpeg"), units = 'in', res = 600,  height = 8, width = 8)
lyrs <- c("afg_mid", "afg_hi", "pfg_mid", "pfg_hi") # layers to show
tm_shape(rast_delta1[[lyrs]], 
         bbox = bbox) +
  tm_raster(title = lab_delta,
            breaks = breaks_delta,
            labels = labels_delta,
            palette = cols_delta,
            midpoint = 0) +
  tm_facets(ncol =2, free.scales = FALSE) +
  tm_layout(panel.labels = delta_titles0[lyrs],
            panel.label.bg.color = 'white',
            legend.outside = TRUE,
            main.title = paste("Change in probability relative to", afg_low_change, " g/m^2")) +
  basemap(layout = FALSE)
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

minmax(rast_delta1)

# * histograms --------------------------------------------------------------


# extracting values from rasters (excluding biomass alterations)
df_pred1 <- rasts_alter2[!str_detect(names(rasts_alter2), "(pfg)|(afg)")] %>% 
  map_dfr(function(x) {
    out <- tibble(
      pred = values_nona(x$pred)*100, # convert to percent
      delta = values_nona(x$delta)*100
    )
    out
  }, 
  .id = "scenario")

df_pred2 <- df_pred1 %>% 
  mutate(name = delta_titles0[scenario])

df_pred3 <- tibble(pred = values_nona(rast_pred1)*100, # convert to percent
       scenario = "original",
       name = "original data") %>% 
  bind_rows(df_pred2) %>% 
  mutate(name = factor(name, levels = c(delta_titles0, "original data")))

# summarize 
summary1 <- df_pred3 %>% 
  group_by(scenario, name) %>% 
  summarize(across(c(pred, delta), 
                   .fns = list(mean = mean, median = median, min = min,
                               max = max)),
            .groups = 'drop')
summary1

summary_long1 <-  summary1 %>% 
  pivot_longer(cols = c(starts_with("pred_"), starts_with("delta_")),
               names_to = c('variable', "summary_stat"),
               names_sep = "_") %>% 
  pivot_wider(names_from = 'variable') %>% 
  mutate(summary_stat = factor(summary_stat,
                               levels = c("min", "median", "mean", "max")))

ggplot(df_pred3, aes(x = pred)) +
  geom_vline(data = summary_long1, aes(xintercept = pred, color = summary_stat)) +
  geom_histogram(bins = 200) +
  facet_wrap(~name)+
  theme_bw()

ggplot(df_pred3, aes(x = delta)) +
  geom_vline(data = summary_long1, aes(xintercept = delta, color = summary_stat)) +
  geom_histogram(bins = 200) +
  facet_wrap(~name)+
  theme_bw()

# delta summary stats -----------------------------------------------------

quants <- map(names(rast_delta1),  function(name) {
  r <- rast_delta1[[name]]
  x <- as.vector(values(r))
  out <- quantile(x, c(0, 0.05, 0.1, 0.5, 0.9, 0.95, 1), na.rm = TRUE)
  # mean absolute change
  out <- c(out, 'mean_abs' = mean(abs(x), na.rm = TRUE))
  out*100 # convert to percent change
})
names(quants) <- names(rast_delta1)

# mean absolute change is larger for mat (both warm and hot) then
# map or prop changes
quants[1:6]
# maps pred and delta  ------------------------------------------------


# * clim vars -------------------------------------------------------------


tm_delta_clim1 <- tm_shape(rast_delta1[[1:6]], bbox = bbox) +
  tm_raster(title = lab_delta,
            breaks = breaks_delta,
            labels = labels_delta,
            palette = cols_delta,
            midpoint = 0,
            legend.hist = TRUE) +
  tm_layout(panel.labels = delta_titles0,
            panel.label.bg.color = 'white',
            main.title = "Delta probability",
            legend.show = FALSE,
            panel.label.size = 0.75)+
  tm_facets(ncol =1) +
  basemap(legend.text.size = legend.text.size,
          main.title.size = 0.7)

tm_pred_clim1 <- tm_create_prob_map(rast_alter_pred1[[1:6]],
                                    main.title = "Predicted probability (%)",
                                    main.title.size = 0.7) +
  tm_layout(panel.labels = delta_titles0,
            panel.label.bg.color = 'white',
            legend.show = FALSE,
            panel.label.size = 0.75)+
  tm_facets(ncol =1)
tm_pred_clim1
jpeg(paste0("figures/maps_sensitivity/pred_delta-prob_clim-vars_v1", s, ".jpeg"), 
     units = 'in', res = 600, height = 12, width = 4)
tmap_arrange(tm_pred_clim1, tm_delta_clim1, ncol = 2)
dev.off()


# * veg vars --------------------------------------------------------------

tm_delta_bio1 <- tm_shape(rast_delta1[[-(1:6)]], bbox = bbox) +
  tm_raster(title = lab_delta,
            breaks = breaks_delta,
            labels = labels_delta,
            palette = cols_delta,
            midpoint = 0) +
  tm_layout(panel.labels = delta_titles0[-(1:6)],
            panel.label.bg.color = 'white',
            main.title = "Delta probability",
            legend.show = FALSE,
            panel.label.size = 0.7)+
  tm_facets(ncol =1) +
  basemap(legend.text.size = legend.text.size,
          main.title.size = 0.7)

tm_pred_bio1 <- tm_create_prob_map(rast_alter_pred1[[-(1:6)]],
                                    main.title = "Predicted probability (%)",
                                    main.title.size = 0.7) +
  tm_layout(panel.labels = delta_titles0[-(1:6)],
            panel.label.bg.color = 'white',
            legend.show = FALSE,
            panel.label.size = 0.7)+
  tm_facets(ncol =1)

jpeg(paste0("figures/maps_sensitivity/pred_delta-prob_bio-vars_v1", s, ".jpeg"), 
     units = 'in', res = 600, height = 12, width = 4)
tmap_arrange(tm_pred_bio1, tm_delta_bio1, ncol = 2)
dev.off()

# delta due to hmod -------------------------------------------------------
# difference in predicted fire probability when the hmod variable is used.

hmod_delta <- rast_pred1 - rast_pred_hmod1

x <- values(hmod_delta) %>% as.vector()
hist(x, breaks = 100, xlim = c(-0.01, 0.01))



tm_hmod <- tm_create_prob_map(rast_pred_hmod1,
                   main.title = paste(fig_letters[1], "Predicted probability (%)",
                                      '\nfor model including human modification'),
                   main.title.size = 0.8,
                   legend.title.size = 0.6)

tm_hmod_delta <- tm_shape(hmod_delta, bbox = bbox) +
  tm_raster(title = lab_delta,
            breaks = breaks_delta,
            labels = labels_delta,
            palette = cols_delta,
            midpoint = 0) +
  basemap(legend.text.size = legend.text.size,
          legend.title.size = 0.6) +
  tm_layout(main.title = paste(fig_letters[2],
                               "Change in fire probability relative to model",
                               "\nwithout human modification"),
            main.title.size = 0.8)



jpeg("figures/maps_fire_prob/cwf_hmod_predicted_v1.jpeg", units = 'in', res = 600,
     height = 2.8, width = 7)
tmap_arrange(tm_hmod, tm_hmod_delta, nrow = 1)
dev.off()
