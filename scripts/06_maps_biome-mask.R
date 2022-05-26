# Martin Holdrege

# Script started March 31, 2022

# Purpose: Create maps of spatial probability and fire occurrence datasets
# (i.e. response variables), and of predictor variables (e.g., RAP
# biomass cover datasets). This is for data masked to the extent of 
# the sagebrush biome.
# additionally create maps of predicted fire probability based on
# model objects created in 05_models_biome-mask_fire-prob.Rmd

# note--as the code is written right now it is a major memory hog,
# and is using up to ~18Gb of memory--i.e. this code would need to be
# improved (e.g. remove objects not used in downstream code), if it needed 
# to be run on a machine with 16Gb RAM

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

rasts_clim1 # daymet biome-mask

# ** sw2 simulation extent --------------------------------

# this is daymet climate data for the same gridcells as the upscaled
# stepwat2 output

paths_sw2_clim <- list.files("data_processed/daymet/",
                             "sw2sim-extent_v1.tif",
                             full.names = TRUE)

# name by time period
names(paths_sw2_clim) <- basename(paths_sw2_clim) %>% 
  str_extract("(?<=_clim)[A-z]+(?=Avg_)")

rasts_sw2_clim_list <- map(paths_sw2_clim, rast)


# * model objects ---------------------------------------------------------

# model objects created in 05_models_biome-mask_fire-prob.Rmd

# (these objects are very large ~2Gb)

glm_mods1 <- readRDS("models/glm_binomial_models_v4.RDS")

# glm models fit to resampled/balanced data

# here the bin_string refers to how many bins each predictor variable
# was split into before resampling
bin_string <- "bin20"
glm_mods_resample1 <- readRDS(
  paste0("models/glm_binomial_models_resample_v2_", bin_string, ".RDS"))

# generalized non-linear models.
#non-linear term fit for afg
# gnm_mods1 <- readRDS("models/gnm_binomial_models_v3_afgAGB-MAP-interaction.RDS")
# * sw2 biomass -----------------------------------------------------------

rasts_bio1 <- rast("../grazing_effects/data_processed/interpolated_rasters/bio_future_median_across_GCMs.tif")


# predicted fire probability ----------------------------------------------

# * biome-extent ----------------------------------------------------------

# predictions from glms & gnms, predict on original data that includes NA (i.e
# masked gridcells). Data is sturcture is to have lists, one list item for
# glm models and the other for the gnms

# list of list of models
# mods1 <- list(glm = glm_mods1, gnm = gnm_mods1) 
mods1 <- list(glm_mods1,
              glm_mods_resample1) 

names(mods1) <- c("glm",
                  paste0("glm_resample_", bin_string))

mod_types <- names(mods1)
formulas <- map(mods1, function(x) x$formula) # string of the model formula
mods2 <- map(mods1, function(x) {
  x$formula <- NULL
  x
})

# model predictions for each model 
mod_preds1 <- map_depth(mods2, .depth = 2, predict, newdata = df_biome0,
                  type = "response")

# longform dataframe of predictions
mod_preds_dfs <- map(mod_preds1, function(x) {
  bind_cols(x) %>% 
    pivot_longer(cols = everything(),
                 names_to = 'model',
                 values_to = 'probability') %>% 
    drop_na()
})

empty <- rast_rap1[[1]]
empty[] <- NA

# filling empty raster w/ predicted values
rasts_pred1 <- map_depth(mod_preds1, .depth = 2, function(x) {
  empty[] <- x
  empty
})

# one raster w/ layer for each response variable for glm,
# and same for gnm
rasts_pred2 <- map(rasts_pred1, rast)


# **+5c prediction --------------------------------------------------------
# predicted fire probability for +5c 

df_biome5c <- dfs_biome0$paint
df_biome5c$MAT <- df_biome5c$MAT + 5

# mods for doing +5c predictions on
mods_5c <- map(mods1,function(x) x$paint_mtbs) 

names_5c <- names(mods_5c)
names(names_5c) <- names_5c
pred_5c <- map(mods_5c, predict, newdata = df_biome5c, 
                   type = 'response')

r_pred_5c <- map(pred_5c, function(x) {
  out <- empty
  out[] <- x
  out
})


# difference in predicted fire probabilyt, current vs + 5C

r_delta_5c <- map(names_5c, function(name) {
  r_pred_5c[[name]] - rasts_pred2[[name]]$paint_mtbs

})

# * sw2sim extent ---------------------------------------------------------
# predictions, given stepwat simulation output as predictor variables


# ** create dataframe -----------------------------------------------------

rasts_sw2_clim_list2 <- list(
  rasts_sw2_clim_list[['Yearly']][[c('prcp', 'tavg')]],
  rasts_sw2_clim_list[['Summer']][['prcpProp']])
  
rasts_sw2_clim1 <- rast(rasts_sw2_clim_list2)


# the problem is they have (slightly) different extents
compareGeom(rasts_sw2_clim1, rasts_bio1,
            lyrs = FALSE, crs = TRUE, ext = FALSE,
            rowcol = FALSE, res = TRUE)

rasts_sw2_clim2 <- crop(rasts_sw2_clim1, rasts_bio1)

# now the extent issue should be fixed
compareGeom(rasts_sw2_clim2, rasts_bio1,
            lyrs = FALSE, crs = TRUE, ext = TRUE,
            rowcol = TRUE, res = TRUE)


lyrs_perennial <- paste0("c4on_", 
                         c("C3Pgrass", "C4Pgrass", "Pforb"),
                         "_biomass_Current_Current_Light")

lyrs_annual <- paste0("c4on_", 
                      c("Cheatgrass", "Aforb"),
                      "_biomass_Current_Current_Light")

# simulated perennial grass & forb biomass
r_pfgAGB_sw2 <- sum(rasts_bio1[[lyrs_perennial]])
# simulated annual grass & forb
r_afgAGB_sw2 <- sum(rasts_bio1[[lyrs_annual]]) 


df_sw2sim1 <- tibble(
  pfgAGB = values(r_pfgAGB_sw2) %>% as.vector()
)

df_sw2sim1$afgAGB <- r_afgAGB_sw2 %>% 
  values() %>% 
  as.vector()

# don't have shrub cover from stepwat 2 output (need allometric equation)
# for now just using mean remote sensed value
df_sw2sim1$shrCover <- mean(df_biome0$shrCover, na.rm = TRUE)

# add in climate variables
df_sw2sim1$MAP <- rasts_sw2_clim2[['prcp']] %>% 
  values() %>% 
  as.vector()

df_sw2sim1$MAT <- rasts_sw2_clim2[['tavg']] %>% 
  values() %>% 
  as.vector()

df_sw2sim1$prcpPropSum <- rasts_sw2_clim2[['prcpProp']] %>% 
  values() %>% 
  as.vector()

df_sw2sim1$MAT <- df_sw2sim1$MAT + 273.15 # c to K

df_sw2_sim2 <- df_sw2sim1 


# ** prediction -----------------------------------------------------------

# predictions for glm and gnm models
mod_sw2_preds1 <- map_depth(mods2, .depth = 2, .f = predict, 
                            newdata = df_sw2_sim2, type = "response")

mod_sw2_preds_dfs <- map(mod_sw2_preds1, function(x) {
  bind_cols(x) %>% 
    pivot_longer(cols = everything(),
                 names_to = 'model',
                 values_to = 'probability') %>% 
    drop_na()
})

# filling empty raster w/ predicted values
empty_sw2 <- rasts_bio1[[1]]
empty_sw2[] <- NA
rasts_sw2_pred1 <- map_depth(mod_sw2_preds1, .depth = 2, .f = function(x) {
  empty_sw2[] <- x
  empty_sw2
})

# combination of the names of the outer and inner lists
# for later 'looping', this will break down if the gnm and glm
# elements don't have the same names
all_mod_names <- expand_grid(
  type = names(rasts_sw2_pred1), #e.g., gnm, vs glm
  mod = map(rasts_sw2_pred1, names) %>% unlist() %>% unique()
)


# scatterplots ------------------------------------------------------------


# * delta 5c --------------------------------------------------------------

map(names_5c, function(x) {
  jpeg(paste0("figures/delta_fire-prob_vs_MAT_v2_", x, ".jpeg"))
  plot(dfs_biome0$paint$MAT - 273.15, as.numeric(values(r_delta_5c[[x]])),
       ylab = "Delta fire probability", 
       xlab = 'MAT (deg C)',
       main = paste0("Change in predicted fire probability with 5C warming\n",
                    x),
       col = rgb(red = 0, green = 0, blue = 0, alpha = 0.1),
       cex = 0.2)
  dev.off()
})


# hist(as.numeric(values(r_delta_5c[[1]])),
#      main = "Change in predicted fire probability with 5C warming")

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

breaks <- c(-0.5, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 37)
labels <-  c(0, 1, 2, 3, 4, 5, '>5')
prop_change <- c(round(0:5/36, 3), ">0.139")
labels <- paste0(labels, " (", prop_change, ")")
palette <- c('grey', RColorBrewer::brewer.pal(7, "YlOrRd")[-1])

base_raster <- function() {
  tm_raster(title = "N fires (probability)",
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

hist_base <- function(df) {
  summary_df <- df %>% 
    group_by(model) %>% 
    summarise(mean = mean(probability),
              median = median(probability),
              .groups = 'drop')
  list(
    geom_histogram(bins = 300),
    facet_wrap(~model),
    coord_cartesian(xlim = c(0, 0.05)),
    labs(caption = 'xlim restricted'),
    geom_vline(data = summary_df, 
               aes(xintercept = mean, linetype = "mean"), alpha = 0.5),
    geom_vline(data = summary_df,
               aes(xintercept = median, linetype = "median"), alpha = 0.5)
  )
}

# biome wide
pred_hists <- map(mod_types, function(x) {
  df <- mod_preds_dfs[[x]]
  ggplot(df, aes(probability)) +
    hist_base(df) +
    labs(title = paste(x, "models"),
         subtitle = "predicted yearly fire probability for all sagebrush biome pixels")
})

#pred_hists
# sw2 simulation extent
pred_sw2_hists <- map(mod_types, function(x) {
  df <- mod_sw2_preds_dfs[[x]]
  ggplot(df, aes(probability)) +
    hist_base(df) +
    labs(title = paste(x, "models"),
         subtitle = "predicted yearly fire probability using SW2 afgAGB and pfgAGB")
})
#pred_sw2_hists

# ***maps -----------------------------------------------------------------
# creating maps of both glm predictions
breaks_prob <- c(seq(0, 0.02, .002), 0.2)

# sagebrush biome

pred_maps <- pmap(all_mod_names, function(type, mod) {
  formula <- formulas[type]
  out <- tm_shape(rasts_pred2[[type]][[mod]], bbox = bbox) +
    tm_raster(breaks = breaks_prob,
              title = 'fire probability') +
    base +
    tm_layout(main.title = paste("predicted probability (RAP input),",
                                 type, mod, "model\n", formula),
                main.title.size = 0.5)
  
  out
})

# sw2 simulation extent
pred_sw2_maps <- pmap(all_mod_names, function(type, mod) {
  formula <- formulas[type]
  out <- tm_shape(rasts_sw2_pred1[[type]][[mod]], bbox = bbox) +
    tm_raster(breaks = breaks_prob,
              title = 'fire probability') +
    base +
    tm_layout(main.title = paste("predicted probability (SW2 input), ",
                                 type, mod, "model\n", formula),
              main.title.size = 0.5)
  
  out
})

# predicted for + 5c (sagebrush extent)
pred_maps_5c = map(names_5c, function(x) {
  tm_shape(r_pred_5c[[x]], bbox = bbox) +
  tm_raster(breaks = breaks_prob, 
            title = 'fire probability') +
  base +
  tm_layout(main.title = paste('Predicted fire probability for +5C\n',
                               x),
            main.title.size = 0.5)
})

breaks_delta <- c(-0.04, -.02, -.01, -.005, -.004, -.003, -.002, -0.001, 0, 
                  rev(c(.01, .003, .002, 0.001)))
breaks_delta0 <- c( 0.001, .002, .003, .005, .01, .04)
breaks_delta <- c(-rev(breaks_delta0), 0, breaks_delta0)

cols_delta <- c(rev(brewer.pal(8, 'Greens')[-(1:2)]),
                brewer.pal(8, 'OrRd')[-(1:2)])


# change in fire probability with 5 c warming

delta_maps <- map(names_5c, function(x) {
  tm_shape(r_delta_5c[[x]], bbox = bbox) +
    tm_raster(title = 'Delta probability',
              breaks = breaks_delta,
              palette = cols_delta) +
    base +
    tm_layout(main.title = paste0('Change fire probability with 5C warming\n', 
                                  x),
              main.title.size = 0.5)
})


# ** combine into multi panel map ------------------------------------------

# get endices of model types, for map order below
get_endices <- function(type, method) {
  which(all_mod_names$type == type &  str_detect(all_mod_names$mod, method))
}

pdf(paste0("figures/maps_fire_prob/fire_prob_biome-mask_v6_", bin_string, 
           ".pdf"),
    width = 8, height = 7)
  # paint method

  tmap_arrange(maps_fire[[1]], ncol = 2) # observed
  tmap_arrange(pred_maps[get_endices('glm', 'paint')]) # predicted
  tmap_arrange(pred_sw2_maps[get_endices('glm', 'paint')]) # predicted
  # glm resample
  tmap_arrange(pred_maps[get_endices(names_5c[2], 'paint')]) # predicted
  tmap_arrange(pred_sw2_maps[get_endices(names_5c[2], 'paint')]) # predicted
 
   # reduceToImage method
  tmap_arrange(maps_fire[[2]], ncol = 2) # observed
  tmap_arrange(pred_maps[get_endices('glm', 'reduceToImage')]) # predicted
  tmap_arrange(pred_sw2_maps[get_endices('glm', 'reduceToImage')]) # predicted
  pred_hists
  pred_sw2_hists
dev.off()

# single page sets of maps, observed, predicted
for(x in names_5c) {
  jpeg(paste0("figures/maps_fire_prob/mtbs_observed_predicted_maps_",
              x,"_v3.jpeg"),
       width = 8, height = 3.2, res = 600, units = 'in')
  print(tmap_arrange(maps_fire$paint[[1]], 
                     pred_maps[[get_endices(x, 'paint_mtbs')]], nrow = 1))
  dev.off()
  
  # also predicted for +5c, and delta with 5c warming
  jpeg(paste0("figures/maps_fire_prob/mtbs_observed_predicted_maps_",
              x,"_v2_5c.jpeg"),
       width = 8, height = 6.5, res = 600, units = 'in')
  print(tmap_arrange(maps_fire$paint[[1]], pred_maps[[get_endices(x, 'paint_mtbs')]],
                     pred_maps_5c[[x]], delta_maps[[x]], nrow = 2))
  dev.off()
}




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

title <- "\nMedian values (1986-2019)"

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


# * STEPWAT2 biomass maps -------------------------------------------------

title_sw2 <- "\nSTEPWAT2 (current, light graze, cheatgrass-fire)"
# annuals
tm_sw2_afg <- tm_shape(r_afgAGB_sw2, bbox = bbox) +
  tm_raster(breaks = breaks_bio2,
            palette = palette_bio2,
            title = lab_bio0) +
  base +
  tm_layout(main.title = paste("Annual forb and grass biomass", 
                               title_sw2))

# perennials
tm_sw2_pfg <- tm_shape(r_pfgAGB_sw2, bbox = bbox) +
  tm_raster(breaks = breaks_bio1,
            palette = palette_bio1,
            title = lab_bio0) +
  base +
  tm_layout(main.title = paste("Perennial forb and grass biomass", 
                               title_sw2))

# ** histograms -----------------------------------------------------------
bins = 100

# want same axis limits between RAP and stepwat data
afgAGB_max <- max(dfs_biome2[[1]]$afgAGB, 
                   df_sw2_sim2$afgAGB, na.rm = TRUE)

pfgAGB_max <- max(dfs_biome2[[1]]$pfgAGB, 
                  df_sw2_sim2$pfgAGB, na.rm = TRUE)

# RAP data
h0 <- ggplot(dfs_biome2[[1]]) +
  labs(subtitle = 'RAP data (biome mask)')

h1 <- h0+
  geom_histogram(aes(afgAGB), bins = bins) +
  coord_cartesian(xlim = c(0, afgAGB_max))

h2 <- h0+
  geom_histogram(aes(pfgAGB), bins = bins) +
  coord_cartesian(xlim = c(0, pfgAGB_max))
h3 <- h0+
  geom_histogram(aes(shrCover), 
                 breaks = seq(0, ceiling(max(dfs_biome2[[1]]$shrCover)), 1)) +
  labs(caption = "Cover rounded to the integer")

# STEPWAT data
h0_sw2 <- ggplot(df_sw2_sim2)+
  labs(subtitle = 'STEPWAT2 data (upscaling extent)')
  

h1_sw2 <- h0_sw2 +
  geom_histogram(aes(afgAGB), bins = bins) +
  coord_cartesian(xlim = c(0, afgAGB_max))

h2_sw2 <- h0_sw2 +
  geom_histogram(aes(pfgAGB), bins = bins) +
  coord_cartesian(xlim = c(0, pfgAGB_max))

# save maps & histograms
pdf("figures/maps_veg/RAP_bio-cover_biome-mask_v2.pdf",
    width = 8, height = 6)
tmap_arrange(tm_rap1, tm_rap2, tm_rap3, nrow = 2)
# the pfg map is given twice on this panel, so map sizing remains the same
tmap_arrange(tm_sw2_afg, tm_sw2_pfg, base, nrow = 2)
gridExtra::grid.arrange(h1, h2, h3, h1_sw2, h2_sw2, ncol = 3)
dev.off()


# * daymet data -----------------------------------------------------------

# making breaks at the low points between the the 3 humps in the prcpPropSum
# histogram, so the first hump is red, second purple, 3rd blue
first <- seq(min(dfs_biome0[[1]]$prcpPropSum, na.rm = TRUE), 0.165, 
             length.out = 4)
second <- seq(0.165, 0.265, length.out = 4)
third <- seq(0.265, max(dfs_biome0[[1]]$prcpPropSum, na.rm = TRUE),
             length.out = 4)
breaks_prop <- unique(c(first, second, third))
cols_prop <- c("#ffeda0","#feb24c", "#f03b20", # reds (from 3-class YlOrRd))
               "#fbb4b9", "#f768a1","#ae017e", # purples (from 4-class RdPu)
               "#bdc9e1", "#74a9cf", "#0570b0") # blues (from 4-class PuBu)

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
            breaks = breaks_prop,
            palette = cols_prop)  +
  base+
  tm_layout(main.title = 'Proportion of precipitation that falls in summer (Jun-Aug)\ncolored according to trimodal distribution')

# ** histograms -----------------------------------------------------------
h0 <- dfs_biome2[[1]] %>% 
  mutate(prcpPropSum_group = cut(prcpPropSum, breaks_prop)) %>% 
  ggplot() +
  labs(subtitle = 'biome mask')
bins <-  500
h4 <- h0+
  geom_histogram(aes(MAP), bins = bins) +
  coord_cartesian(xlim = c(0, 1000)) +
  labs(caption = "xlim restricted")

h5 <- h0+
  geom_histogram(aes(MAT - 273.15), bins = bins) +
  labs(x = "MAT (deg C)")

h6 <- h0+
  geom_histogram(aes(prcpPropSum, fill = prcpPropSum_group), bins = bins) +
  scale_fill_manual(values = cols_prop) +
  theme(legend.position = 'none')

h6

# save maps & histograms
pdf("figures/maps_climate/climate_biome-mask_v2.pdf",
    width = 8, height = 6)
  tmap_arrange(met1, met2, met3, nrow = 2)
  gridExtra::grid.arrange(h4, h5, h6, ncol = 2)
dev.off()
