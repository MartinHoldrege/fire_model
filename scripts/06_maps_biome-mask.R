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

# created in 05_models_biome-mask_fire-prob.Rmd
# (this object is very large ~2Gb)
glm_mods1 <- readRDS("models/glm_binomial_models_v1.RDS")


# * sw2 biomass -----------------------------------------------------------

rasts_bio1 <- rast("../grazing_effects/data_processed/interpolated_rasters/bio_future_median_across_GCMs.tif")


# predicted fire probability ----------------------------------------------

# * biome-extent ----------------------------------------------------------

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
pfgAGB <- sum(rasts_bio1[[lyrs_perennial]]) %>% 
  values() %>% 
  as.vector()

df_sw2sim1 <- tibble(
  pfgAGB = pfgAGB
)

df_sw2sim1$afgAGB <- sum(rasts_bio1[[lyrs_annual]]) %>% 
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

df_sw2_sim2 <- df_sw2sim1 %>% 
  drop_na()


# ** prediction -----------------------------------------------------------

glm_sw2_preds1 <- map(glm_mods2, predict, newdata = df_sw2_sim2,
                  type = "response")

glm_sw2_preds_df <- bind_cols(glm_sw2_preds1) %>% 
  pivot_longer(cols = everything(),
               names_to = 'model',
               values_to = 'probability') 

# filling empty raster w/ predicted values
rasts_sw2_pred1 <- map(glm_sw2_preds1, function(x) {
  empty_sw2 <- rasts_bio1[[1]]
  empty_sw2[!is.na(empty_sw2)] <- x
  empty_sw2
})


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
pred_hist <- ggplot(glm_preds_df, aes(probability)) +
  hist_base(glm_preds_df) +
  labs(subtitle = "predicted yearly fire probability for all sagebrush biome pixels, for each GLM")

#pred_hist
# sw2 simulation extent
pred_sw2_hist <- ggplot(glm_sw2_preds_df, aes(probability)) +
  hist_base(glm_sw2_preds_df) +
  labs(subtitle = "predicted yearly fire probability using SW2 afgAGB and pfgAGB, for each GLM")
#pred_sw2_hist
# ***maps -----------------------------------------------------------------

breaks_prob <- c(seq(0, 0.02, .002), 0.2)
# sagebrush biome
pred_maps <- map(names(rasts_pred2), function(lyr) {
  out <- tm_shape(rasts_pred2[[lyr]], bbox = bbox) +
    tm_raster(breaks = breaks_prob,
              title = 'fire probability') +
    base +
    tm_layout(main.title = paste("predicted fire probability (RAP input),",
                                 lyr, "model\n", formula),
                main.title.size = 0.5)
  
  out
})

# sw2 simulation extent
pred_sw2_maps <- map(names(rasts_sw2_pred1), function(lyr) {
  out <- tm_shape(rasts_sw2_pred1[[lyr]], bbox = bbox) +
    tm_raster(breaks = breaks_prob,
              title = 'fire probability') +
    base +
    tm_layout(main.title = paste("predicted fire probability (SW2 input), ",
                                 lyr, "model\n", formula),
              main.title.size = 0.5)
  
  out
})

# ** combine into multi panel map ------------------------------------------

pdf("figures/maps_fire_prob/fire_prob_biome-mask_v2.pdf",
    width = 8, height = 7)
  tmap_arrange(maps_fire[[1]], ncol = 2) # observed
  tmap_arrange(pred_maps[1:4]) # predicted
  tmap_arrange(pred_sw2_maps[1:4]) # predicted
  tmap_arrange(maps_fire[[2]], ncol = 2)
  tmap_arrange(pred_maps[-(1:4)])
  tmap_arrange(pred_sw2_maps[-(1:4)])
  pred_hist
  pred_sw2_hist
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
pdf("figures/maps_veg/RAP_bio-cover_biome-mask_v1.pdf",
    width = 8, height = 6)
tmap_arrange(tm_rap1, tm_rap2, tm_rap3, nrow = 2)
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
