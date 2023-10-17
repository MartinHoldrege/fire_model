# Martin Holdrege

# started 10/1/2022

# Purpose: Assess sensitivity of the fire model to changes in predictor
# variables, and create maps of the output for later use.


# dependencies ------------------------------------------------------------

# this script is useful because it reads in the rasters used below
# and dataframes to predict on (note this is not the 
# data used to fit the byNfire model)
source("src/basemaps.R")
source("src/fig_params.R")
source("src/general_functions.R")
library(RColorBrewer)
library(stars)
library(terra)
library(tidyverse)
theme_set(theme_classic())

# params ------------------------------------------------------------------

# string vector, part of the name of the model, usually identifying
# the model interactions
s <- '_ann_A-P_entire'

files_mod <- paste0("models/glm_binomial_models_v1", s, ".RDS")

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

# read in data ------------------------------------------------------------


# * raster template ---------------------------------------------------------
# provides cell numbers
template <- rast("data_processed/data_publication/cell_nums.tif")

# * burned area ----------------------------------------------------------

# area burned each year, for all sagebrush biome pixels. based on the USGS
# combined wildland fire dataset
# file created by the 02_burned_area_per_yr.js script
ba1 <- read_csv("data_processed/area_burned_by_yr_cwf_30m.csv",
                show_col_types = FALSE) %>% 
  select(area_ha, year)


# * predictor/response dataframe ------------------------------------------

df_ann1 <- read_csv('data_processed/fire-clim-veg_3yrAvg_v1.csv')

# * dataframe -------------------------------------------------------------

#df1 <- dfs_byNFire3_hmod$paint 

# * model objects ---------------------------------------------------------

# (these objects are very large ~2Gb)

# glm models fit to resampled/balanced data


mod1 <- readRDS(files_mod)
formula1 <- mod1$formula

# removing , 2, raw = TRUE from formula terms, just to shorten the string
formula2 <- str_replace_all(formula1, ",[ ]*2[ ]*,[ ]*raw[ ]*=[ ]*TRUE[ ]*", "") %>% 
  str_replace_all("[ ]*", "") %>% # getting rid of additional spaces
  # new line so prints on two lines (2nd line is interactions)
  str_replace_all("poly\\(prcpPropSum\\)", "poly\\(prcpPropSum\\)\n") 
names(formula2) <- s

# STOP--this hasn't been updated yet (the new Hmod model object will
# be needed)
# model that includes hmod (human modification) as an additional
# predictor variable (object created in
# "scripts/05_models_biome-mask_fire-prob_byNFire_hmod.Rmd")

# glm_mods_hmod <- readRDS(
#   paste0("models/glm_binomial_models_byNFire_hmod_v2_", 'bin20', "_cwf.RDS"))

# checking that the hmod and regular mod of the 'target' model
# have the same interactions (i.e. are comparable)
# hmod1 <- glm_mods_hmod$paint_cwf
# x <- glm_mods_hmod$pred_vars_inter
# 
# # note that some of the older model objects don't include the pred_vars_inter
# # list element
# stopifnot(x[x!='hmod'] == mods1[[s_target]]$pred_vars_inter)

# predicted fire probability ----------------------------------------------

df_ann2 <- df_ann1 

# model predictions 
# predicting on the 'old' data where there is just 1 row per pixel
# regardless of whether there was a fire--otherwise you have multiple
# predictions per pixel which is a bit harder to deal with
df_ann2$pred <- predict(mod1, newdata = df_ann2, type = "response")
df_avg1 <- summarize_yearly(df_ann2, mean_vars = 'pred')

#hmod_pred1 <- predict_cell_avg(hmod1, newdata = df1, type = "response")

# raster layers included observed fire occurence,
# mean prediction, and means of predictor vars
rasts1 <- fill_raster(df_avg1, template)


# rast_pred_hmod1 <- df2rast(hmod_pred1, empty)



# * Observed fire occurrence --------------------------------------------------

# prepare breaks, colors, labels
breaks <- c(-0.5, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 37)

labels <-  c(0, 1, 2, 3, 4, 5, NA)
f_max <- max(df_avg1$nfire_cwf)

# labels for n fire
labels[length(labels)] <- paste0('6-', f_max)

# labels fire fire probability
n <- 2019-1987
label_prob <- c(round(0:5/n, 3)*100, NA)
label_prob[length(label_prob)] <- paste0(c(round(6/n, 3), round(f_max/n, 3))*100,
                                           collapse = "-")


palette <- c('grey', RColorBrewer::brewer.pal(7, "YlOrRd")[-1])

stars_fPerPixel <- st_as_stars(rasts1[['nfire_cwf']])
names(stars_fPerPixel) <- 'fPerPixel'
levels <- levels(cut(0:10, breaks))
names(palette) <- levels

# create map
g_obs <- ggplot() +
  geom_stars(data = stars_fPerPixel, 
             aes(x = x, y = y, fill = cut(fPerPixel, breaks))) +
  # 
  geom_point(data = tibble(x = 1:10, y = 1:10, values = 0:9), 
             alpha = 0,
             aes(x = x, y = y, shape = cut(values, breaks))) +
  basemap_g(bbox = bbox3)+
  theme(plot.margin = margin(0, 0, 0, 45),
        legend.position = c(-0.05, 0.5),
        legend.box = 'horizontal',
        legend.margin = margin(),
        legend.box.background = element_rect(fill = "white",
                                             color = 'white'),
        #legend.title = element_rect(fill = "white")
        ) +
  scale_fill_manual(na.value = NA,
                    name = "        Probability (%)",
                    values = palette,
                    #breaks = breaks,
                    labels = label_prob,
                    # keeping all levels in the legend
                    drop = TRUE) +
  scale_shape_manual(labels = labels,
                     name = "N fires",
                     values = 1:length(labels),
                     drop = FALSE) +
  labs(subtitle = paste(fig_letters[1], "Number of observed fires (1988-2019)")) +
  make_legend_small() +
  guides(fill = guide_legend(order = 2),
         shape = guide_legend(order = 1, label.hjust = 1,
                              title.hjust = 1))
g_obs

# * predicted ------------------------------------------------------------

pal_prob <- RColorBrewer::brewer.pal(11, "RdYlBu")[9:2]
breaks_prob <- c(seq(0, 0.021, .003), 0.2)
labels_prob <- label_creator(breaks_prob, convert2percent = TRUE)
breaks_perc <- breaks_prob*100 # proportion to %
r <- rasts1[['pred']]*100

# histogram for inset
h <- hist_colored(r, 
                  palette = pal_prob, 
                  palette_breaks = breaks_perc,
                  binwidth = 0.1) +
  coord_cartesian(xlim = c(0, 3)) +
  labs(x = "Probability (%)") +
  theme(axis.title = element_text(size = 7))

mean(values_nona(r) > 3)*100 # % of data not shown in histogram

stars_pred <- st_as_stars(r)
names(stars_pred) <- 'values'

# changing labels so they reflect that actual maximum in the dataset
p_max <- round(max(values_nona(r)), 1)
labels_prob[length(labels_prob)] <- paste(breaks_perc[length(breaks_perc) -1],
                                           "to", p_max)

f1 <- labels_prob2fri(breaks_perc[-1])
labels_fri <- character(length(labels_prob))
labels_fri[1] <- paste(">", f1[1])
labels_fri[2:length(labels_fri)] <- paste(f1[-1], "to",f1[-length(f1)] )

shapes <- 1:length(pal_prob) # arbitrary values (not actually showing shapes)
names(shapes)  <- levels(cut(values_nona(r), breaks_perc)) # so NA doesn't show in legend
names(pal_prob) <- names(shapes)

g_pred1 <- ggplot() +
  geom_stars(data = stars_pred, 
           aes(x = x, y = y, fill = cut(values, breaks_perc))) +
  # to create second legend
  geom_point(data = tibble(x = 1:10, y = 1:10, values = 0:9), 
             alpha = 0,
             aes(x = x, y = y, shape = cut(values, breaks_perc))) +
  basemap_g(bbox = bbox3)+
  theme(plot.margin = margin(0, 0, 0, 0),
        legend.position = 'right',
        legend.box = 'horizontal') +
  scale_fill_manual(na.value = NA,
                    # leading spaces so title name over the labels
                    name = "        FRI (years)", 
                    values = pal_prob,
                    labels = labels_fri,
                    drop = FALSE) +
  scale_shape_manual(labels = labels_prob,
                     name = "Probability (%)",
                     values = shapes,
                     drop = FALSE) +
  labs(subtitle = paste("   ", fig_letters[2], "Modelled annual fire probability")) +
  make_legend_small() +
  guides(fill = guide_legend(order = 2, legend.title.align = 1),
         shape = guide_legend(order = 1, label.hjust = 1,
                              title.hjust = 1))
g_pred1

# add histogram inset
g_pred2 <- g_pred1 +
  inset_element(h, -0.1, 0, 0.25, 0.4, align_to = 'plot') 
#g_pred2

# combine into two paneled map (fig 2 in manuscript)
png(paste0("figures/maps_fire_prob/cwf_observed_predicted_pub-qual_v6", s, 
            ".png"), units = 'in', res = 600, height = 3, width =9)
  g_obs + g_pred2
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
                 "prop_plus" # increase in prcpPropSum
                 )

dfs_alter1 <- vector('list', length(alter_names))
for (i in 1:length(dfs_alter1)) {
  dfs_alter1[[i]] <- df_ann2
}
names(dfs_alter1) <- alter_names

warm <- 2
hot <- 5
dfs_alter1$mat_warm$MAT <- df_ann2$MAT + warm
dfs_alter1$mat_hot$MAT <- df_ann2$MAT + hot

map_change <- 0.2
dfs_alter1$map_minus$MAP <- df_ann2$MAP*(1-map_change)
dfs_alter1$map_plus$MAP <- df_ann2$MAP*(1 + map_change)

prop_change <- 0.2
dfs_alter1$prop_minus$prcpPropSum <- df_ann2$prcpPropSum*(1-prop_change)
dfs_alter1$prop_plus$prcpPropSum <- df_ann2$prcpPropSum*(1+prop_change)


# * predictions -----------------------------------------------------------
# create predictions for each model and climate scenario
# then average across years, then convert to a raster
# map over models
rasts_alter1 <- map(dfs_alter1, function(df) {

    tmp <- df_ann2[,  c('cell_num', 'year')]
    
    # raster of predicted values for the given alteration and model
    tmp$pred <- predict(mod1, newdata = df, type = "response") 
    
    tmp_avg <- summarize_yearly(tmp, weighted_vars = NULL,
                                mean_vars = 'pred', sum_vars = NULL)
    
    pred <- fill_raster(tmp_avg, template)
    
    # difference between the altered prediction and the original
    # data prediction 
    delta <- pred - rasts1[['pred']]
    
    list(pred = pred, delta = delta)
})

# * burned area -----------------------------------------------------------

plot(area_ha ~ year, data = ba1, type = 'l')

# total area of study area
study_area <- template
study_area[!is.na(study_area)] <- 1
total_area <- calc_exp_ba(study_area)
total_area
#824174

# observed mean burned area (accurately calculated based on rasterizing polygons
# to ~30 m pixels)
ba_obs <- ba1 %>% 
  filter(year >=1988) %>% 
  summarise(area_km2 = mean(area_ha)/100)
ba_obs
# 4835.

# observed burned area (based on the coarse ~1x1 km data)
ba_obs2 <- df_ann2 %>% 
  # each pixel is now exactly 1 km
  mutate(ba = nfire_cwf) %>% 
  group_by(year) %>% 
  summarize(ba = sum(ba)) %>% 
  summarize(area_km2 = mean(ba))
ba_obs2
# 4104.
ba_obs2/total_area*100 # observed fire probability

# expected (long term) mean annual burned area (ha) based on the model
ba_exp <- calc_exp_ba(rasts1[['pred']])
ba_exp
# 4374
ba_exp/total_area*100 # average modelled fire probability

# change in burned area with climate perturbations

ba_delta1 <- map_dfr(rasts_alter1, function(x) {
  calc_exp_ba(x$delta)
})
ba_delta1


# observed & predicted figs ------------------------------------------------

delta_titles0 <- c(
  "mat_warm" = paste0("+", warm, "°C MAT"),
  "mat_hot" = paste0("+", hot, "°C MAT"),
  "map_minus" = paste0("-",map_change*100, "% MAP"),
  "map_plus" = paste0("+",map_change*100, "% MAP"),
  "prop_minus" = paste0("-",prop_change*100, "% PSP"),
  "prop_plus" = paste0("+",prop_change*100, "% PSP")
)

delta_titles <- paste(fig_letters[1:length(delta_titles0)], delta_titles0)
names(delta_titles) <- names(delta_titles0)

# * delta probability histograms ---------------------------------------------

# for adding to histograms
ba_delta2 <- ba_delta1%>% 
  pivot_longer(everything()) %>% 
  mutate(percent = round(value/ba_exp*100), # % change relative to modelled under ambient conditions
         percent = paste0("(", percent, "%)"),
         # adding leading "+"
         percent = str_replace(percent, "\\((?!\\-)", "\\(\\+"),
          title = delta_titles[name],
         label = paste0(round(value), "km\U00B2"),
         label = str_replace(label, '^(?!\\-)', '+'), # addin + infront of non-negatives
         label = paste(label, percent, sep = "\n"),
         x = Inf, y = Inf)

df_delta1 <- map2_dfr(names(rasts_alter1), rasts_alter1, 
         function(lyr, x) {
           out <- get_values(1, x$delta)
           out$lyr <- lyr
           out
         }) %>% 
  # convert to %
  mutate(delta_fire_prob = value*100,
         title = factor(delta_titles[lyr], levels = delta_titles)) %>% 
  select(-value)

delta_range <- df_delta1 %>% 
  group_by(title, lyr) %>% 
  summarise(min = min(delta_fire_prob),
            max = max(delta_fire_prob))
# limits for other panels
h <- ggplot(df_delta1, aes(x = delta_fire_prob)) +
  geom_histogram(bins = 100) +
  geom_vline(data = delta_range, aes(xintercept = min, linetype = "min")) +
  geom_vline(data = delta_range, aes(xintercept = max, linetype = "max")) +
  facet_wrap(~title, scales = 'fixed', ncol = 2) +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, size = 0.7) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, size = 0.7) +
  geom_label(data = ba_delta2, aes(x = x, y = y, label = label),
            hjust = 1.0,
            vjust = 1.2,
            size = 2.5,
            label.size = NA # remove border around label
            )+
  scale_linetype_manual(values = c('max' = 3, "min" = 4)) +
  labs(x = lab_delta,
       y = 'Count') +
  theme(legend.title = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(hjust = 0),
        strip.background = element_blank()) 
  # want to avoid e style scientific notation
  # scale_y_continuous(labels = scales::label_number(scale = 10^(-5), 
  #                                           # x10^5 
  #                                          suffix = "x10\U2075"))
h
png("figures/histograms/sensitivity_delta_fire-prob_v2.png",
    height = 5, width = 4, units = 'in', res = 600)
h
dev.off()



# * 6 panel map -----------------------------------------------------------
# delta fire probabilities for each of 6 climate scenarios

legend.text.size <- 0.55


rasts_alter2 <- rasts_alter1

# make sure elements refer to the same alteration
stopifnot(names(rasts_alter2) == names(delta_titles))

# combining delta rasters for a given model into one raster w/ multiple layers
# creating one multilayered raster per model
rasts_delta1 <- map2(rasts_alter2, names(rasts_alter2), function(x, name) {
    r <- x$delta
    names(r) <- name
    r
  }) %>% 
  rast()


# combining predicted rasters for a given model into one raster w/ multiple layers
# creating one multilayered raster per model
rasts_alter_pred1 <- map2(rasts_alter2, names(rasts_alter2), function(x, name) {
  r <- x$pred
  names(r) <- name
  r
}) %>% 
  rast()



# ** pub qual version  ----------------------------------------------------
# change in the number of fires per 100 years in response to change in climate
# figure 6 in manuscript

# breaks 
b1  <- breaks_delta*100

# labels (for legend)
l1 <- labels_delta


delta_names <- names(delta_titles) %>% 
  self_name()

# histograms (for insets)
hist_l1 <- map(delta_names, function(lyr) {
  r <-  rasts_delta1[[lyr]]*100
  out <- hist_colored(r, palette = cols_delta, palette_breaks = b1,
               binwidth = 0.1)
  out
})

hist_l2 <- hist_l1
# restricting axes, so meat of distribution is more visible
hist_l2[1:2] <- map(hist_l2[1:2], function(g) {
  g + coord_cartesian(xlim = c(-0.7, 0.7))
})

hist_l2[3:6] <- map(hist_l2[3:6], function(g) {
  g + coord_cartesian(xlim = c(-0.7, 0.7))
})

maps_delta1 <-  map(delta_names, function(lyr) {
  r <-  rasts_delta1[[lyr]]*100
  
  r_star <- st_as_stars(r)
 
  # naming colors and labels so that they appropriately
  # match the cut raster values
  colors <- cols_delta
  labels <- l1
  levels <- levels(cut(-100:100, b1))
  names(colors) <- levels
  names(labels) <- levels
  
  # hack, so cut in the aes() below, works (ie. lyr always has same name
  names(r_star) <- 'values' 
  g <- ggplot() +
    geom_stars(data = r_star, 
               # values is theattribute that corresponds
               # to the values in the grid-cells
               aes(x = x, y = y, fill = cut(values, b1))) +
    basemap_g(bbox = bbox3) +
    theme(plot.margin = margin(5.5, 0, 0, 0),
          legend.position = 'left')+
    scale_fill_manual(na.value = NA,
                      name = lab_delta,
                      values = colors,
                      #breaks = b1,
                      labels = labels,
                      # keeping all levels in the legend
                      drop = FALSE) +
    labs(subtitle = delta_titles[lyr])
  g
})


# add histograms on top
maps_delta2 <- map2(maps_delta1, hist_l2, function(g, h) {
  out <- g +
    inset_element(h, 0, 0, 0.32, 0.4, align_to = 'plot') 
  out
})

#maps_delta2[[1]]

png(paste0("figures/maps_sensitivity/delta-prob_clim-vars_v7", s, ".png"), 
     units = 'in', res = 600, height = 8.5, width = 8)
wrap_plots(maps_delta2, ncol = 2) +
  plot_layout(guides = 'collect') 

dev.off()



# delta summary stats -----------------------------------------------------

quants <- map(names(rasts_delta1),  function(name) {
  r <- rasts_delta1[[name]]
  x <- as.vector(values(r))
  out <- quantile(x, c(0, 0.05, 0.1, 0.5, 0.9, 0.95, 1), na.rm = TRUE)
  # mean absolute change
  out <- c(out, 'mean_abs' = mean(abs(x), na.rm = TRUE))
  out*100 # convert to percent change
})
names(quants) <- names(rasts_delta1)

# mean absolute change is larger for mat (both warm and hot) then
# map or prop changes
quants

# delta due to hmod -------------------------------------------------------
# difference in predicted fire probability when the hmod variable is used.

# caution--make sure you actually want to use the s_target
# model to compare (i.e. make sure calculating delta from model
# with all the same terms in it (but without hmod))

if (FALSE) {
  hmod_delta <- rasts_pred1[[s_target]] - rast_pred_hmod1
  
  x <- values(hmod_delta) %>% as.vector()
  hist(x, breaks = 100, xlim = c(-0.01, 0.01))
  
  
  
  tm_hmod <- tm_create_prob_map(rast_pred_hmod1,
                                main.title = paste(fig_letters[1], "Predicted probability (%)",
                                                   '\nfor model including human modification'),
                                main.title.size = 0.8,
                                legend.title.size = 0.6)
  
  tm_hmod_delta <- tm_shape(hmod_delta*100, bbox = bbox) +
    tm_raster(title = lab_delta,
              breaks = b1,
              labels = l1,
              palette = cols_delta,
              midpoint = 0) +
    basemap(legend.text.size = legend.text.size,
            legend.title.size = 0.6) +
    tm_layout(main.title = paste(fig_letters[2],
                                 "Change in fire probability relative to model",
                                 "\nwithout human modification"),
              main.title.size = 0.8)
  
  
  
  jpeg("figures/maps_fire_prob/cwf_hmod_predicted_v2.jpeg", units = 'in', res = 600,
       height = 2.8, width = 7)
  tmap_arrange(tm_hmod, tm_hmod_delta, nrow = 1)
  dev.off()
}

