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

# strings of model names
bin_string <- "bin20"
# string vector, part of the name of the model, usually identifying
# the model interactions
sv <-  c("", # original model (model 1)
  "_A2-T2_A-Pr", # model 4
  "_A-P_A2-T2_A-Pr", # model 4b # final 'best' model
  "_S-T_A2-T2_A-Pr", # model 6
  "_A-P_S-T_A2-T2_A-Pr", # model 6b
  "_S2-T2_A2-T2_A-Pr", # model 7
  "_7B_A-P_S2-T2_A2-T2_A-Pr" # model 7b
)

files_mod <- paste0("models/glm_binomial_models_byNFire_v2_", bin_string, "_cwf", 
                sv, ".RDS")
sv[sv == ""] <- "original"
names(files_mod) <- sv

s_target <- "_A-P_A2-T2_A-Pr" # which model (based on files_mod names) do you want
# to make publication quality figures for? This if files_mod has names
# of multiple model objects, figures for all those are just packaged 
# together pdfs for exploration

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

# here the bin_string in the file name refers to how many bins each predictor variable
# was split into before resampling, and the by NFire means that
# data was split into before/after fires for training
glm_mods_resample1 <- map(files_mod, readRDS)

mods1 <- map(glm_mods_resample1, function(x) x$paint)
formulas1 <- map_chr(glm_mods_resample1, function(x) x$formula)

# removing , 2, raw = TRUE from formula terms, just to shorten the string
formulas2 <- str_replace_all(formulas1, ",[ ]*2[ ]*,[ ]*raw[ ]*=[ ]*TRUE[ ]*", "") %>% 
  str_replace_all("[ ]*", "") %>% # getting rid of additional spaces
  # new line so prints on two lines (2nd line is interactions)
  str_replace_all("poly\\(prcpPropSum\\)", "poly\\(prcpPropSum\\)\n") 
names(formulas2) <- sv
cat(formulas2, sep = "\n")

# model that includes hmod (human modification) as an additional
# predictor variable (object created in
# "scripts/05_models_biome-mask_fire-prob_byNFire_hmod.Rmd")

glm_mods_hmod <- readRDS(
  paste0("models/glm_binomial_models_byNFire_hmod_v2_", bin_string, "_cwf.RDS"))

# checking that the hmod and regular mod of the 'target' model
# have the same interactions (i.e. are comparable)
hmod1 <- glm_mods_hmod$paint_cwf
x <- glm_mods_hmod$pred_vars_inter

# note that some of the older model objects don't include the pred_vars_inter
# list element
stopifnot(x[x!='hmod'] == mods1[[s_target]]$pred_vars_inter)
# predicted fire probability ----------------------------------------------


# model predictions 
# predicting on the 'old' data where there is just 1 row per pixel
# regardless of whether there was a fire--otherwise you have multiple
# predictions per pixel which is a bit harder to deal with
mods_pred1 <- map(mods1, predict, newdata = df1, type = "response")

hmod_pred1 <- predict(hmod1, newdata = df1,
                     type = "response")

empty <- rast_rap1[[1]]
empty[] <- NA

# filling empty raster w/ predicted values
rasts_pred1 <- map(mods_pred1, function(x) {
  out <- empty
  out[] <- x
  out
})

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

tms_pred1 <-  map(rasts_pred1, function(r) {
  tm_shape(r*100, bbox = bbox) +
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
})


jpeg(paste0("figures/maps_fire_prob/cwf_observed_predicted_pub-qual_v3", s_target, 
            ".jpeg"), units = 'in', res = 600, height = 2.6, width = 7)
  tmap_arrange(tm_obs, tms_pred1[[s_target]],  ncol = 2)
dev.off()

# predicted fire probability maps, for each model (for exploration) 

# dimensions for figure flexible depending on the number of maps
n <- length(tms_pred1)
if(n == 1) {
  width = 3.5
  height <- 2.6
  ncol <- 1
} else {
  ncol <- 2
  width <- 7
  rows <- ceiling(n/2) # number of rows
  height <- 2.6*rows
}

tms_pred2 <- map2(tms_pred1, names(tms_pred1), function(x, name) {
  x2 <- x +
    tm_credits(text = formulas2[name],
               size = 0.6,
               position = c("center", "BOTTOM"),
               bg.color = 'white') +
    tm_layout(main.title = paste(name, "model"))
  x2
})


jpeg("figures/maps_fire_prob/cwf_observed_predicted_maps_v3.jpeg",
    units = 'in', res = 600, height = height, width = width)

tmap_arrange(tms_pred2, ncol = ncol)

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
# create predictions for each model and climate scenario
# map over models
rasts_alter1 <- map2(mods1, rasts_pred1, function(mod, r) {
  # map over dataframes
  map(dfs_alter1, function(df) {

    pred <- empty
    
    # raster of predicted values for the given alteration and model
    pred[] <- predict(mod, newdata = df, type = "response")
    
    # difference between the altered prediction and the original
    # data prediction 
    delta <- pred - r
    
    list(pred = pred, delta = delta)
  })
})


# * 6 panel map -----------------------------------------------------------
# delta fire probabilities for each of 6 climate scenarios

legend.text.size <- 0.55

delta_titles0 <- c(
  "mat_warm" = paste0("+", warm, "°C MAT"),
  "mat_hot" = paste0("+", hot, "°C MAT"),
  "map_minus" = paste0("-",map_change*100, "% MAP"),
  "map_plus" = paste0("+",map_change*100, "% MAP"),
  "prop_minus" = paste0("-",prop_change*100, "% Proportion summer ppt"),
  "prop_plus" = paste0("+",prop_change*100, "% Proportion summer ppt")
)

delta_titles <- paste(fig_letters[1:length(delta_titles0)], delta_titles0)
names(delta_titles) <- names(delta_titles0)
rasts_alter2 <- rasts_alter1

# make sure elements refer to the same alteration
stopifnot(names(rasts_alter2[[1]]) == names(delta_titles))

# combining delta rasters for a given model into one raster w/ multiple layers
# creating one multilayered raster per model
rasts_delta1 <- map(rasts_alter2, function(x_list) {
  out <- map2(x_list, names(x_list), function(x, name) {
    r <- x$delta
    names(r) <- name
    r
  }) 
  rast(out)
})

# combining predicted rasters for a given model into one raster w/ multiple layers
# creating one multilayered raster per model
rasts_alter_pred1 <- map(rasts_alter2, function(x_list) {
  out <- map2(x_list, names(x_list), function(x, name) {
    r <- x$pred
    names(r) <- name
    r
  }) 
  rast(out)
})


tms_delta <- map(rasts_delta1, function(r) {
  tm_shape(r, bbox = bbox) +
    tm_raster(title = lab_delta,
              breaks = breaks_delta,
              labels = labels_delta,
              palette = cols_delta,
              midpoint = 0) +
    tm_layout(panel.labels = delta_titles,
              panel.label.bg.color = 'white')+
    tm_facets(ncol =2) +
    basemap(legend.text.size = legend.text.size)
})



# creating version for all models (for exploration)

tms_delta2 <- map2(tms_delta, names(tms_delta), function(x, name) {
  x2 <- x +
    tm_layout(main.title = paste(name, "model\n", formulas2[name]),
              main.title.size = 0.5)
  x2
})

pdf("figures/maps_sensitivity/delta-prob_clim-vars_by-mod_v2.pdf",
     height = 8.5, width = 7.5)
  tms_delta2
dev.off()


# ** pub qual version  ----------------------------------------------------
# figure 5 in manuscript
# work in progress
r <- rasts_delta1[[s_target]]

# breaks for delta temperature panels
b1  <- c( 0.1, .3, .6, 1, 2, 100) 
b1 <- c(-rev(b1), b1)

l1 <- label_creator(b1, convert2percent = FALSE)
l1[1] <- paste0("< ", b1[2])

# breaks for delta MAP and prcpPropSum maps (which have less range)
b2  <- c( 0.1, .2, .3, .4, .5, 100) # original breaks
b2 <- c(-rev(b2), b2)

l2 <- label_creator(b2, convert2percent = FALSE)
l2[1] <- paste0("< ", b2[2])


input_l <- list(names(delta_titles), #layers
                list(b1, b1, b2, b2, b2, b2), # breaks
                list(l1, l1, l2, l2, l2, l2) # labels
)
tms_target1 <- pmap(input_l, 
                function(lyr, breaks, labels) {
  title <- delta_titles[lyr]
  r <-  rasts_delta1[[s_target]][[lyr]]*100
  
  tm_shape(r, bbox = bbox2) +
    tm_raster(breaks = breaks,
              labels = labels,
              palette = cols_delta,
              midpoint = 0,
              title = lab_delta,
              legend.hist = TRUE) +
    basemap_hist() +
    tm_layout(main.title = title)

})


jpeg(paste0("figures/maps_sensitivity/delta-prob_clim-vars_v4", s_target, ".jpeg"), 
     units = 'in', res = 600, height = 8.5, width = 7.5)
tmap_arrange(tms_target1, ncol = 2)
dev.off()


# * histograms --------------------------------------------------------------

# creating 1 dataframe for each model
# where each dataframe includes predicted and delta values
# for each climate scenario
# (memory hog)
dfs_pred1 <- map(rasts_alter2, function(r_list) {
  map_dfr(r_list, function(x) {
    out <- tibble(
      pred = values_nona(x$pred)*100, # convert to percent
      delta = values_nona(x$delta)*100
    )
    out
  }, 
  .id = "scenario")
})


dfs_pred2 <- map(dfs_pred1, mutate, name = delta_titles0[scenario])

# dfs of predicted values for the observed (climate, veg) data, for each model
dfs_pred_obs <- map(rasts_pred1, function(r) {
  tibble(pred = values_nona(r)*100, # convert to percent
       scenario = "observed",
       name = "observed data")
})

# combining predicted data from climate scenarios with predictions on
# the original (observed) data
dfs_pred3 <- map2(dfs_pred_obs, dfs_pred2, function(x, y) {
  out <- bind_rows(x, y)%>% 
    mutate(name = factor(name, levels = c(delta_titles0, "observed data")))
  out
})

# summarize 
summary1 <- map_dfr(dfs_pred3, function(df) {
  out <- df %>% 
    group_by(scenario, name) %>% 
    summarize(across(c(pred, delta), 
                     .fns = list(mean = mean, median = median, min = min,
                                 max = max)),
              .groups = 'drop')
}, .id = 'model')

summary_long1 <-  summary1 %>% 
  pivot_longer(cols = c(starts_with("pred_"), starts_with("delta_")),
               names_to = c('variable', "summary_stat"),
               names_sep = "_") %>% 
  pivot_wider(names_from = 'variable') %>% 
  mutate(summary_stat = factor(summary_stat,
                               levels = c("min", "median", "mean", "max")))

# ** predicted probability ------------------------------------------------


hists_pred1 <- map2(dfs_pred3, names(dfs_pred3), function(df, name) {
  ggplot(df, aes(x = pred)) +
    geom_vline(data = summary_long1[summary_long1$model == name, ], 
               aes(xintercept = pred, color = summary_stat)) +
    geom_histogram(bins = 200) +
    facet_wrap(~name, scales = "free_x")+
    theme_bw() +
    labs(x = paste(lab_fireProbPerc, "(predicted)"),
         subtitle = paste(name, "model"),
         caption = formulas2[name])
})
#hists_pred1[[1]]

# version with fixed limits (across figures)
hists_pred2 <- map(hists_pred1, function(g) {
  g +
    # so all figures have the same limits
    coord_cartesian(xlim = range(summary_long1$pred)) 
})


# ** delta probability ----------------------------------------------------


hists_delta1 <- map2(dfs_pred3, names(dfs_pred3), function(df, name) {
  ggplot(df, aes(x = delta)) +
    geom_vline(data = summary_long1[summary_long1$model == name, ],
               aes(xintercept = delta, color = summary_stat)) +
    geom_histogram(bins = 200) +
    facet_wrap(~name, scales = "free_x")+
    theme_bw() +
    labs(x = lab_delta,
         title = paste(name, "model"),
         subtitle = 'Difference in fire probabily relative to observed data',
         caption = formulas2[name])
})
#hists_delta1[[1]]

# version with fixed limits (across figures)
hists_delta2 <- map(hists_delta1, function(g) {
  g +
    # so all figures have the same limits
    coord_cartesian(xlim = range(summary_long1$delta, na.rm = TRUE)) 
})

# this code is slow (takes several minutes)
pdf("figures/histograms/hists_pred_and_delta_by-model_v2.pdf",
    width = 8, height = 6)
  hists_pred2
  hists_pred1
  hists_delta2
  hists_delta1
dev.off()

# delta summary stats -----------------------------------------------------

quants <- map(names(rasts_delta1[[s_target]]),  function(name) {
  r <- rasts_delta1[[s_target]][[name]]
  x <- as.vector(values(r))
  out <- quantile(x, c(0, 0.05, 0.1, 0.5, 0.9, 0.95, 1), na.rm = TRUE)
  # mean absolute change
  out <- c(out, 'mean_abs' = mean(abs(x), na.rm = TRUE))
  out*100 # convert to percent change
})
names(quants) <- names(rasts_delta1[[s_target]])

# mean absolute change is larger for mat (both warm and hot) then
# map or prop changes
quants
# maps pred and delta  ------------------------------------------------


# * clim vars -------------------------------------------------------------

tm_delta_clim1 <- tm_shape(rasts_delta1[[s_target]], bbox = bbox) +
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

tm_pred_clim1 <- tm_create_prob_map(rasts_alter_pred1[[s_target]],
                                    main.title = "Predicted probability (%)",
                                    main.title.size = 0.7) +
  tm_layout(panel.labels = delta_titles0,
            panel.label.bg.color = 'white',
            legend.show = FALSE,
            panel.label.size = 0.75)+
  tm_facets(ncol =1)

jpeg(paste0("figures/maps_sensitivity/pred_delta-prob_clim-vars_v2", s_target, ".jpeg"), 
     units = 'in', res = 600, height = 12, width = 4)
  tmap_arrange(tm_pred_clim1, tm_delta_clim1, ncol = 2)
dev.off()


# delta due to hmod -------------------------------------------------------
# difference in predicted fire probability when the hmod variable is used.

# caution--make sure you actually want to use the s_target
# model to compare (i.e. make sure calculating delta from model
# with all the same terms in it (but without hmod))
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
            breaks = b2,
            labels = l2,
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
