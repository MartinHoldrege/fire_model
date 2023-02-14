# Purpose:
# compare observed and predicted fire probability values in multiple ways
# including making 'quantile' like plots but base them on averages of geographic
# blocks
# Also examine 'residuals' (of average fire probabilities)
# to understand if these pseudo residuals looks ok for predictions
# along the range of annuals

# Martin Holdrege

# script started 2/13/2023


# params ------------------------------------------------------------------

# string vector, part of the name of the model, usually identifying
# the model interactions
sv <-  c("", "_S-T_A-T", "_A-T_A-Pr", "_A2-T2_A-Pr", "_S-T_A-T_A-Pr",
         "_S-T_A2-T2_A-Pr", "_S2-T2_A2-T2_A-Pr", "_S2-T2_A2-T2_A2-Pr2")

# whether to create quantile plots based on 10,000 bins (which is slow)
run_10k <- TRUE 


# dependencies ------------------------------------------------------------

source("src/general_functions.R")
source("src/fig_params.R")
library(tidyverse)
library(terra)
theme_set(theme_classic())

# read in data ------------------------------------------------------------
# model objects created in 
# scripts/05_models_biome-mask_fire-prob_byNFire.R

files_mod <- paste0("models/glm_binomial_models_byNFire_v2_bin20_cwf", 
                    sv, ".RDS")

sv[sv == ""] <- "original"
names(files_mod) <- sv

mods1 <- map(files_mod, readRDS)

# raster--used below for spatial aggregation
# actual values in raster aren't of importance (but resolution
# and extent are)
r <- terra::rast("data_processed/RAP/RAP_afgAGB-pfgAGB_byNFire_1986-2019_mean_1000m_sagebrush-biome-mask_v1.tif")[[1]]

# predict -----------------------------------------------------------------

dfs1 <- map(mods1, function(x) {
  mod <- x$paint_cwf
  data <- mod$data
  data$cwf_prop_pred <- predict(mod, type = "response")
  data$MAT <- data$MAT - 273.15 # k to c
  data
})


# spatial grouping --------------------------------------------------------

df_cell_nums <- group_cell_nums(r, fact = 100)

# visualize the grouping:
# r_groups <- r
# r_groups[] <- df_cell_nums$group_cell_num
# plot(r_groups)


# summarise ----------------------------------------------------------

# calculating mean of predicted for each percentile of afgAGB
pred_vars <- c("afgAGB", "pfgAGB", "MAT", "MAP", "prcpPropSum")
response_vars <- c("cwf_prop", "cwf_prop_pred")

# * by cell number groups -------------------------------------------------

# naming: bgcn = 'by group cell numbers"
dfs_bgcn1 <- map(dfs1, function(df) {
    out <- df %>% 
      left_join(df_cell_nums, by = "cell_num") %>% 
      pivot_longer(cols = all_of(pred_vars)) %>% 
      group_by(group_cell_num, name) %>% 
      summarize(across(c(all_of(response_vars), value),
                       .fns = weighted.mean,
                       w = numYrs),
                .groups = "drop") %>% 
      # rename so that downstream plotting functions work 
      rename(mean_value = value) 
    out
  })

# only keeping groups with more than this number of
# cells 
min_g_size <- 1000

big_groups <- dfs1[[1]] %>% 
  select(cell_num) %>% 
  left_join(df_cell_nums, by = "cell_num") %>% 
  group_by(group_cell_num) %>% 
  summarise(n = n()) %>% 
  filter(n >= min_g_size) %>% 
  pull(group_cell_num)

n_groups <- length(big_groups)
dfs_bgcn2 <- map(dfs_bgcn1, filter, group_cell_num %in% big_groups)

# * by quantile -----------------------------------------------------------

# by percentile (100 bins)
quant1 <- map_dfr(dfs1, predvars2deciles, 
                  response_vars = response_vars, pred_vars = pred_vars,
                  .id = 'model') %>% 
  # predvars2deciles addes herbAGB variable by default (which not interested
  # in here)
  filter(name %in% pred_vars)

quant2 <- quant1 %>% 
  mutate(residual = cwf_prop - cwf_prop_pred,
         model = factor(model, levels = sv))

# by 1000 bins
quant1k <-   map_dfr(dfs1, predvars2deciles, 
                     response_vars = response_vars, pred_vars = pred_vars,
                     cut_points = seq(0, 1, 0.001),
                     .id = 'model') %>% 
  mutate(model = factor(model, levels = sv))

# by 10000 bins
if(run_10k){
quant10k <-   map_dfr(dfs1, predvars2deciles, 
                     response_vars = response_vars, pred_vars = pred_vars,
                     cut_points = seq(0, 1, 0.0001),
                     .id = 'model') %>% 
  mutate(model = factor(model, levels = sv))
}
# figures -----------------------------------------------------------------


# bgcn plots --------------------------------------------------------------
# 'quantile plots' but instead of showing quantiles on the, they
# 

set.seed(123)
plots_bgcn <- map2(dfs_bgcn2, names(dfs_bgcn1), function(df, name) {
  g <- decile_dotplot_pq(df) +
    geom_smooth(aes(y = cwf_prop, color = "Observed"), se = FALSE) +
    geom_smooth(aes(y = cwf_prop_pred, color = 'Predicted'), se = FALSE) +
    labs(subtitle = paste(name, "model"),
         caption = paste("values represent averages of observations belonging to each of", 
                         n_groups, 
                         "geographical squares\n",
                         "(each containing at least", min_g_size, "observations)")) +
    theme(plot.caption = element_text(size = 6))
  # observed and predicted values are repeated in all panels
  # so just keeping one copy
  df2 <- df %>% 
    filter(name == "afgAGB")
  out <- add_dotplot_inset(g, df2, add_smooth = TRUE, method = 'loess')
})

pdf("figures/spatial_grouping/obs_and_pred_by_group_v1.pdf", 
    width = 6, height = 4.5)
  plots_bgcn
dev.off()


# * quantile (1k bins) ----------------------------------------------

plots_1k <- split(quant1k, quant1k$model) %>% 
  map2(., names(.), function(df, name) {
    g <- decile_dotplot_pq(df) +
      labs(subtitle = paste(name, "model"),
           caption = 'predictor variables cut into 1000 bins (based on quantile)')+
      theme(plot.caption = element_text(size = 6))
    
    
    out <- add_dotplot_inset(g, df, add_smooth = TRUE, method = 'loess')
    out
  })

pdf("figures/quantile_plots/quantile_plot_1k_by_mod_v1.pdf", 
    width = 6, height = 4.5)
plots_1k 
dev.off()

# * quantile (10k bins) ----------------------------------------------

if(run_10k){
plots_10k <- split(quant10k, quant10k$model) %>% 
  map2(., names(.), function(df, name) {
    g <- decile_dotplot_pq(df) +
      labs(subtitle = paste(name, "model"),
           caption = 'predictor variables cut into 10,000 bins (based on quantile)')+
      theme(plot.caption = element_text(size = 6))
    
    out <- add_dotplot_inset(g, df, add_smooth = FALSE)
    out
  })

pdf("figures/quantile_plots/quantile_plot_10k_by_mod_v1.pdf", 
    width = 6, height = 4.5)
plots_10k
dev.off()
}

# * residuals -------------------------------------------------------------

# residuals vs predictor variabls

residual_plot <- function(df, xlab) {
  ggplot(df, aes(mean_value, residual*100)) +
    geom_point() +
    geom_abline(slope = 0, intercept = 0) + 
    geom_smooth(se = FALSE) +
    facet_wrap(~model) +
    labs(x = xlab,
         y = "Observed - Predicted (%)",
         subtitle = "Residuals by model",
         caption = "Points based on mean observed and predicted values
         for each percentile of the predictor variable")
}


quant3 <- split(quant2, quant2$name, drop = TRUE)

plots <- map2(quant3, var2lab(names(quant3)), residual_plot)

pdf("figures/residuals/residuals_v1.pdf")
plots
dev.off()