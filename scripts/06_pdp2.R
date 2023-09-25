# Martin Holdrege

# Script started 6/1/2022

# Purpose--create partial dependence plots that show multiple
# lines for a given variable, e.g. also show the annuals trend when
# MAP is fixed at it's 80th or 20th percentile


# dependencies ------------------------------------------------------------

source("src/fig_params.R")
source("src/general_functions.R")
library(vip)
library(tidyverse)
library(ggtext)
theme_set(theme_classic())


# params ------------------------------------------------------------------

# string vector, part of the name of the model, usually identifying
# the model interactions

s <- "_A-P_A2-T2_A-Pr"#, # model 4b (final model)

limit_axes <- FALSE

# read in model objects ---------------------------------------------------

# main model
mods <- readRDS(paste0("models/glm_binomial_models_byNFire_v2_bin20_cwf",
                       s, ".RDS"))
mod <- mods$paint_cwf



# create pdp --------------------------------------------------------------

# lookup table
lookup_var <- var2lab(x = NULL, units_md = TRUE)
mod_vars <- names(lookup_var)
names(mod_vars) <- mod_vars
  
# * prepare 'training' datasets -------------------------------------------
# datasets with a given variable fixed at a certain percentile. 

train1 <- mod$data 

# train1 <- slice_sample(train1, n = 1000) # for testing

probs <- c(0.0001, 0.9999)
quantiles <- map(train1[mod_vars], \(x) quantile(x, probs = probs))
quantiles
vars <- quantiles[c('MAT', 'MAP', 'pfgAGB', 'afgAGB')]
train_l1 <- map2(quantiles, names(quantiles), function(quant, var) {
  out <- list()
  df <- train1
  df[[var]] <- quant[1]
  out$low <- df
  df[[var]] <- quant[2]
  out$high <- df
  out
})

train_l2 <- flatten_rename(train_l1)
  
dfs_pdp <- map(mod_vars, function(var) {
  out <- pdp::partial(mod, pred.var = var, plot = FALSE,
               prob = TRUE, train = train1,
               parallel = TRUE)
  out$inter_var <- 'none'
  out
}) 

# for showing interactions
inter_l <- list(
  # first element of vector is the variable plotted on the pdp plot
  # the second is the variable fixed at a given percentile
  c('afgAGB', 'MAT_low'),
  c('afgAGB', 'MAT_high'),
  c('afgAGB', 'MAP_low'),
  c('afgAGB', 'MAP_high'),
  c('afgAGB', 'pfgAGB_low'),
  c('afgAGB', 'pfgAGB_high'),
  c('MAT', 'afgAGB_low'),
  c('MAT', 'afgAGB_high'),
  c('MAP', 'afgAGB_low'),
  c('MAP', 'afgAGB_high'),
  c('pfgAGB', 'afgAGB_low'),
  c('pfgAGB', 'afgAGB_high')
)  
  
dfs_inter1 <- map(inter_l, function(x) {
  var <- x[1]
  out <- pdp::partial(mod, pred.var = var, plot = FALSE,
               prob = TRUE, train = train_l2[[x[2]]])
  # interacting variable
  out$inter_var <- x[2]
  out
}) 

df_pdp1 <- map(c(dfs_pdp, dfs_inter1), function(df) {
    var_name <- names(df)[1]
    
    out <- as_tibble(df)
    names(out) <- c("x_value", "yhat", 'inter_var')
    out$variable <- var_name
    out
  }) %>% 
  bind_rows() %>% 
  mutate(x_value = ifelse(variable == 'MAT', 
                           x_value  - 273.15, # k to c  
                           x_value
                           ))
  
df_pdp2 <- df_pdp1 %>% 
  mutate(xlab = lookup_var[variable],
         xlab = factor(xlab, levels = unique(xlab)))


  
# for rug plot 
deciles <- train1 %>% 
  select(all_of(names(lookup_var))) %>% 
  map(quantile, probs = seq(0, 1, 0.1)) %>% 
  bind_cols() %>% 
  mutate(MAT = MAT - 273.15 # k to c
  ) %>% 
  pivot_longer(cols = everything(), 
               names_to = "variable",
               values_to = "decile") %>% 
  mutate(xlab = lookup_var[variable],
         xlab = factor(xlab, levels = unique(xlab))) %>% 
  group_by(variable) %>% 
  # don't want to plot the first and last deciles (i.e. min/max values)
  # b/ they stretch the xlims to areas with very little data
  filter(decile != max(decile),
         decile != min(decile))
  


letter_df <- tibble(
  letter = fig_letters[1:length(mod_vars)],
  xlab = factor(lookup_var),
  x_value = -Inf,
  yhat = Inf
)


base_pdp <- function(limit_x = FALSE) {
  out <- list(
    facet_wrap(~xlab, scales = 'free', strip.position = "bottom"),
    theme(strip.text = element_markdown(),
          strip.placement = "outside",
          axis.title.x = element_blank(),
          strip.background = element_blank()),
    labs(y = "Annual fire probability (%)"),
    sec_axis_fri(), # adding second axis (fire return interval)
    "limit_x" = ggh4x::facetted_pos_scales(
      x = list(xlab == lookup_var['MAP'] ~ scale_x_continuous(limits = c(0, 1000)),
               xlab == lookup_var['MAT'] ~ scale_x_continuous(limits = c(0, 20)))
    ))
  
  out
}

ggplot(df_pdp2, aes(x_value, yhat, color = inter_var)) +
  geom_line() +
  facet_wrap(~xlab, scales = 'free')
png(paste0("figures/pdp/pdp_high-low_v2", s, ".png"),
    units = "in", res = 600,  width = 8, height = 3.5)
ggplot(df_pdp2, aes(x_value, yhat*100)) +
  geom_line(aes(color = inter_var)) +
  geom_rug(data = deciles, aes(x = decile, y = NULL), sides = 'b') +
  geom_text(data = letter_df, aes(label = letter),
            hjust = -0.8,
            vjust = 1) +
  # if limit_axes is true, then dataset was already filtered,
  # so additional limits should not be set
  base_pdp() +
  scale_color_manual(values = c('#006837', '#66bd63',
                       '#2166ac', '#92c5de',
                       '#b2182b', '#f4a582',
                       'black',
                       '#66c2a5', '#abdda4')) +
  labs(caption = paste('percentiles used:', paste0(probs*100, "%", collapse = ', ')))


dev.off()
  
  
 