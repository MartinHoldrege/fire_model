# Martin Holdrege

# Script started 6/1/2022

# Purpose--create partial dependence plots that show multiple
# lines for a given variable, e.g. also show the annuals trend when
# MAP is fixed at it's 80th or 20th percentile


# dependencies ------------------------------------------------------------

source("src/fig_params.R")
source("src/general_functions.R")
source("src/fig_params.R")
source("src/modeling_functions.R")
library(vip)
library(tidyverse)
library(ggtext)
theme_set(theme_classic())

# params ------------------------------------------------------------------

# string vector, part of the name of the model, usually identifying
# the model interactions

s <- '_annf3_A-P_entire'

# quantiles to fix interacting terms at
probs <- c(0.2, 0.8)

pred_vars <- c("afgAGB", "pfgAGB", "MAT", "MAP", "prcpPropSum")
var_prop <- c('cwf_prop')

n_pdp <- 1e6# number of data points to use for pdp plots

n_quant <-  3e7 # number of data points to use for quantile plots

# lookup table
lookup_var <- var2lab(x = NULL, units_md = TRUE)
mod_vars <- names(lookup_var)
names(mod_vars) <- mod_vars

# read in data ------------------------------------------------------------

df_ann1 <- read_csv("data_processed/fire-clim-veg_3yrAvg_v2.csv",
                  show_col_types = FALSE)

# * read in model objects ---------------------------------------------------

# main model (fit to the entire dataset)
v <- 3
mod <- readRDS(paste0("models/glm_binomial_models_v",v,
                       s, ".RDS"))

# subsample data ----------------------------------------------------------
set.seed(123)
df4pdp <- df_ann1 %>% 
  slice_sample(n = n_pdp)

df4quant <- df_ann1 %>% 
  slice_sample(n = n_quant)

# Quantile plots ----------------------------------------------------------

# *model predictions -------------------------------------------------------

# create prediction for each each model
# (i.e. for each fire proporation variable)
predict_by_response <- function(mod, df) {
  df_out <- df
  
  response_name <- paste0(var_prop, "_pred")
  df_out[[response_name]] <- predict(mod, df, type = 'response')
  df_out
}

pred_glm1 <- predict_by_response(mod, df4quant)

var_prop_pred <- paste0(var_prop, "_pred")
response_vars <- c(var_prop, var_prop_pred)


# * filtered quantile plot ------------------------------------------------

# 
# Filtered 'Decile' plots of data. These plots show each vegetation variable,
# but only based on data that falls into the upper and lower two deciles of
# each climate variable. 

clim_vars <- c("MAT", "MAP", "prcpPropSum")
pred_glm1_deciles_filt <- predvars2deciles( pred_glm1, 
                                            response_vars = response_vars,
                                            pred_vars = pred_vars,
                                            filter_var = TRUE,
                                            filter_vars = pred_vars) 

# ~~~ x limit objects ~~~~
# want to keep the axis limits consistent between figures
x_lims <- pred_glm1_deciles_filt %>% 
  mutate(mean_value = ifelse(name == 'MAT', mean_value - 273.15, mean_value)) %>% 
  group_by(name) %>% 
  summarise(min = floor(min(mean_value)),
            max = max(mean_value)) %>% 
  mutate(max = ifelse(name == 'prcpPropSum', max, ceiling(max)))

x_lims_l <- split(x_lims, x_lims$name) %>% 
  map(., function(df) {
    c(df$min, df$max)
  })

x_lims_ggh4x <- ggh4x::facetted_pos_scales(
  x = list(name == lookup_var['afgAGB'] ~ scale_x_continuous(limits = x_lims_l$afgAGB),
           name == lookup_var['pfgAGB'] ~ scale_x_continuous(limits = x_lims_l$pfgAGB),
           name == lookup_var['MAT'] ~ scale_x_continuous(limits = x_lims_l$MAT),
           name == lookup_var['MAP'] ~ scale_x_continuous(limits = x_lims_l$MAP),
           name == lookup_var['prcpPropSum'] ~ scale_x_continuous(limits = x_lims_l$prcpPropSum)
           )
)

# ~~~ end xlimit objections ~~~

df_l1 <- pred_glm1_deciles_filt %>% 
  dplyr::filter(name %in% c("afgAGB", "pfgAGB")) %>% 
  split(., .$name, drop = TRUE)

# list of lists, 
df_l2 <- map(df_l1, function(df) split(df, df$filter_var))

# one list element for veg var and filter var
df_l3 <- flatten_rename(df_l2)

inset_l1 <- map(df_l3, create_inset_filt)

l <- 0.01 # left
b <- c(0.18, 0.515, 0.85) + 0.02 #bottom
w <- 0.36 # width
h <- 0.11 #height

# letters of insets correspond to the panels they belong in
A <- inset_element(inset_l1$afgAGB_MAT, l, b[3], l + w, b[3] + h)
B <- inset_element(inset_l1$pfgAGB_MAT, l, b[3], l + w, b[3] + h)
C <- inset_element(inset_l1$afgAGB_MAP, l, b[2], l + w, b[2] + h)
D <- inset_element(inset_l1$pfgAGB_MAP, l, b[2], l + w, b[2] + h)
E <- inset_element(inset_l1$afgAGB_prcpPropSum, l, b[1], l + w, b[1] + h)
f <- inset_element(inset_l1$pfgAGB_prcpPropSum, l, b[1], l + w, b[1] + h)

g2 <- pred_glm1_deciles_filt %>% 
  filter(filter_var %in% clim_vars) %>% 
  decile_dotplot_filtered_pq2(insets_left = list(A, C, E),
                              insets_right = list(B, D, f),
                              ylim = c(0, 4))
g2
# add insets

png(paste0("figures/quantile_plots/quantile_plot_filtered_insets_v3",
           s, ".png"), units = "in", res = 600, width = 8, height = 8)
print(g2)
dev.off()

# * quantile plot ---------------------------------------------------------

# Binning predictor variables into deciles (actually percentiles) and looking at the mean
# predicted probability for each percentile. The use of the word deciles
# is just a legacy thing (they started out being actual deciles)
# 
# Then predicting on an identical dataset but with warming



pred_glm1_deciles <- predvars2deciles(pred_glm1,
                                      response_vars = response_vars,
                                      pred_vars = pred_vars)

# Publication quality quantile plot

# publication quality version
g <- decile_dotplot_pq(pred_glm1_deciles) +
  x_lims_ggh4x

# obs/pred inset
g2 <- add_dotplot_inset(g, pred_glm1_deciles)

png(paste0("figures/quantile_plots/quantile_plot_v5", s,  ".png"), 
    units = "in", res = 600, width = 5.5, height = 3.5 )
print(g2)
dev.off()

# create pdp --------------------------------------------------------------


  
# * prepare 'training' datasets -------------------------------------------
# datasets with a given variable fixed at a certain percentile. 

train1 <- df4pdp

# train1 <- slice_sample(train1, n = 1000) # for testing


v <- case_when(
  probs[1] == 0.2 ~ 'v1',
  probs[1] == 0.01 ~ 'v2',
  TRUE ~ paste0('v', probs[1])
)

quantiles <- map(df_ann1[mod_vars], \(x) quantile(x, probs = probs))
quantiles
vars <- quantiles[mod_vars]
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
               parallel = FALSE)
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
  c('afgAGB', 'prcpPropSum_low'),
  c('afgAGB', 'prcpPropSum_high'),
  c('MAT', 'afgAGB_low'),
  c('MAT', 'afgAGB_high'),
  c('MAP', 'afgAGB_low'),
  c('MAP', 'afgAGB_high'),
  c('pfgAGB', 'afgAGB_low'),
  c('pfgAGB', 'afgAGB_high'),
  c('prcpPropSum', 'afgAGB_low'),
  c('prcpPropSum', 'afgAGB_high')
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
  mutate(name = lookup_var[variable],
         name = factor(name, levels = unique(name)))


  
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
  mutate(name = lookup_var[variable],
         name = factor(name, levels = unique(name))) %>% 
  group_by(variable) %>% 
  # don't want to plot the first and last deciles (i.e. min/max values)
  # b/ they stretch the xlims to areas with very little data
  filter(decile != max(decile),
         decile != min(decile))
  


letter_df <- tibble(
  letter = fig_letters[1:length(mod_vars)],
  name = factor(lookup_var),
  x_value = -Inf,
  yhat = Inf
)


base_pdp <- function(limit_x = FALSE, scales = 'free_x') {
  out <- list(
    facet_wrap(~name, scales = scales, strip.position = "bottom"),
    theme(strip.text = element_markdown(),
          strip.placement = "outside",
          axis.title.x = element_blank(),
          strip.background = element_blank()),
    labs(y = "Annual fire probability (%)"),
    sec_axis_fri(), # adding second axis (fire return interval)
    x_lims_ggh4x
)
  
  out
}


g <- ggplot(df_pdp2, aes(x_value, yhat*100)) +
  geom_line(aes(color = inter_var, linetype = inter_var)) +
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
                                '#66c2a5', '#abdda4',
                                '#8e0152', '#de77ae'
                                
  )) +
  #scale_linetype_manual(values = c(rep(c(1,2), 3), 1, rep(c(1,2), 2))) +
  labs(caption = paste('percentiles used:', paste0(probs*100, "%", collapse = ', '))) 
g

png(paste0("figures/pdp/pdp_high-low_", v, s, ".png"),
    units = "in", res = 600,  width = 8, height = 5)
print(g)
dev.off()
  



 