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

# memory heavy script so smaller samples may be needed depending on computer
n_pdp <- 1e5 # number of data points to use for pdp plots (I can run at 1e6)

# note--on a 32 gb memory machine this just barely runs using the full dataset
# (otherwise just subsample down to a couple million points)
n_quant <-  3e7 # number of data points to use for quantile plots (if > 2.5*10^7 then using entire dataset)

# save quantile and pdp plots to file?
save_quant <- TRUE
save_pdp <- TRUE
# lookup table
lookup_var <- var2lab(x = NULL, units_md = TRUE)
mod_vars <- names(lookup_var)
names(mod_vars) <- mod_vars

lookup_var_h <- var2lab(x = NULL, units_md = TRUE, include_hmod = TRUE)
mod_vars_h <- names(lookup_var_h)
names(mod_vars_h) <- mod_vars_h

# read in data ------------------------------------------------------------

df_ann1 <- read_csv("data_processed/fire-clim-veg_3yrAvg_v2.csv",
                  show_col_types = FALSE) %>% 
  # removing excess rows for memory saving
  select(-matches('burn_frac'), -matches('nfire_cwf_centroid'),
         -matches("nfire_cwf"), -matches('weight'))

# * read in model objects ---------------------------------------------------

# main model (fit to the entire dataset)

mod <- readRDS(paste0("models/glm_binomial_models_v3",
                       s, ".RDS"))

# reading in hmod later b/ summarizing for filtered quantile plot uses so much memory

# subsample data ----------------------------------------------------------
set.seed(123)
df4pdp <- df_ann1 %>% 
  slice_sample(n = n_pdp)

df4quant <- if(n_quant >= nrow(df_ann1)) {
  df_ann1
} else {
  df_ann1 %>% 
    slice_sample(n = n_quant)
}


# variable importance -----------------------------------------------------

g <- vip(mod, num_features = 17)

lookup <- c("stats::poly(I(log10(I(afgAGB + 1))), 2, raw = TRUE)1" = 'log(afgAGB + 1)', 
  "stats::poly(I(log10(I(afgAGB + 1))), 2, raw = TRUE)2" = 'log(afgAGB + 1)^2', 
  "stats::poly(I(sqrt(pfgAGB)), 2, raw = TRUE)1" ='sqrt(pfgAGB)', 
  "stats::poly(I(sqrt(pfgAGB)), 2, raw = TRUE)2" = 'pfgAGB',
  "MAT" = 'MAT',
  "stats::poly(I(log10(I(MAP + 1))), 2, raw = TRUE)1" = 'log(MAP + 1)', 
  "stats::poly(I(log10(I(MAP + 1))), 2, raw = TRUE)2" = 'log(MAP + 1)^2', 
  "stats::poly(I(log10(I(prcpPropSum + 0.001))), 2, raw = TRUE)1" = 'log(prcpPropSum + 0.001)', 
  "stats::poly(I(log10(I(prcpPropSum + 0.001))), 2, raw = TRUE)2" = 'log(prcpPropSum + 0.001)^2',
  "log10(I(afgAGB + 1)):log10(I(MAP + 1))" = 'log(afgAGB + 1):log(MAP + 1)',
  "stats::poly(I(log10(I(hmod + 1))), 2, raw = TRUE)1" = 'log(Hmod + 1)',
  "stats::poly(I(log10(I(hmod + 1))), 2, raw = TRUE)2" = 'log(Hmod + 1)^2',
  '(Intercept)' = 'Intercept'
  )

string_rename_vars <- function(x) {
  x %>% 
    str_replace_all("prcpPropSum", "PSP") %>% 
    str_replace_all("afgAGB", "AFG") %>% 
    str_replace_all("pfgAGB", "PFG") %>% 
    str_replace_all('MAT', "T") %>% 
    str_replace_all("MAP", 'P')
}
g$data$Variable <- lookup[g$data$Variable] %>% 
  # renaming based on updated acronyms used the paper
  string_rename_vars()

# figure for appendix
jpeg(paste0('figures/vip_v3_glm', s, '.jpeg'), 
     width = 3, height = 3, units = 'in', res = 600)
print(g)
dev.off()


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

saveRDS(x_lims, 'data_processed/x_lims_for_quant_plots.RDS')

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

# ~~~ end xlimit objects ~~~

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
#g2
# add insets

if(save_quant){
# figure 5
png(paste0("figures/quantile_plots/quantile_plot_filtered_insets_v3",
           s, ".png"), units = "in", res = 600, width = 8, height = 8)
print(g2)
dev.off()
}
# * quantile plot ---------------------------------------------------------

# Binning predictor variables into deciles (actually percentiles) and looking at the mean
# predicted probability for each percentile. The use of the word deciles
# is just a legacy thing (they started out being actual deciles)


pred_deciles <- predvars2deciles(pred_glm1,
                                      response_vars = response_vars,
                                      pred_vars = pred_vars)

# Publication quality quantile plot

# publication quality version
g <- decile_dotplot_pq(pred_deciles) +
  x_lims_ggh4x

# obs/pred inset
g2 <- add_dotplot_inset(g, pred_deciles)

if(save_quant){
  # figure 4
png(paste0("figures/quantile_plots/quantile_plot_v5", s,  ".png"), 
    units = "in", res = 600, width = 5.5, height = 3.5 )
print(g2)
dev.off()
}


# ** for hmod model -------------------------------------------------------
  
hmod <- readRDS(paste0("models/glm_binomial_models_v3_hmod",
                       s, ".RDS"))

pred_glm_h1 <- predict_by_response(hmod, df4quant) %>% 
  filter(!is.na(hmod))

pred_deciles <- predvars2deciles(pred_glm_h1,
                                      response_vars = response_vars,
                                      pred_vars = c(pred_vars, 'hmod'))


g <- decile_dotplot_pq(pred_deciles)


if(save_quant){
  # figure for appendix
  png(paste0("figures/quantile_plots/quantile_plot_v5_hmod", s,  ".png"), 
      units = "in", res = 600, width = 5.5, height = 3.5 )
  print(g)
  dev.off()
}

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

df_pdp1 <- df_pdp1 %>% 
  left_join(x_lims, by = c("variable" = "name")) %>% 
  filter(x_value >= min, x_value <= max) %>% 
  select(-min, -max)
# renaming variables for plotting
n <- length(unique(df_pdp1$inter_var)) -1
df_pdp2 <-df_pdp1 %>% 
  mutate(inter_level = str_extract(inter_var, '(high)|(low)'),
         inter_level = str_to_title(inter_level),
         inter_var2 = str_extract(inter_var, "[[:alpha:]]+(?=_)"),
         # hack b/ var2lab() only accepts pred vars (not 'none')
         inter_var2 = ifelse(inter_var == 'none', 'MAT', inter_var2),
         inter_var2 = var2lab(inter_var2),
         inter_name = paste(inter_level, inter_var2),
         inter_name = ifelse(inter_var == 'none', 'Mean prediction', inter_name)) %>% 
  arrange(inter_var2) %>% 
  mutate(inter_name = factor(inter_name, levels = unique(inter_name)),
         inter_name = fct_relevel(inter_name, 'Mean prediction',
                                  after = n),
         name = lookup_var[variable],
         name = factor(name, levels = unique(name))) %>% 
  select(-inter_var2, -inter_level, -inter_var) %>% 
  rename(inter_var = inter_name)



# for rug plot 
deciles_h <- df_ann1 %>% 
  select(all_of(names(lookup_var_h))) %>% 
  map(quantile, probs = seq(0, 1, 0.1), na.rm = TRUE) %>% 
  map2(., names(.), function(x, name) {
    tibble(variable = name,
           decile = x,
           percentile_name = names(x))
  }) %>% 
  bind_rows() %>% 
  mutate(name = lookup_var_h[variable],
         name = factor(name, levels = unique(name)),
         decile = ifelse(variable == 'MAT', decile - 273.15, decile)) %>% 
  group_by(variable) %>% 
  # don't want to plot the first and last deciles (i.e. min/max values)
  # b/ they stretch the xlims to areas with very little data
  filter(decile != max(decile),
         decile != min(decile))
  

deciles <- deciles_h %>% 
  filter(variable != 'hmod')

letter_df <- tibble(
  letter = fig_letters[1:length(mod_vars)],
  name = factor(lookup_var),
  x_value = -Inf,
  yhat = Inf
)


base_pdp <- function(scales = 'free_x') {
  out <- list(
    facet_wrap(~name, scales = scales, strip.position = "bottom"),
    theme(strip.text = element_markdown(),
          strip.placement = "outside",
          axis.title.x = element_blank(),
          strip.background = element_blank()),
    labs(y = "Annual fire probability (%)"),
    sec_axis_fri(), # adding second axis (fire return interval)
    annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, size = 1)
)
  
  out
}


g <- ggplot(df_pdp2, aes(x_value, yhat*100)) +
  geom_line(aes(color = inter_var, linetype = inter_var, linewidth = inter_var)) +
  geom_rug(data = deciles, aes(x = decile, y = NULL,
                               alpha = percentile_name %in% c('20%', '80%')), sides = 'b') +
  geom_text(data = letter_df, aes(label = letter),
            hjust = -0.8,
            vjust = 1) +
  # if limit_axes is true, then dataset was already filtered,
  # so additional limits should not be set
  base_pdp() +
  scale_color_manual(values = c(rep('#b2182b', 2),
                                rep('#2166ac', 2),
                                rep('#67a9cf', 2),
                                rep('#4daf4a', 2),
                                rep('#ef8a62', 2),
                                'black'),
                     name = 'legend') +
  scale_linetype_manual(values = c(rep(c(1,2), n/2), 1),
                        name = 'legend') +
  #scale_linewidth_continuous(range = c(0.5, 1))+
  guides(alpha = 'none') +
  theme(legend.title = element_blank())+
  # scale_linewidth_manual(values = c(rep(0.5, n), 2),
  #                        name = 'legend') +
  scale_discrete_manual('linewidth', values = c(rep(0.7, n), 1.2),
                        name = 'legend') +
  scale_discrete_manual('alpha', values = c(0.4, 1))
g


if(save_pdp){
  # figure 3
png(paste0("figures/pdp/pdp_high-low_", v, s, ".png"),
    units = "in", res = 600,  width = 8, height = 5)
print(g)
dev.off()
}
  


# pdp for hmod ------------------------------------------------------------
# model that includes human modification

dfs_pdp_h <- map(mod_vars_h, function(var) {
  out <- pdp::partial(hmod, pred.var = var, plot = FALSE,
                      prob = TRUE, train = train1,
                      parallel = FALSE)
  out$inter_var <- 'none'
  out
}) 

dfs_pdp_h$MAT$MAT <- dfs_pdp_h$MAT$MAT - 273.15 # k to c
df_pdp1_h <- map(dfs_pdp_h, function(df) {
  var_name <- names(df)[1]
  
  out <- as_tibble(df)
  names(out) <- c("x_value", "yhat", 'inter_var')
  out$variable <- var_name
  out
}) %>% 
  bind_rows() 


df_pdp2_h <- df_pdp1_h %>% 
  mutate(name = lookup_var_h[variable],
         name = factor(name, levels = unique(name))) %>% 
  left_join(x_lims, by = c("variable" = "name")) %>% 
  filter(x_value >= min | is.na(min), x_value <= max | is.na(max)) %>% 
  select(-min, -max)

letter_df_h <- tibble(
  letter = fig_letters[1:length(mod_vars_h)],
  name = factor(lookup_var_h),
  x_value = -Inf,
  yhat = Inf
)

g <- ggplot(df_pdp2_h, aes(x_value, yhat*100)) +
  geom_line(aes(color = 'Model with human modification',
                linetype = 'Model with human modification')) +
  geom_rug(data = deciles_h, aes(x = decile, y = NULL), sides = 'b') +
  geom_text(data = letter_df_h, aes(label = letter),
            hjust = -0.8,
            vjust = 1) +
  geom_line(data = df_pdp2 %>% filter(inter_var == 'Mean prediction'), 
            aes(color = "Original model",
                                linetype = "Original model")) +
  theme(legend.title = element_blank(),
        legend.position = "top") +
  scale_color_manual(values = c("black", "blue"), name = 'name') +
  scale_linetype_manual(values = c(1, 2), name = 'name')+
  base_pdp()
  
g
if(save_pdp){  
  # figure for appendix
png(paste0("figures/pdp/pdp_pub-qual_v3_hmod", s, ".png"), units = "in", res = 600,
    width = 6, height = 3.5)
print(g)
dev.off()
}


# find maxima -------------------------------------------------------------
# values of the predictor variable for which the probability (from the pdp)
# is hightest

df_pdp2 %>% 
  filter(inter_var == 'Mean prediction') %>% 
  group_by(variable) %>% 
  filter(yhat == max(yhat))

# x_value    yhat variable    inter_var       name                                linewidth
# <dbl>   <dbl> <chr>       <fct>           <fct>                                   <dbl>
# 1  19.5    0.00661 MAT         Mean prediction Temperature (°C)                            1
# 2 487.     0.00661 MAP         Mean prediction Precitation (mm)                            1
# 3   0.0574 0.00845 prcpPropSum Mean prediction PSP (proportion)                            1
# 4 102.     0.0170  afgAGB      Mean prediction Annual biomass (g/m<sup>2</sup>)            1
# 5  46.3    0.00612 pfgAGB      Mean prediction Perennial biomass (g/m<sup>2</sup>)         1

# output rounded coefficients ----------------------------------------------


# useful for copy and pasting into manuscript
coef_df <-  map_dfr(list(main = mod, HMod = hmod), function(x) {
  out <- summary(x) %>% 
    .$coefficients %>% 
    as_tibble() %>% 
    mutate(variable = names(x$coefficients),
           Estimate = map_chr(Estimate, format,scientific = F, digits = 4),
           `Std. Error` = map_chr(`Std. Error`, format,scientific = F, digits = 3),
           `z value` =  map_chr(`z value`, format,scientific = F, digits = 3),
           variable_name = lookup[variable],
           variable_name = string_rename_vars(variable_name))
  out
},
.id = "model")

# table for appendix
write_csv(coef_df, paste0("models/models_coefs_glm", s, ".csv"))

