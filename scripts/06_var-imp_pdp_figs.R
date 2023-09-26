# Martin Holdrege

# Script started 6/1/2022

# Purpose--create better variable importance plots
# than created in 05_models_biome-mask_fire-prob_resample.rmd


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

sv <-  c(#"", # original model (model 1)
         #"_A2-T2_A-Pr", # model 4
         # "_A-P_A2-T2_A-Pr"#, # model 4b (used for first submission two fire ecology)
         "_A-P_A2-T2_A2-S2_A-Pr"#, # model 4c
         #"_S-T_A2-T2_A-Pr", # model 6
         #"_A-P_S-T_A2-T2_A-Pr", # model 6b
         #"_S2-T2_A2-T2_A-Pr", # model 7
         #"_7B_A-P_S2-T2_A2-T2_A-Pr" # model 7b
         )

limit_axes <- FALSE

# looping through models
for (s in sv) {
  message(s, " model")
# read in model objects ---------------------------------------------------

# main model
mods <- readRDS(paste0("models/glm_binomial_models_byNFire_v2_bin20_cwf",
                       s, ".RDS"))
mod <- mods$paint_cwf

# model fit with human modification
mods_h <- readRDS("models/glm_binomial_models_byNFire_hmod_v2_bin20_cwf.RDS")
mod_h <- mods_h$paint_cwf

# create vip --------------------------------------------------------------

if(TRUE){
g <- vip(mod, num_features = 17)
# g$data$Variable %>% 
#   paste(collapse = "' = , '")

lookup <- c('sqrt(afgAGB)' = 'sqrt(afgAGB)', 
            'poly(afgAGB, 2, raw = TRUE)1' = 'afgAGB',
            'poly(afgAGB, 2, raw = TRUE)2' = 'afgAGB^2',
            'poly(MAP, 2, raw = TRUE)1' = 'MAP', 
            'poly(prcpPropSum, 2, raw = TRUE)1' = 'prcpPropSum', 
            'poly(prcpPropSum, 2, raw = TRUE)2' = 'prcpPropSum^2', 
            'poly(MAP, 2, raw = TRUE)2' = 'MAP^2', 
            'poly(pfgAGB, 2, raw = TRUE)1' = 'pfgAGB', 
            'poly(pfgAGB, 2, raw = TRUE)2' = 'pfgAGB^2', 
            'poly(MAT, 2, raw = TRUE)2' = 'MAT^2', 
            'poly(MAT, 2, raw = TRUE)1' = 'MAT', 
            'afgAGB:MAP' = 'afgAGB:MAP', 
            "afgAGB:pfgAGB" = "afgAGB:pfgAGB",
            'afgAGB:prcpPropSum' = 'afgAGB:prcpPropSum',
            'afgAGB:MAT' = 'afgAGB:MAT',
            "I(afgAGB^2):I(MAT^2)" = '(afgAGB:MAT)^2'
            )

g$data$Variable <- lookup[g$data$Variable] %>% 
  # renaming based on updated acronyms used the paper
  str_replace_all("prcpPropSum", "PSP") %>% 
  str_replace_all("afgAGB", "AFG") %>% 
  str_replace_all("pfgAGB", "PFG")

jpeg(paste0('figures/vip_v3_glm_byNFire', s, '.jpeg'), 
            width = 3, height = 3, units = 'in', res = 600)
print(g)
dev.off()
}

# create pdp --------------------------------------------------------------

# lookup table
lookup_var_h <- var2lab(x = NULL, units_md = TRUE, include_hmod = TRUE)
mod_vars_h <- names(lookup_var_h)
names(mod_vars_h) <- mod_vars_h

# w/o hmod included
lookup_var <- lookup_var_h[names(lookup_var_h) != 'hmod']
mod_vars <- mod_vars_h[names(mod_vars_h) != 'hmod']

dfs_pdp <- map(mod_vars, function(var) {
  pdp::partial(mod, pred.var = var, plot = FALSE,
               prob = TRUE, train = mod$data)
}) 


dfs_pdp$MAT$MAT <- dfs_pdp$MAT$MAT - 273.15 # k to c
df_pdp1 <- map(dfs_pdp, function(df) {
  var_name <- names(df)[1]
  
  out <- as_tibble(df)
  names(out) <- c("x_value", "yhat")
  out$variable <- var_name
  out
}) %>% 
  bind_rows() 


df_pdp2 <- df_pdp1 %>% 
  mutate(xlab = lookup_var[variable],
         xlab = factor(xlab, levels = unique(xlab)))

# for rug plot 
deciles <- mod$data %>% 
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

# limiting data to the average of the first and last percentile
# so that these pdp plots show the same range of data as the quantile plots
xlims <- mod$data %>% 
  select(all_of(names(lookup_var))) %>% 
  mutate(MAT = MAT - 273.15) %>% # k to c 
  pivot_longer(cols = everything(),
               names_to = 'var') %>% 
  group_by(var) %>% 
  # calculating percentiles after first fitting an empirical cdf function
  mutate(percentile = ecdf(value)(value), 
         group = case_when(percentile < 0.01 ~ 'min',
                           percentile > 0.99 ~ 'max',
                           TRUE ~ 'middle')) %>% 
  filter(group != "middle") %>% 
  group_by(var, group) %>% 
  summarise(value = mean(value), .groups = 'drop') %>% 
  pivot_wider(id_cols = 'var',
              names_from = 'group',
              values_from = "value")

# reduce x axis limits? 
df_pdp3 <- if(limit_axes) {
  df_pdp2 %>% 
    left_join(xlims, by = c("variable" = "var")) %>% 
    filter(x_value <= max & x_value >= min) %>% 
    select(-max, -min)
} else {
  df_pdp2
}

  
letter_df_h <- tibble(
  letter = fig_letters[1:length(mod_vars_h)],
  xlab = factor(lookup_var_h),
  x_value = -Inf,
  yhat = Inf
)

letter_df <- letter_df_h %>% 
  filter(xlab!= lookup_var_h['hmod'])

base_pdp <- function(limit_x = FALSE) {
  out <- list(
    facet_wrap(~xlab, scales = 'free', strip.position = "bottom"),
    theme(strip.text = element_markdown(),
          strip.placement = "outside",
          axis.title.x = element_blank(),
          strip.background = element_blank()),
      labs(y = "Annual fire probability (%)"),
      sec_axis_fri(), # adding second axis (fire return interval)
      expand_limits(y = c(0, 3)),
      "limit_x" = ggh4x::facetted_pos_scales(
        x = list(xlab == lookup_var['MAP'] ~ scale_x_continuous(limits = c(0, 1000)),
                 xlab == lookup_var['MAT'] ~ scale_x_continuous(limits = c(0, 20)))
  ))
  
  if(!limit_x) {
    out[["limit_x"]] <- NULL
  }
  out
}

# for naming
v <- if(limit_axes) {
  "v2"
} else {
  "v3"
}


png(paste0("figures/pdp/pdp_pub-qual_", v, s, ".png"), 
     units = "in", res = 600,  width = 7, height = 3.5)
g <- ggplot(df_pdp3, aes(x_value, yhat*100)) +
  geom_line() +
  geom_rug(data = deciles, aes(x = decile, y = NULL), sides = 'b') +
  geom_text(data = letter_df, aes(label = letter),
            hjust = -0.8,
            vjust = 1) +
  # if limit_axes is true, then dataset was already filtered,
  # so additional limits should not be set
  base_pdp(limit_x = !limit_axes) 
  
print(g)
dev.off()


# create pdp hmod------------------------------------------------------------

dfs_pdp_h <- map(mod_vars_h, function(var) {
  pdp::partial(mod_h, pred.var = var, plot = FALSE,
               prob = TRUE, train = mod_h$data)
}) 

dfs_pdp_h$MAT$MAT <- dfs_pdp_h$MAT$MAT - 273.15 # k to c
df_pdp1_h <- map(dfs_pdp_h, function(df) {
  var_name <- names(df)[1]
  
  out <- as_tibble(df)
  names(out) <- c("x_value", "yhat")
  out$variable <- var_name
  out
}) %>% 
  bind_rows() 


df_pdp2_h <- df_pdp1_h %>% 
  mutate(xlab = lookup_var_h[variable],
         xlab = factor(xlab, levels = unique(xlab)))

# for rug plot 
deciles_h <- mod_h$data %>% 
  select(all_of(names(lookup_var_h))) %>% 
  map(quantile, probs = seq(0, 1, 0.1)) %>% 
  bind_cols() %>% 
  mutate(MAT = MAT - 273.15 # k to c
  ) %>% 
  pivot_longer(cols = everything(), 
               names_to = "variable",
               values_to = "decile") %>% 
  mutate(xlab = lookup_var_h[variable],
         xlab = factor(xlab, levels = unique(xlab)))

# checking that the hmod and regular mod 
# have the same interactions (i.e. are comparable)
x <- mods_h$pred_vars_inter
y <- mods$pred_vars_inter


# note that some of the older model objects don't include the pred_vars_inter
# list element
# this if statement is included to help insure that figure is only
# created for comparable models (ie only difference is presence of hmod term in)
if(all(x[x!='hmod'] == y)) {
  g <- ggplot(df_pdp2_h, aes(x_value, yhat*100)) +
    geom_line(aes(color = 'Model with human modification',
                  linetype = 'Model with human modification')) +
    geom_rug(data = deciles_h, aes(x = decile, y = NULL), sides = 'b') +
    geom_text(data = letter_df_h, aes(label = letter),
              hjust = -0.8,
              vjust = 1) +
    base_pdp(limit_x = TRUE) +
    geom_line(data = df_pdp2, aes(color = "Original model",
                                  linetype = "Original model")) +
    theme(legend.title = element_blank(),
          legend.position = "top") +
    scale_color_manual(values = c("black", "blue"), name = 'name') +
    scale_linetype_manual(values = c(1, 2), name = 'name')
  
  png("figures/pdp/pdp_pub-qual_hmod_v3.png", units = "in", res = 600,
       width = 6, height = 3.5)
    print(g)

  dev.off()
}



# find maxima -------------------------------------------------------------
# values of the predictor variable for which the probability (from the pdp)
# is hightest

df_pdp2 %>% 
  group_by(variable) %>% 
  filter(yhat == max(yhat))


# output rounded coefficients ----------------------------------------------

# useful for copy and pasting into manuscript
coef_df <-  map_dfr(list(HMod = mod_h, main = mod), function(x) {
    out <- summary(x) %>% 
    .$coefficients %>% 
    as_tibble() %>% 
    mutate(variable = names(x$coefficients),
           Estimate = map_chr(Estimate, format,scientific = F, digits = 4),
           `Std. Error` = map_chr(`Std. Error`, format,scientific = F, digits = 3),
           `z value` =  map_chr(`z value`, format,scientific = F, digits = 3))
    out
  },
  .id = "model")

if(all(x[x!='hmod'] == y)) {
  write_csv(coef_df, "models/models_coefs_glm_byNFire_bin20.csv")
}


}

