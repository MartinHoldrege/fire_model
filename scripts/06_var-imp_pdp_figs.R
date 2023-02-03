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

# string defining model name
s <- "_S-T_A-T_A-Pr"
# s <- "" # original model string

# read in model objects ---------------------------------------------------

# main model
mods <- readRDS(paste0("models/glm_binomial_models_byNFire_v2_bin20_cwf",
                       s, ".RDS"))
mod <- mods$paint_cwf

# model fit with human modification
mods_h <- readRDS("models/glm_binomial_models_byNFire_hmod_v1_bin20_cwf.RDS")
mod_h <- mods_h$paint_cwf

# create vip --------------------------------------------------------------

g <- vip(mod, num_features = 13)
# g$data$Variable %>% 
#   paste(collapse = "' = , '")

lookup <- c('sqrt(afgAGB)' = 'sqrt(afgAGB)', 
            'poly(afgAGB, 2, raw = TRUE)1' = 'afgAGB',
            'poly(afgAGB, 2, raw = TRUE)2' = 'afgAGB^2',
            'afgAGB:MAP' = 'afgAGB:MAP', 
            'poly(MAP, 2, raw = TRUE)1' = 'MAP', 
            'poly(prcpPropSum, 2, raw = TRUE)1' = 'prcpPropSum', 
            'poly(prcpPropSum, 2, raw = TRUE)2' = 'prcpPropSum^2', 
            'poly(MAP, 2, raw = TRUE)2' = 'MAP^2', 
            'poly(pfgAGB, 2, raw = TRUE)1' = 'pfgAGB', 
            'poly(pfgAGB, 2, raw = TRUE)2' = 'pfgAGB^2', 
            'poly(MAT, 2, raw = TRUE)2' = 'MAT^2', 
            'poly(MAT, 2, raw = TRUE)1' = 'MAT', 
            'afgAGB:prcpPropSum' = 'afgAGB:prcpPropSum')

g$data$Variable <- lookup[g$data$Variable]

jpeg('figures/vip_v3_glm_byNFire.jpeg', width = 3, height = 3, units = 'in', 
     res = 600)
g
dev.off()


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
         xlab = factor(xlab, levels = unique(xlab)))

letter_df_h <- tibble(
  letter = fig_letters[1:length(mod_vars_h)],
  xlab = factor(lookup_var_h),
  x_value = -Inf,
  yhat = Inf
)

letter_df <- letter_df_h %>% 
  filter(xlab!= lookup_var_h['hmod'])

base_pdp <- function() {
  list(
    facet_wrap(~xlab, scales = 'free', strip.position = "bottom"),
    theme(strip.text = element_markdown(),
          strip.placement = "outside",
          axis.title.x = element_blank(),
          strip.background = element_blank()),
      labs(y = "Annual fire probability (%)"),
      #scale_y_continuous(expand = c(0.1, 0)) +
      expand_limits(y = 2.2),
      ggh4x::facetted_pos_scales(
        x = list(xlab == lookup_var['MAP'] ~ scale_x_continuous(limits = c(0, 1000)),
                 xlab == lookup_var['MAT'] ~ scale_x_continuous(limits = c(0, 20)))
      )
  )
}

jpeg(paste0("figures/pdp/pdp_pub-qual_v1", s, ".jpeg"), 
     units = "in", res = 600,  width = 6, height = 3.5)
ggplot(df_pdp2, aes(x_value, yhat*100)) +
  geom_line() +
  geom_rug(data = deciles, aes(x = decile, y = NULL), sides = 'b') +
  geom_text(data = letter_df, aes(label = letter),
            hjust = -0.8,
            vjust = 1) +
  base_pdp()
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



jpeg("figures/pdp/pdp_pub-qual_hmod_v1.jpeg", units = "in", res = 600,
     width = 6, height = 3.5)
ggplot(df_pdp2_h, aes(x_value, yhat*100)) +
  geom_line(aes(color = 'Model with human modification',
                linetype = 'Model with human modification')) +
  geom_rug(data = deciles_h, aes(x = decile, y = NULL), sides = 'b') +
  geom_text(data = letter_df_h, aes(label = letter),
            hjust = -0.8,
            vjust = 1) +
  base_pdp() +
  geom_line(data = df_pdp2, aes(color = "Original model",
                                linetype = "Original model")) +
  theme(legend.title = element_blank(),
        legend.position = "top") +
  scale_color_manual(values = c("black", "blue"), name = 'name') +
  scale_linetype_manual(values = c(1, 2), name = 'name')
dev.off()


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


write_csv(coef_df, "models/models_coefs_glm_byNFire_bin20.csv")

