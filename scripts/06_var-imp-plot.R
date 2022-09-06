# Martin Holdrege

# Script started 6/1/2022

# Purpose--create better variable importance plots
# than created in 05_models_biome-mask_fire-prob_resample.rmd


# dependencies ------------------------------------------------------------

library(vip)
library(ggplot2)
theme_set(theme_classic())

# read in model objects ---------------------------------------------------

mods <- readRDS("models/glm_binomial_models_byNFire_v2_bin20_cwf.RDS")
mod <- mods$paint_cwf

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

