# Purpose: Fit GLM with specified transformations to the entire
# dataset of annual fire occurrence.
# I rand through the variable transformation selection with the
# 05_models_biome-mask_fire-prop_ann.Rmd code 5 times each time using a
# different random ~1/5 of the data and each time it resulted in the
# same transformations. Here using those transformation and the bigglm
# package to a fit a model with those transformation to the entire 
# dataset (which has ~25 million observations)

# Author: Martin 

# Started October 16, 2023


# dependencies ------------------------------------------------------------

library(readr)
library(biglm)
library(dplyr)

# read in data ------------------------------------------------------------

df_ann <- read_csv("data_processed/fire-clim-veg_3yrAvg_v1.csv",
                   show_col_types = FALSE) 

# confirming the model objects all have the same final formula
if(FALSE) {
  paths <- list.files("models", pattern = 'glm_binomial_models_v1_ann.*',
                      full.names = TRUE)

  y <- lapply(paths, function(x) {
    readRDS(x)$formula
  })
  
  y2 <- y %>% unlist() %>% 
    unname() %>% 
    str_replace_all(' ', '')
  all(y2[[1]] == y2)
}


# model fitting -----------------------------------------------------------
set.seed(1234)
form <- "cwf_prop ~ sqrt(afgAGB) + poly(pfgAGB,2,raw=TRUE) + poly(MAT,2,raw=TRUE) + poly(MAP,2,raw=TRUE) + poly(prcpPropSum,2, raw = TRUE) + (afgAGB:MAP)"

# starting values are rounded coefficients from the same model fit to a sample of 5 million data points
start <- c(`(Intercept)` = -600, `sqrt(afgAGB)` = 0.8, `poly(pfgAGB, 2, raw = TRUE)1` = 0.03, 
           `poly(pfgAGB, 2, raw = TRUE)2` = -2e-04, `poly(MAT, 2, raw = TRUE)1` = 4, 
           `poly(MAT, 2, raw = TRUE)2` = -0.007, `poly(MAP, 2, raw = TRUE)1` = 0.02, 
           `poly(MAP, 2, raw = TRUE)2` = -1e-05, `poly(prcpPropSum, 2, raw = TRUE)1` = -3, 
           `poly(prcpPropSum, 2, raw = TRUE)2` = -6, `afgAGB:MAP` = -1e-04
)

m1 <- glm(as.formula(form), family = binomial(link = 'logit'),
             data = df_ann, 
             #start = start
          )


# save objects ------------------------------------------------------------

mod2save <- butcher::butcher(m1) # removes some model components so the saved object isn't as huge

mod2save$formula <- form

saveRDS(mod2save, "models/glm_binomial_models_v1_ann_A-P_entire.RDS")

