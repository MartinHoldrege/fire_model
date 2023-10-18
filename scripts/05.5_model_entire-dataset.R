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

# burn occurrence determined from fraction burned
df_ann <- read_csv("data_processed/fire-clim-veg_3yrAvg_v2.csv",
                   show_col_types = FALSE) 

# confirming the model objects all have the same final formula (update as needed)
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
# hard coding this model (for now)
form <- "cwf_prop ~ sqrt(afgAGB) + stats::poly(pfgAGB,2,raw=TRUE) + stats::poly(MAT,2,raw=TRUE) + stats::poly(MAP,2,raw=TRUE) + stats::poly(prcpPropSum,2, raw = TRUE) + (sqrt(afgAGB):MAP)"


m1 <- glm(as.formula(form), family = binomial(link = 'logit'),
             data = df_ann, 
             #start = start
          )


# save objects ------------------------------------------------------------

mod2save <- butcher::butcher(m1) # removes some model components so the saved object isn't as huge

mod2save$formula <- form

saveRDS(mod2save, "models/glm_binomial_models_v2_ann_sA-P_entire.RDS")

