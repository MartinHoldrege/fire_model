# Purpose: Fit GLM with specified transformations to the entire
# dataset of annual fire occurrence.
# I rand through the variable transformation selection with the
# 05_models_biome-mask_fire-prop_ann.Rmd code 5 times each time using a
# different random ~1/5 of the data and each time it resulted in the
# same transformations. Here using those transformations to fit a model to the entire 
# dataset (which has ~25 million observations)

# Author: Martin 

# Started October 16, 2023


# dependencies ------------------------------------------------------------

library(readr)
library(dplyr)

# read in data ------------------------------------------------------------

# burn occurrence determined from fraction burned
df_ann <- read_csv("data_processed/fire-clim-veg_3yrAvg_v2.csv",
                   show_col_types = FALSE) 

# get the formula of the model fit in 05_models_biome-mask_fire-prob_ann.Rmd
# s <- '_annf3_A-P' # The main model
s <- '_hmod_annf3_A-P' # the model that also includes human modification
v <- 3
form <- readRDS(paste0("models/glm_binomial_models_v", v, s, "_5000000n_g1.RDS"))$formula

# examine if models fit two different subsets have the same formula have the same final formula (update as needed)
if(FALSE) {
  paths <- list.files("models", pattern = paste0("glm_binomial_models_v", v, s, "_\\d+.*.RDS"),
                      full.names = TRUE)

  y <- lapply(paths, function(x) {
    readRDS(x)$formula
  })
  
  y2 <- y %>% unlist() %>% 
    unname() %>% 
    stringr::str_replace_all(' ', '')
  
  all(y2[[1]] == y2) # one model fit differently. --going with the 'majority' model, which has a more plausible temperature shape
}


# model fitting -----------------------------------------------------------
set.seed(1234)


m1 <- glm(as.formula(form), family = binomial(link = 'logit'),
             data = df_ann, 
             #start = start
          )


# save objects ------------------------------------------------------------

mod2save <- butcher::butcher(m1) # removes some model components so the saved object isn't as huge

mod2save$formula <- form

saveRDS(mod2save, paste0("models/glm_binomial_models_v", v, s, "_entire.RDS"))

