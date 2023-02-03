# Purpose:
# Examine model predictions, to see why some areas have high bias (i.e
# what variables and interactions are causing this)


# Martin Holdrege

# script started 2/2/2023


# params ------------------------------------------------------------------

s <- "_S-T_A-T_A-Pr" # string of specific model

# dependencies ------------------------------------------------------------

source("src/general_functions.R")
library(tidyverse)

# read in data ------------------------------------------------------------

mods1 <- readRDS(paste0("models/glm_binomial_models_byNFire_v2_bin20_cwf", s, 
                        ".RDS"))
print(mods1$formula)
mod1 <- mods1$paint_cwf
summary(mod1)


# model predictions -------------------------------------------------------

# multiply coefficients by each linear predictor, to get the contribution
# to the total predicted value (ie. to see which predictors are causing
# very large predictions)

mod_matrix <- model.matrix(mod1) # all linear predictors

class(mod_matrix)
coefs <- mod1$coefficients
class(coefs)
lin_terms <- t(coefs * t(mod_matrix))

# summarise ----------------------------------------------------------

pred_vars <- c("afgAGB", "pfgAGB", "MAT", "MAP", "prcpPropSum")
response_vars <- c("cwf_prop", "cwf_prop_pred")
df1 <- mod1$data
df1$cwf_prop_pred <- predict(mod1, type = "response")


# categorizing rows of data by the percentile of afg while filtering
# by other variables

filt_afg1 <- bind_cols(df1, lin_terms) %>% 
  filter_by_climate(filter_vars = pred_vars) %>% 
  group_by(filter_var, percentile_category) %>% 
  nest() %>% 
  # empirical cdf
  mutate(cdf = map(data, function(df) ecdf(df$afgAGB)),
         # calculate the percentile of each data point based on the ecdf
         percentile = map2(data, cdf, function(df, f) f(df$afgAGB))) %>% 
  select(-cdf) %>% 
  unnest(cols = c("data", "percentile")) %>% 
  group_by(filter_var, percentile_category) %>% 
  # these 'deciles' are actually percentiles
  mutate(decile_afg = cut(percentile, seq(0, 1, 0.01),
                      labels = 1:100),
         MAT = MAT - 273.15) %>% 
  select(-percentile, -herbAGB)
  

filt_afg_avg1 <- filt_afg1 %>% 
  # convert to %
  mutate(across(matches("_prop"),
                .fns = function(x) x*100)) %>% 
  select(-occur_cwf, -bin_all, -cell_num, -nfire_cwf) %>% 
  pivot_longer(cols = c(all_of(response_vars), all_of(pred_vars),
                        all_of(colnames(lin_terms))),
               names_to = 'predictor_var') %>% 
  group_by(filter_var, percentile_category, decile_afg, predictor_var) %>% 
  summarise(mean = weighted.mean(value, w = numYrs),
            sd = sd(value),
            max = max(value),
            min = min(value))

# just looking at high annuals at top 20th percentile perennials
afg1 <- filt_afg_avg1 %>% 
  filter(filter_var == "pfgAGB",
         percentile_category == ">80th",
           decile_afg %in% (96:100))# predictor_var %in% colnames(lin_terms)) 

# calculating difference in means (of the linear additive terms) between successive
# percentiles
afg_l1 <- split(afg1, afg1$decile_afg, drop = TRUE)

afg_diffs <- accumulate(afg_l1, function(x1, x2) {
  out <- x2
  out$mean_diff <- x2$mean - x1$mean
  out
})
afg_diffs2 <- bind_rows(afg_diffs)
View(afg_diffs2)
