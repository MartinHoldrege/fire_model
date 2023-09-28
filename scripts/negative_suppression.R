# Purpose: examine whether negative suppression
# (as described here: https://www.pnas.org/doi/full/10.1073/pnas.1611704113)
# could be occuring in our model, driving an erronious negative relationship
# between annuals and fire in the model, when clearly the relationship
# is positive when just the two variables are compared;

# Author: Martin Holdrege
# Script started: September 28, 2023

# dependencies ------------------------------------------------------------
library(tidyverse)


# functions ---------------------------------------------------------------

test_neg_suppress <- function(y, x1, x2, method = 'pearson') {
  
  # forcing the correlations of x's and y to be positive
  if(cor(y, x1, method = method) < 0) {
    x1b <- -x1
  } else {
    x1b <- x1
  }
  
  if(cor(y, x2, method = method) < 0) {
    x2b <- -x2
  } else {
    x2b <- x2
  }
  
  rYX1 <- cor(y, x1b, method = method)
  rYX2 <- cor(y, x2b, method = method)
  rX1X2 <- cor(x1b, x2b)
  
  # meets the necessary conditions for negative suppression
  rX1X2 > rYX2/rYX1
}


# simulation with fake data (to test function) ----------------------------

# * generate data -----------------------------------------------------------

set.seed(5)

n <- 1000

# correlations between variables
rYX1 <- 0.4 # this variable gets it's sign flipped
rYX2 <- 0.18 # x2 the negative suppressing variable
rX1X2 <- 0.6

# meets the necessary conditions for negative suppression
rX1X2 > rYX2/rYX1

# create the variance covariance matrix
sigma<-rbind(c(1, rYX1, rYX2), c(rYX1 ,1, rX1X2), c(rYX2, rX1X2, 1))
sigma

sim_df <- as.data.frame(MASS::mvrnorm(n = 1000, mu = rep(0, 3), Sigma = sigma))
names(sim_df) <- c("Y", "X1", "X2")                    


# *examine -----------------------------------------------------------------

pairs(sim_df)
cor(sim_df)

# *fit model ---------------------------------------------------------------

sim_mod <- lm(Y~X1 + X2, data = sim_df)

# negative coefficient for X2--statistical artifact!
summary(sim_mod)

test_neg_suppress(y = sim_df$Y, x1 = sim_df$X1, x2 = sim_df$X2)

# read in data ------------------------------------------------------------

mod <- readRDS('models/glm_binomial_models_byNFire_v2_bin20_cwf_A-P_A2-T2_A-Pr.RDS')$paint_cwf

df_resampled <- mod$data # this is the resampled data
df_original <- read_csv('data_processed/data_publication/fire_climate_vegetation.csv')
# prepare data ------------------------------------------------------------

# predicted value on probability scale
df_resampled$p_pred <- predict(mod, type = 'response')
# predicted value on logit (linear) scale
df_resampled$logit_p_pred <- predict(mod, type = 'link')

df_resampled2 <- df_resampled %>%
  # 'observed' probability
  rename(p_obs = cwf_prop) %>% 
  # this logit isn't really worthwile because of so many 0s (i.e. -Inf)
  mutate(logit_p_obs = log(p_obs/(1-p_obs)))

df_original2 <- df_original %>% 
  rename(p_pred = predicted_prob)%>% 
  mutate(p_obs = nfire/numYrs,
         logit_p_pred = log(p_pred/(1-p_pred))
         )

df_l <- list(resampled = df_resampled2,
             original = df_original2)
# test for negative suppression -------------------------------------------
# I'm interested in whether the sign of annuals is flipped. i.e., treating
# annuals as x1. Also using various formulations of y (even model predicted)
# because the test I'm using is developed for OLS and this is a GLM case 
# where the equation probably doesn't directly apply. 

yv <- c("p_obs", "p_pred", "logit_p_pred")
vars <-  c('afgAGB', 'pfgAGB', 'MAT', 'MAP', 'prcpPropSum')
 var_comb <- expand_grid(x1_name = vars,
            y_name = yv,
            x2_name = vars,
            dataset = c('resampled', 'original')) %>% 
  filter(x1_name != x2_name)

# checking whether criteria for negative suppression are being met
var_comb$can_suppress <- pmap_lgl(var_comb, function(x1_name, y_name, x2_name, dataset) {
  df <- df_l[[dataset]]                                  
  test_neg_suppress(y = df[[y_name]],
                    x1 = df[[x1_name]],
                    x2 = df[[x2_name]])
})

comb_summary <- var_comb %>% 
  group_by(x1_name, x2_name, can_suppress, dataset) %>% 
  summarize(can_suppress = mean(can_suppress))


# * fit linear mods -------------------------------------------------------

# model formulas
y <- 'logit_p_pred'
# all predictor vars
forms_full <- paste(y, '~', paste(vars, collapse = ' + '))

# just predictor at a time
forms_univariate <- paste(y, '~', vars)
names(forms_univariate) <- vars

# normalize predictors
df_l_norm <- map(df_l, function(df) {
  df %>% 
    mutate(across(all_of(vars), .fns = \(x) as.numeric(scale(x))))
})

mods_full <- map(df_l_norm, function(df) {
  lm(as.formula(forms_full), data = df)$coefficients[vars]
})

mods_univariate <- map(df_l_norm, function(df) {
  out <- map(forms_univariate, function(form) {
    lm(as.formula(form), data = df)$coefficients[2]
  })
  unlist(out)
})

# slopes from full models
mods_full

# $resampled
# afgAGB      pfgAGB         MAT         MAP prcpPropSum 
# -0.04286425  0.17522308  0.40574253  0.01235771 -0.72371458 
# 
# $original
# afgAGB      pfgAGB         MAT         MAP prcpPropSum 
# 0.1894183   0.3885737   0.3383902   0.1957762  -0.7842959 

# slopes from univariate models, in the resampled dataset multiple slopes
# switch direction. 
mods_univariate

# > mods_univariate
# $resampled
# afgAGB.afgAGB           pfgAGB.pfgAGB                 MAT.MAT                 MAP.MAP prcpPropSum.prcpPropSum 
# 0.38569470             -0.15543142              0.46522900             -0.02638571             -0.75014263 
# 
# $original
# afgAGB.afgAGB           pfgAGB.pfgAGB                 MAT.MAT                 MAP.MAP prcpPropSum.prcpPropSum 
# 0.569576503             0.008978496             0.268258299             0.238279873            -0.733142101 

# note-- in the resampled data afg, pfg and MAP parameters change signs from multivariate to univariate models. 
# afg and pfg also show up as 'can have a negative suppression' effect in the tables above