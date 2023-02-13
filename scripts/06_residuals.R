# Purpose:
# Examine model predictions, and calculate 'residuals' (of average fire probabilities)
# the goal is to understand if these pseudo residuals looks ok for predictions
# along the range of annuals


# Martin Holdrege

# script started 2/13/2023


# params ------------------------------------------------------------------

# string vector, part of the name of the model, usually identifying
# the model interactions
sv <-  c("", "_S-T_A-T", "_A-T_A-Pr", "_A2-T2_A-Pr", "_S-T_A-T_A-Pr",
         "_S-T_A2-T2_A-Pr", "_S2-T2_A2-T2_A-Pr", "_S2-T2_A2-T2_A2-Pr2")

# dependencies ------------------------------------------------------------

source("src/general_functions.R")
library(tidyverse)
theme_set(theme_classic())

# read in data ------------------------------------------------------------
# model objects created in 
# scripts/05_models_biome-mask_fire-prob_byNFire.R

files_mod <- paste0("models/glm_binomial_models_byNFire_v2_bin20_cwf", 
                    sv, ".RDS")

sv[sv == ""] <- "original"
names(files_mod) <- sv

mods1 <- map(files_mod, readRDS)

mods1 <- map


# predict -----------------------------------------------------------------

dfs1 <- map(mods1, function(x) {
  mod <- x$paint_cwf
  data <- mod$data
  data$cwf_prop_pred <- predict(mod, type = "response")*100
  data$cwf_prop <- data$cwf_prop*100 # convert to percent
  data$MAT <- data$MAT - 273.15 # k to c
  data
})

# summarise ----------------------------------------------------------

# calculating mean of predicted for each percentile of afgAGB
pred_vars <- c("afgAGB", "pfgAGB", "MAT", "MAP", "prcpPropSum")
response_vars <- c("cwf_prop", "cwf_prop_pred")

quant1 <- map_dfr(dfs1, predvars2deciles, 
                  response_vars = response_vars, pred_vars = pred_vars,
                  .id = 'model') %>% 
  # predvars2deciles addes herbAGB variable by default (which not interested
  # in here)
  filter(name %in% pred_vars)

quant2 <- quant1 %>% 
  mutate(residual = cwf_prop - cwf_prop_pred,
         model = factor(model, levels = sv))
  

# figures -----------------------------------------------------------------

# residuals vs afgAGB

residual_plot <- function(df, xlab) {
  ggplot(df, aes(mean_value, residual)) +
    geom_point() +
    geom_abline(slope = 0, intercept = 0) + 
    geom_smooth(se = FALSE) +
    facet_wrap(~model) +
    labs(x = xlab,
         y = "Observed - Predicted (%)",
         subtitle = "Residuals by model",
         caption = "Points based on mean observed and predicted values
         for each percentile of the predictor variable")
}


quant3 <- split(quant2, quant2$name, drop = TRUE)

plots <- map2(quant3, var2lab(names(quant3)), residual_plot)

pdf("figures/residuals/residuals_v1.pdf")
plots
dev.off()