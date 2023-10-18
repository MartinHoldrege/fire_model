# Purpose:
# fit quantile (and filtered quantile) plots to models. This is most useful
# for model objects that were not created in the 


# dependencies ------------------------------------------------------------

source("src/general_functions.R")
source("src/fig_params.R")
source("src/modeling_functions.R")
library(tidyverse)

# reading in data ---------------------------------------------------------

# burn occurrence determined from fraction burned
df_ann1 <- read_csv("data_processed/fire-clim-veg_3yrAvg_v2.csv",
                   show_col_types = FALSE) 

set.seed(123)
df_test <- df_ann1 %>% 
  slice_sample(n = 3e6)

sv <- c("_ann_sA-P_entire", "_ann_lA-P_entire")
v <- 2

for(s in sv) {
  print(s)
mod <- readRDS(paste0("models/glm_binomial_models_v", v, s, ".RDS"))
if (v ==2) {
  s <- stringr::str_replace(s, 'ann', 'annf')
}


# params ------------------------------------------------------------------


pred_vars <- c("afgAGB", "pfgAGB", "MAT", "MAP", "prcpPropSum")
var_prop <- c('cwf_prop')


# model predictions -------------------------------------------------------

# create prediction for each each model
# (i.e. for each fire proporation variable)
predict_by_response <- function(mod, df) {
  df_out <- df
  
  response_name <- paste0(var_prop, "_pred")
  df_out[[response_name]] <- predict(mod, df, type = 'response')
  df_out
}

pred_glm1 <- predict_by_response(mod, df_test)

### Deciles

# Binning predictor variables into deciles (actually percentiles) and looking at the mean
# predicted probability for each percentile. The use of the word deciles
# is just a legacy thing (they started out being actual deciles)
# 
# Then predicting on an identical dataset but with warming

var_prop_pred <- paste0(var_prop, "_pred")
response_vars <- c(var_prop, var_prop_pred)

pred_glm1_deciles <- predvars2deciles(pred_glm1,
                                      response_vars = response_vars,
                                      pred_vars = pred_vars)


# Publication quality quantile plot

# publication quality version
g <- decile_dotplot_pq(pred_glm1_deciles)

# obs/pred inset
g2 <- add_dotplot_inset(g, pred_glm1_deciles)




png(paste0("figures/quantile_plots/quantile_plot_v5", s,  ".png"), 
    units = "in", res = 600, width = 5.5, height = 3.5 )
print(g2)
dev.off()



### Deciles Filtered 


df <- pred_glm1[, c("MAT", "MAP", "prcpPropSum")] %>% 
  mutate(MAT = MAT - 273.15) # k to c
map(df, quantile, probs = c(0.2, 0.8), na.rm = TRUE)

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

pdf(paste0("figures/quantile_plots/quantile_plot_filtered_v3", s, '.pdf'),
    height = 10, width = 5)

print(decile_dotplot_filtered_pq(pred_glm1_deciles_filt, xvars = clim_vars)) 
print(decile_dotplot_filtered_pq(pred_glm1_deciles_filt))

dev.off()

}