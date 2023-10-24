# Purpose:
# fit quantile (and filtered quantile) plots to models. This is most useful
# for model objects that were not created in the 


# dependencies ------------------------------------------------------------

source("src/general_functions.R")
source("src/fig_params.R")
source("src/modeling_functions.R")
library(tidyverse)
theme_set(theme_classic())
# reading in data ---------------------------------------------------------

# burn occurrence determined from fraction burned
df_ann1 <- read_csv("data_processed/fire-clim-veg_3yrAvg_v2.csv",
                   show_col_types = FALSE) 

set.seed(123)
df_test <- df_ann1 %>% 
  slice_sample(n = 1e7)

sv <- c("_annf3_A-P_entire")

for(s in sv) {
  print(s)
mod <- readRDS(paste0("models/glm_binomial_models_v3", s, ".RDS"))

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

df_l1 <- pred_glm1_deciles_filt %>% 
  dplyr::filter(name %in% c("afgAGB", "pfgAGB")) %>% 
  split(., .$name, drop = TRUE)

# list of lists, 
df_l2 <- map(df_l1, function(df) split(df, df$filter_var))

# one list element for veg var and filter var
df_l3 <- flatten_rename(df_l2)

inset_l1 <- map(df_l3, create_inset_filt)

l <- 0.01 # left
b <- c(0.18, 0.515, 0.85) + 0.02 #bottom
w <- 0.36 # width
h <- 0.11 #height

# letters of insets correspond to the panels they belong in
A <- inset_element(inset_l1$afgAGB_MAT, l, b[3], l + w, b[3] + h)
B <- inset_element(inset_l1$pfgAGB_MAT, l, b[3], l + w, b[3] + h)
C <- inset_element(inset_l1$afgAGB_MAP, l, b[2], l + w, b[2] + h)
D <- inset_element(inset_l1$pfgAGB_MAP, l, b[2], l + w, b[2] + h)
E <- inset_element(inset_l1$afgAGB_prcpPropSum, l, b[1], l + w, b[1] + h)
f <- inset_element(inset_l1$pfgAGB_prcpPropSum, l, b[1], l + w, b[1] + h)

g2 <- pred_glm1_deciles_filt %>% 
  filter(filter_var %in% clim_vars) %>% 
  decile_dotplot_filtered_pq2(insets_left = list(A, C, E),
                              insets_right = list(B, D, f),
                              ylim = c(0, 4))
g2
# add insets

png(paste0("figures/quantile_plots/quantile_plot_filtered_insets_v3",
           s, ".png"), units = "in", res = 600, width = 8, height = 8)
print(g2)
dev.off()

}