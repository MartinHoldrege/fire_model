# Martin Holdrege

# Script started 3/9/2023

# Purpose: create file for science base data publication. 

# not the part of the script that creates the index raster needs to be
# run prior to a lot of the model fitting code 


# dependencies ------------------------------------------------------------


library(terra)
library(tidyverse)


# read in data ------------------------------------------------------------


# the final model, described in the manuscript submitted to fire ecology
mod <- readRDS("models/glm_binomial_models_v3_annf3_A-P_entire.RDS")

# the data frame of predictor and response variables
df_ann1 <- read_csv("data_processed/fire-clim-veg_3yrAvg_v2.csv",
                    show_col_types = FALSE) %>% 
  # removing excess rows for memory saving
  select(-matches('burn_frac'), -matches('nfire_cwf_centroid'),
         -matches('weight'))


# create dataframe for output ----------------------------------------------


df1 <- df_ann1
df1$predicted_prob <- predict(mod, newdata = df1, type = "response")

df2 <- df1 %>%
  select(-cwf_prop) %>%
  mutate(nfire_cwf = as.numeric(nfire_cwf)) %>% 
  rename(nfire = nfire_cwf) %>%
  select(cell_num, nfire, predicted_prob, everything(), -numYrs)


# *summary stats -----------------------------------------------------------
# column summary stats for terry
summaries <- df2 %>%
  pivot_longer(cols = everything(),
               names_to = "column_name") %>%
  group_by(column_name) %>%
  summarize(min = min(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE))

# output data -------------------------------------------------------------

write_csv(df2, "data_processed/data_publication/fire_climate_vegetation.csv")
# 
# 
write_csv(summaries, "data_processed/data_publication/data_pub_col_summaries.csv")
