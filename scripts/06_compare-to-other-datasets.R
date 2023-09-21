# Martin Holdrege

# Script Started: March 9, 2019

# Purpose--compare predicted fire probability from our GLM
# to fire probability (or other relative metrics of fire probability) from
# other studies particularly Short et al 2020 (FSim model, US wide dataset) and
# Smith et al 2022 (Random forest model, great basin dataset)
# Short et al 2023 (FSim sagebrush dataset)

# dependencies ------------------------------------------------------------

library(terra)
source("scripts/04_create_biome-mask_dataframe_byNfire.R")
source("src/general_functions.R")

# read in data ------------------------------------------------------------

# * cell numbers ----------------------------------------------------------
# reference raster giving cell numbers that correspond to unique identifiers
# in other data frames

cell_nums <- rast("data_processed/data_publication/cell_nums.tif")


# * model object ----------------------------------------------------------

# model '4b'
mod <- readRDS("models/glm_binomial_models_byNFire_v2_bin20_cwf_A-P_A2-T2_A-Pr.RDS")$paint_cwf


# * dataframe ---------------------------------------------------------------
# dataframe with 1 observation per grid-cell
data1 <- dfs_byNFire3$paint


# * FSim ------------------------------------------------------------------
# modeled burn probability 

# 2nd edition fsim dataset
# downloaded from: https://doi.org/10.2737/RDS-2016-0034-2
fsim1 <- rast('data_raw/fsim_fire_prob_RDS-2016-0034-2/Data/I_FSim_CONUS_LF2014_270m/CONUS_iBP.tif')


# * smith2020 -------------------------------------------------------------

smith1 <- rast("data_processed/fire_probability/smith2022_mean_annual_fire_prob_1988-2019.tif")


# * short2023 -------------------------------------------------------------
# Similar approach to Fsim dataset above (I think) but specifically run for the sagebrush region
# dataset: https://doi.org/10.2737/RDS-2023-0050

short1 <- rast('data_raw/SBRA_BP/SBRA_BP.tif')


# * pastick ---------------------------------------------------------------
# https://doi.org/10.5066/P9ZN7BN8
pastick1 <- rast('data_raw/fire_probability/LT_Wildfire_Prob_85to19_v1-0.tif')

# line up rasters ---------------------------------------------------------

# *fsim -------------------------------------------------------------------

names(fsim1) <- 'fsim' # renaming the layer
fsim2 <- project(fsim1, cell_nums, method = 'bilinear')

# confirm the two rasters now have the same projection etc
compareGeom(fsim2, cell_nums,  crs = TRUE, ext = TRUE,
            rowcol = TRUE)

fsim3 <- fsim2

# masking out grid cells not used in our study
fsim3[is.na(cell_nums)] <- NA

# extracting values into a dataframe
df_fsim <- get_values(lyr = 'fsim', r = fsim3) %>% 
  rename('fsim' = 'value') %>% 
  select(-lyr)


# * smith -----------------------------------------------------------------

smith2 <- project(smith1, cell_nums, method = 'bilinear')

compareGeom(smith2, cell_nums,  crs = TRUE, ext = TRUE,
            rowcol = TRUE)

df_smith <- get_values(lyr = 1, r = smith2) %>% 
  rename('smith' = 'value') %>% 
  select(-lyr)


# * short -----------------------------------------------------------------
short2 <- project(short1, cell_nums, method = 'bilinear')

compareGeom(short2, cell_nums,  crs = TRUE, ext = TRUE,
            rowcol = TRUE)

df_short <- get_values(lyr = 1, r = short2) %>% 
  rename('short' = 'value') %>% 
  select(-lyr)


# * pastick ---------------------------------------------------------------
pastick2 <- project(pastick1, cell_nums, method = 'bilinear')

compareGeom(pastick2, cell_nums,  crs = TRUE, ext = TRUE,
            rowcol = TRUE)

df_pastick <- get_values(lyr = 1, r = pastick2) %>% 
  rename('pastick' = 'value') %>% 
  select(-lyr)

# predicted fire probability ----------------------------------------------

# predicted fire probability (from our model)
pred <- predict_cell_avg(mod, newdata = data1, type = 'response')


# compare to others -------------------------------------------------------

data2 <- pred %>% 
  left_join(df_fsim, by = "cell_num") %>% 
  left_join(df_smith, by = "cell_num") %>% 
  left_join(df_short, by = 'cell_num')%>% 
  left_join(df_pastick, by = 'cell_num')

cor(data2$pred, data2$fsim, use = "complete.obs")
cor(data2$pred, data2$smith, use = "complete.obs")
# 0.74
cor(data2$pred, data2$short, use = "complete.obs")
# 0.64
cor(data2$pred, data2$pastick, use = "complete.obs")
# 0.76
cor(data2$fsim, data2$smith, use = "complete.obs")
cor(data2$fsim, data2$short, use = "complete.obs")

data3 <- data2 %>% 
  drop_na()

# farer comparison to the strength of correlation with 
# fsim (compared to the fsim&smith correlation) b/ 
# datasets here have the ~ same extent
cor(data3$pred, data3$fsim, use = "complete.obs")

range <- range(c(data2$pred, data2$fsim), na.rm = TRUE)*100

xlab <- "Wildfire probability (%, this manuscript)"

set.seed(123)
png("figures/comparison_with_fsim_v1.png",
    width = 4, height = 4, units = "in", res = 600)
data2 %>% 
  sample_n(1*10^5) %>% 
  ggplot(aes(pred*100, fsim*100)) +
  geom_point(alpha = 0.05, size = 0.5) +
  coord_cartesian(xlim = range, ylim = range) +
  geom_smooth(method = 'lm') +
  geom_abline(slope = 1)+
  labs(x = xlab,
       y = "Wildfire probability (%, FSim)")
dev.off()

set.seed(123)
df_sample <- data2 %>% 
  sample_n(1*10^5)
png("figures/comparison_with_smith_v1.png",
    width = 4, height = 4, units = "in", res = 600)

  ggplot(df_sample, aes(pred*100, smith)) +
  geom_point(alpha = 0.05, size = 0.5) +
  labs(x = xlab,
       y = "Relative wildfire probability (Smith et al. 2022)")
dev.off()

png("figures/comparison_with_short2023_v1.png",
    width = 4, height = 4, units = "in", res = 600)

ggplot(df_sample, aes(pred*100, short*100)) +
  geom_point(alpha = 0.05, size = 0.5) +
  labs(x = xlab,
       y = "Burn probability (Short et al. 2023) (converted to %)") +
  geom_smooth(method = 'lm', se = FALSE) +
  geom_abline(slope = 1)
dev.off()

png("figures/comparison_with_pastick_v1.png",
    width = 4, height = 4, units = "in", res = 600)

ggplot(df_sample, aes(pred*100, pastick)) +
  geom_point(alpha = 0.05, size = 0.5) +
  labs(x = xlab,
       y = "Long term wildfire probability (%, Pastick et al. 2021)")
dev.off()
