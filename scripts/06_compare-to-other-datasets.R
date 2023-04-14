# Martin Holdrege

# Script Started: March 9, 2019

# Purpose--compare predicted fire probability from our GLM
# to fire probability (or other relative metrics of fire probability) from
# other studies particularly Short et al 2020 (FSim model, US wide dataset) and
# Smith et al 2022 (Random forest model, great basin dataset)


# dependencies ------------------------------------------------------------

library(terra)
source("scripts/04_create_biome-mask_dataframe_byNfire.R")
source("src/general_functions.R")

# read in data ------------------------------------------------------------

# * cell numbers ----------------------------------------------------------
# reference raster giving cell numbers that correspond to unique identifiers
# in other data frames

cell_nums <- rast("data_processed/cell_nums.tif")


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
plot(smith2)

compareGeom(smith2, cell_nums,  crs = TRUE, ext = TRUE,
            rowcol = TRUE)

df_smith <- get_values(lyr = 1, r = smith2) %>% 
  rename('smith' = 'value') %>% 
  select(-lyr)

# predicted fire probability ----------------------------------------------

# predicted fire probability (from our model)
pred <- predict_cell_avg(mod, newdata = data1, type = 'response')


# compare to others -------------------------------------------------------


data2 <- pred %>% 
  left_join(df_fsim, by = "cell_num") %>% 
  left_join(df_smith, by = "cell_num")

cor(data2$pred, data2$fsim, use = "complete.obs")
cor(data2$pred, data2$smith, use = "complete.obs")
# 0.74

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

png("figures/comparison_with_smith_v1.png",
    width = 4, height = 4, units = "in", res = 600)
data2 %>% 
  sample_n(1*10^5) %>% 
  ggplot(aes(pred*100, smith)) +
  geom_point(alpha = 0.05, size = 0.5) +
  labs(x = xlab,
       y = "Relative wildfire probability (Smith et al. 2022)")
dev.off()

