# Purpose: Look at time series of mean annuals and perennials in
# pixels that burned, vs never burned, for a few different climate conditions
# (i.e., fold)

# Author: Martin Holdrege
# Started: October 9, 2023


# dependencies ------------------------------------------------------------
library(tidyverse)
theme_set(theme_classic())
# read in data ------------------------------------------------------------

# data pulled together in the 07_bio_time-series.js script
bio1 <- read_csv("data_processed/RAP/mean-ann-biomass_by-fold-burn-status_1986_2019_120m.csv")


# prep data ---------------------------------------------------------------

bio2 <- bio1 %>% 
  select(-.geo, -`system:index`) %>% 
  # first number of group is the fold second is the burn status
  mutate(group = as.character(group),
         fold = as.factor(str_extract(group, '^\\d')),
         burned = as.logical(as.numeric(str_extract(group, '\\d$'))),
         group2 = paste0(fold, " (", c('unburned', 'burned')[burned + 1], ')')) %>% 
  rename(biomass = meanValue,
         PFT = bandName)


# make plots --------------------------------------------------------------
k <- length(levels(bio2$fold))
linetypes <- rep(c(1, 2), k)
cols <- rep(RColorBrewer::brewer.pal(k, 'RdYlBu'), each = 2)
legend_name <- 'Fold & burn status'

g <- ggplot(bio2, aes(year, biomass, color = group2, linetype = group2))   +
  facet_wrap(~PFT, scales = 'free_y', ncol = 1) +
  scale_linetype_manual(values = linetypes, name = legend_name) +
  scale_color_manual(values = cols, name = legend_name) +
  labs(subtitle = 'Mean biomass was calculated annually seperately for pixels that never
       burned and pixels that burned in at least one year in each fold')

pdf('figures/time_series/biomass_time-series_v1.pdf')
g +
  geom_line()

g +
  geom_smooth(method = 'lm', se = FALSE)
dev.off()
  
