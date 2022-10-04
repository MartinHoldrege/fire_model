# Martin Holdrege

# Script started 6/1/2022

# Purpose--create better variable importance plots
# than created in 05_models_biome-mask_fire-prob_resample.rmd


# dependencies ------------------------------------------------------------

library(vip)
library(ggplot2)
library(ggtext)
theme_set(theme_classic())

# read in model objects ---------------------------------------------------

mods <- readRDS("models/glm_binomial_models_byNFire_v2_bin20_cwf.RDS")
mod <- mods$paint_cwf

# create vip --------------------------------------------------------------

g <- vip(mod, num_features = 13)
# g$data$Variable %>% 
#   paste(collapse = "' = , '")

lookup <- c('sqrt(afgAGB)' = 'sqrt(afgAGB)', 
            'poly(afgAGB, 2, raw = TRUE)1' = 'afgAGB',
            'poly(afgAGB, 2, raw = TRUE)2' = 'afgAGB^2',
            'afgAGB:MAP' = 'afgAGB:MAP', 
            'poly(MAP, 2, raw = TRUE)1' = 'MAP', 
            'poly(prcpPropSum, 2, raw = TRUE)1' = 'prcpPropSum', 
            'poly(prcpPropSum, 2, raw = TRUE)2' = 'prcpPropSum^2', 
            'poly(MAP, 2, raw = TRUE)2' = 'MAP^2', 
            'poly(pfgAGB, 2, raw = TRUE)1' = 'pfgAGB', 
            'poly(pfgAGB, 2, raw = TRUE)2' = 'pfgAGB^2', 
            'poly(MAT, 2, raw = TRUE)2' = 'MAT^2', 
            'poly(MAT, 2, raw = TRUE)1' = 'MAT', 
            'afgAGB:prcpPropSum' = 'afgAGB:prcpPropSum')

g$data$Variable <- lookup[g$data$Variable]

jpeg('figures/vip_v3_glm_byNFire.jpeg', width = 3, height = 3, units = 'in', 
     res = 600)
g
dev.off()


# create pdp --------------------------------------------------------------

# lookup table
lookup_var <- c(
  "afgAGB" = "afgAGB (g/m<sup>2</sup>)",
  "pfgAGB" = "pfgAGB (g/m<sup>2</sup>)",
  "MAT" = "MAT (°C)",
  "MAP" = "MAP (mm)",
  "prcpPropSum" = "prcpPropSum (proportion)"
)

mod_vars <- names(lookup_var)

dfs_pdp <- map(mod_vars, function(var) {
  pdp::partial(mod, pred.var = var, plot = FALSE,
               prob = TRUE, train = mod$data)
}) 


df_pdp1 <- map(dfs_pdp, function(df) {
  var_name <- names(df)[1]
  
  out <- as_tibble(df)
  names(out) <- c("x_value", "yhat")
  out$variable <- var_name
  out
}) %>% 
  bind_rows() 


df_pdp2 <- df_pdp1 %>% 
  mutate(xlab = lookup_var[variable],
         xlab = factor(xlab, levels = unique(xlab)))

# for rug plot 
deciles <- mod$data %>% 
  select(all_of(names(lookup_var))) %>% 
  map(quantile, probs = seq(0, 1, 0.1)) %>% 
  bind_cols() %>% 
  pivot_longer(cols = everything(), 
               names_to = "variable",
               values_to = "decile") %>% 
  mutate(xlab = lookup_var[variable],
         xlab = factor(xlab, levels = unique(xlab)))

letter_df <- tibble(
  letter = fig_letters[1:length(mod_vars)],
  xlab = factor(lookup_var),
  x_value = -Inf,
  yhat = Inf
)

jpeg("figures/pdp/pdp_pub-qual_v1.jpeg", units = "in", res = 600,
     width = 6, height = 3.5)
ggplot(df_pdp2, aes(x_value, yhat)) +
  geom_line() +
  geom_rug(data = deciles, aes(x = decile, y = NULL), sides = 'b') +
  geom_text(data = letter_df, aes(label = letter),
            hjust = -0.8,
            vjust = 1) +
  facet_wrap(~xlab, scales = 'free', strip.position = "bottom") +
  theme(strip.text = element_markdown(),
        strip.placement = "outside",
        axis.title.x = element_blank(),
        strip.background = element_blank()) +
  labs(y = "Annual fire probability") +
  #scale_y_continuous(expand = c(0.1, 0)) +
  expand_limits(y = 0.025)
dev.off()

