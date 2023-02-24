# purpose:
# compare 

# dependencies ------------------------------------------------------------

source('src/general_functions.R')
library(DALEX)
library(patchwork)
library(tidyverse)

# params ------------------------------------------------------------------

# string vector, part of the name of the model, usually identifying
# the model interactions
sv <-  c(#"", # original model (model 1)
  #"_A2-T2_A-Pr", # model 4
  #"_A-P_A2-T2_A-Pr", # model 4b
  #"_S-T_A2-T2_A-Pr", # model 6
  #"_A-P_S-T_A2-T2_A-Pr"#, # model 6b
  #"_S2-T2_A2-T2_A-Pr", # model 7
  "_7B_A-P_S2-T2_A2-T2_A-Pr" # model 7b
)


# loop through models -----------------------------------------------------
for (s in sv) {
  message(s, " model")
  
  # read in model objects ---------------------------------------------------
  
  # main model
  mods <- readRDS(paste0("models/glm_binomial_models_byNFire_v2_bin20_cwf",
                         s, ".RDS"))
  mod <- mods$paint_cwf
  
  if(s == "") {
    s <- "_original"
  }
  
  # subsample data ----------------------------------------------------------
  lookup_var <- var2lab(x = NULL)
  mod_vars <- names(lookup_var) %>% 
    set_names()
  
  set.seed(123)
  # sampling the data for the profile plots b/ doing it with the complete
  # datset uses too much memory.
  # here breaking the dataset into even bins along a predictor variable
  # and then sampling from each bin
  dfs_samples <- map(mod_vars, subsample_by_var2, df = mod$data,
                     n_breaks = 1000, n_per_group = 10)
  
  # ale ---------------------------------------------------------------------
  # accumulated local effects plots
  # https://ema.drwhy.ai/accumulatedLocalProfiles.html
  
  # explainers
  exps1 <- map2(dfs_samples, names(dfs_samples), function(data, name) {
    DALEX::explain(model = mod,
                   data = data[mod_vars],
                   y = data$cwf_prop,
                   label = name,
                   vebose = FALSE)
  })
  
  #exps1 <- exps1['afgAGB']
  
  # partial dependence profiles by variable
  pd_l1 <- map2(exps1, names(exps1), function(explainer, variable) {
    out <- model_profile(explainer = explainer,
                  type       = "partial",
                  variables  = variable,
                  N = NULL)
    out$agr_profiles$`_label_` = "partial dependence"
    out
  })
  
  ac_l1 <- map2(exps1, names(exps1), function(explainer, variable) {
    out <- model_profile(explainer = explainer,
                  type       = 'accumulated',
                  variables  = variable,
                  N = NULL)
    out$agr_profiles$`_label_` = "accumulated local"
    out
  })
  
  #plot(pd_l1$afgAGB, geom = 'profiles')
  
  # plot(pd_l1$afgAGB, ac_l1$afgAGB) +
  #   labs(title = NULL,
  #        subtitle = NULL)
  plot_l1 <- map2(pd_l1, ac_l1, function(pd, ac) {
    plot(pd, ac) +
      labs(title = NULL,
           subtitle = NULL) +
      expand_limits(y = c(0, 0.02))
  })
  
  # only keeping legend for first panel
  plot_l1[2:length(plot_l1)] <- map(plot_l1[2:length(plot_l1)], function(g) {
    g +
      theme(legend.position = 'none')
  })
  
  
  plot_comb1 <- patchwork::wrap_plots(plot_l1) +
    plot_annotation(
      caption = paste('plots made from a subset of ~', nrow(dfs_samples[[1]]),
                      'rows'),
      title = paste(s, "model")
    )
  
  jpeg(paste0("figures/ale/ale_vs_pdp_v1", s, ".jpeg"),
              units = 'in', res = 600, width = 7, height = 6)
    print(plot_comb1)
  dev.off()
       
}      
