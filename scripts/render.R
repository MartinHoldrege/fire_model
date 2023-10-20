# Purpose: render rmarkdown files, and changing input params as desired

# Started: October 12, 2023


# dependencies ------------------------------------------------------------

library(magrittr)

# params ------------------------------------------------------------------

date <- lubridate::today() %>% 
  as.character() %>% 
  stringr::str_replace_all("-", "")

knit_root_dir <- getwd() # project directory

# functions ---------------------------------------------------------------

#' render logistic regression model code (fit to annual data)
#'
#' @param s string that defines the model/interactions
#' @param inter interactions to include
#' @param sample_group which sample to fit the model to (at the moment 1-5 should
#' work)
#' @param test_run is this a test run
#' @param save_figs show some figs created in the script be saved externally
render_glm = function(s, inter, sample_group,
                      test_run = FALSE, save_figs = FALSE) {
  rmarkdown::render(
    "scripts/05_models_biome-mask_fire-prob_ann.Rmd",
    knit_root_dir = knit_root_dir,
    params = list(
      test_run = test_run,
      save_figs = save_figs,
      s = s,
      inter = inter,
      sample_group = sample_group
    ),
    # descriptor,  annf2:ann (annual data), f = fractional cover used, 
    # 2 = second version where new transformations tested and annuals transformed based on the main effect transformation
  
    output_file = paste0("05_models_biome-mask_fire-prob_annf2_", date, 
                         stringr::str_replace(s, "_annf2", ""),
                         "_g", sample_group, ".html"),
    output_dir = file.path(knit_root_dir, 'scripts/copies')
  )
}


# render docs ------------------------------------------------------------

# render_glm(s = "_annf2",
#            inter = NULL,
#            sample_group = 1,
#            test_run = FALSE)

render_glm(s = "_annf2_A-P",
           inter = c('afgAGB:MAP' = "afgAGB:MAP"),
           sample_group = 1,
           test_run = FALSE)

# render_glm(s = "_annf2_A-T",
#            inter = c('afgAGB:MAT' = "afgAGB:MAT"),
#            sample_group = 1,
#            test_run = FALSE)
# 
# render_glm(s = "_annf2_A-S",
#            inter = c('afgAGB:prcpPropSum' = "afgAGB:prcpPropSum"),
#            sample_group = 1,
#            test_run = FALSE)
# 
# render_glm(s = "_annf2_A-Pr",
#            inter = c('afgAGB:pfgAGB' = "afgAGB:pfgAGB"),
#            sample_group = 1,
#            test_run = FALSE)

for (sample_group in 2:3) {
  render_glm(s = "_annf2_A-P",
             inter = c('afgAGB:MAP' = "afgAGB:MAP"),
             sample_group = sample_group,
             test_run = FALSE)
}












