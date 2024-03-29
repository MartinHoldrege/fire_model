# Martin Holdrege

# script started: April 1, 2022

# Purpose--various functions useful for modelling


#' create partial dependence plots for generic models
#'
#' @param mod model
#' @param mod_vars names of variables to create pdp plots for
#' @param ylab string, ylab of plot
#' @param ... passed to pdp::partial
#'
#' @return partial dependence plots
pdp_all_vars <- function(mod, mod_vars = NULL, ylab = NULL, ...) {
  if (is.null(mod_vars)) {
    mod_vars <- attributes(terms(mod))[["term.labels"]] %>% 
      # not including interactions or squared terms
      # e.g. I(x^2)
      str_subset(":|(^I\\()", negate = TRUE)
  }
  
  plots <- map(mod_vars, function(var) {
    pdp::partial(mod, pred.var = var, plot = TRUE,
                 prob = TRUE,
                 plot.engine = 'ggplot2', rug = TRUE, ...) +
      ylab(ylab)
  })
  
  # giving layout matrix so that it fills by row, not column
  gridExtra::marrangeGrob(plots, nrow = 2, ncol = 2,
                          layout_matrix = matrix(seq_len(4), nrow = 2, ncol = 2,
                                 byrow = TRUE))
}

#' partial dependence plots for a list of RF models
#'
#' @param mod_list list of random forest models
#' @param df_train training dataframe used to fit those models
#'
#' @return partial dependence plots, for each model and variable, variables
#' are ordered by their importance
pdp_all_rf_mods <- function(mod_list, df_train) {
  # 10th, 20th, ...90th percentiles of each predictor variable
  # for rug plot
  pred_percentiles <- map(df_train, quantile, 
                          probs = seq(0.1, 0.9, by = 0.1))
  
  # variable importance (for ordering plots)
  rf_import <- map(mod_list, function(x) {
    importance(x) %>% 
      as_tibble(rownames = 'var') 
  })
  # looping over models
  for (j in seq_along(mod_list)) {
    name <- names(mod_list[j])

    # this code isn't set up for regression trees at the moment
    stopifnot(mod_list[[j]]$type == "classification")
    
    # order by variable importance
    vars <- rf_import[[name]] %>% 
      arrange(desc(MeanDecreaseGini)) %>% 
      pull(var)
    
    # looping over variables in a model
    par(mfrow = c(2, 3), oma = c(0, 0, 2, 0))
    for (i in seq_along(vars)) {
      out <- myPartialPlot(mod_list[[j]], pred.data = df_train,
                         x.var = vars[i], plot = FALSE)
      
      # get probability from logit
      # convert to probability of fire in a given year
      out$y <- calc_yearly_prob(boot::inv.logit(out$y) , n = 36)
      
      plot(out, xlab = vars[i], main = NULL, ylab = "Probability (0-1)",
           type = 'l')
      rug(pred_percentiles[[vars[i]]])
    }
    mtext(name, outer = TRUE, cex = 1)
  }
}


#' create dateframe of predicted (pdp) values
#'
#' @param mod model object
#' @param mod_vars character vector--names of variable in model
#' @param data data to use for the pdp (providing all rows of the original dataset will be slow)
#' that want to get average prediction fors
#'
#' @return dataframe with value (of x variable), yhat (predicted value)
#' and pred_var which is the name of the predictor (i.e. values from mod_var)
create_pdp_df <- function(mod, mod_vars, data) {
  df_pdp <- map_dfr(mod_vars, function(var) {
    
    out <- pdp::partial(mod, pred.var = var, plot = FALSE,
                        prob = TRUE, 
                        train = data # limited data for training
    )
    names(out) <- c("value", "yhat")
    out$pred_var <- var
    if(var == "MAT") {
      out$value <- out$value - 273.15 # convert from K to C
    }
    
    as_tibble(out)
  }) 
  
  df_pdp
}

#' Create model predictions for given variable
#' 
#' @description This is just a computation quick and dirty way to 
#' approximate the predictions used for making partial dependence plots
#'
#' @param mod model object
#' @param pred_vars predictor variable names
#' @param df dataframe to base predictions on (see additional info below)
#' @param mult multiplier to extend the range of the variable of interest by
#'
#' @return  list with same length as pred_vars. Creats predictions
#' for each pred_var, where all other pred_vars are held constant (at their means)
predict_across_avg <- function(mod, pred_vars, df, mult = 0) {
  
  dfs_avg <- map_dfr(pred_vars, function(var) {
    df <- df %>% 
      mutate(across(-all_of(var),
                    mean))
    n <- 1000
    min <- min(df[[var]])
    max <- max(df[[var]])
    range <- max - min
    min_new <- min - range*mult
    if(min_new < 0) min_new <- 0
    max_new <- max + range*mult
    df <- df[1:n, ]
    
    df[[var]] <- seq(min_new, max_new, length.out = n)
    df$yhat <- predict(mod, newdata = df,
                       type = 'response')
    
    out <- df
    out$pred_var <- var
    out$value <- df[[var]]
    out <- out[, c('yhat', 'pred_var', 'value')]
    out
  })
  
  dfs_avg
}


# find best variable transformations --------------------------------------

transform_funs <- list()

add_constant_string <- function(x) {
  # adding only a small ammount to prcpPropSum so that wouldn't run into
  # problems if got a 0. But for annuals (and other variables) want all the log transformations
  # to be positive (and a less sharp bend) so adding 1 instead. 
  out <- ifelse('prcpPropSum' == x, paste0(x,  "+0.001"), paste0(x, "+1"))
  paste0('I(', out, ')')
}

transform_funs$convert_sqrt <- function(x) {
  p <- paste0("sqrt(", x, ")")
  list(main = p, # transformation for main term
       inter = p) # transformation for term if it is in an interaction
}

# transform_funs$convert_sq <- function(x) paste0("I(", x, "^2)")

# adding x^2 term to the model (in addition to x) i.e. to allow for parabola
#transform_funs$add_sq <- function(x) paste0(x, "+ I(", x, "^2)")

# adding small constant so not taking log of 0 (this is a problem
# for about 300k afg observations)
transform_funs$convert_log10 <- function(x) {
  x <- add_constant_string(x)
  p <- paste0("log10(", x, ")")
  list(main = p,
       inter = p) 
}

# transform_funs$convert_exp <- function(x) paste0("exp(", x, ")") # removing b/ selected

transform_funs$convert_poly2 <- function(x) {
  p <- paste0("stats::poly(", x, ",2, raw = TRUE)")
  list(main = p,
       inter = x) # not changing the interaction term
}

transform_funs$convert_poly2sqrt <- function(x) {
  p <- paste0("stats::poly(I(sqrt(", x, ")),2, raw = TRUE)")
  list(main = p,
       inter = paste0("sqrt(", x, ")")) 
}

transform_funs$convert_poly2log10 <- function(x) {
  x <- add_constant_string(x)
  p <- paste0("stats::poly(I(log10(", x, ")),2, raw = TRUE)")
  list(main = p,
       inter = paste0("log10(", x, ")")) 
}


# # spline with two degrees of freedom (1 would linear)
# transform_funs$convert_spline2 <- function(x) {
#   # requires splines package to be loaded to run these formulas inside glm()
#   # see ?ns() for information on df 
#   paste0("bs(", x, ", df", 2, ")")
# } 
# 
# # 3 dfs
# transform_funs$convert_spline3 <- function(x) {
#   paste0("bs(", x, ", df=", 3, ")")
# } 

# transform_funs$convert_spline4 <- function(x) {
#   paste0("ns(", x, ", df=", 4, ")")
# } 

#' Create right half of model formula with one additional element transformed
#'
#' @param preds vector of predictor variables
#'
#' @return vector where each element returned is a string, that can be used as 
#' the right hand side of a model formula. One additional element is transformed
#' @example 
#' preds <- c('afgAGB', 'prcpPropSum', 'afgAGB:prcpPropSum')
#' names(preds) <- preds
#' transform_preds(preds) # the interaction term is also transformed, depending on the function
#' transform_preds(c(a = 'a', b = 'b', `a:b` = '(a:b)'))
#' transform_preds(c(a = 'sqrt(a)', b = 'b', `a:b`= 'sqrt(a):b'))
transform_preds <- function(preds) {
  stopifnot(
    is.character(preds),
    !is.null(names(preds)), # needs to be a named vector
    is.list(transform_funs) # list of functions created above
  )
  out <- map(transform_funs, function(f) {
    stopifnot(
      is.function(f)
    )
    out <- list()
    # main effects (i.e. not interactions)
    preds_main <- str_subset(preds, pattern = "[:]", negate = TRUE)
    for (var in preds_main) {
      tmp_vars <- preds
      
      # transform main effects
      main_to_transform <- var == preds & str_detect(preds, "\\(|\\:", negate = TRUE)
      tmp_vars[main_to_transform] <- f(tmp_vars[main_to_transform])$main
      
      # transform interactions
      inter_to_transform <- str_detect(preds, paste0("(", var, ":)|(:", var, ")")) & # interaction term
        # not already transformed?
        # if it isn't allowed to be transformed then the variable will have a (
        # before it or a ')' after it
        str_detect(preds, paste0('(\\(',var, ")|(", var, "\\))"), negate = TRUE)
      
      tmp_vars[inter_to_transform] <- str_replace(tmp_vars[inter_to_transform],
                                                  var,
                                                  f(var)$inter)
      
      out[[var]] <- paste(tmp_vars, collapse = " + ") %>% 
        paste("~", .)
    }
    out
  }) 
  # if there are already transformed variables in preds
  # those will be repeated multiple times in the output (so removing)
  out <- flatten_rename(out)
  # by putting convert_none first, it is kept, no matter one
  # even if later elements are duplicates 
  out <- c(convert_none = paste(preds, collapse = " + ") %>% 
             paste("~", .),
           out)
  out <- out[!duplicated(out)]
  out
}

#' Fit a number of binomial glms
#'
#' @param forms character vector where each element can be parsed to a 
#' model formula
#' @param df dataframe to fit the model on
#'
#' @return list of models, one for each formula
fit_bin_glms <- function(forms, df, weights_col = NULL) {
  stopifnot(weights_col %in% names(df))
  
  glm_list <- map(forms, function(form) {
    char_form <- form
    form <- as.formula(form)
    # some of these won't fit so returns NA if throws error
    # not using purrr::safely() didn't seem to work, maybe b/ 
    # of environment issues?

    if(is.null(weights_col)) {
      out <- tryCatch(glm(formula = form, data = df, 
                          family = 'binomial'),
                      error = function(e) NA)
    } else if(weights_col == 'numYrs') {
      out <- tryCatch(glm(formula = form, data = df, 
                          family = 'binomial', 
                          weights = numYrs),
                      error = function(e) NA)
      } else {
      stop('This function not set upt to use that weights_col')
    }

    if (any(is.na(out))) message(paste(char_form,  "model couldn't fit \n"))
    butcher::butcher(out) # to save memory
  })
  # removing models that couldn't be fit b/ they through an error
  out <- keep(glm_list, function(x) all(!is.na(x)))
  out
}

#' Fit a number of binomial gnms
#'
#' @param forms character vector where each element can be parsed to a 
#' model formula, this function is specifically designed so that
#' the first element of the formulat is afgNonLin(afgAGB),
#' @param df dataframe to fit the model on
#'
#' @return list of models, one for each formula
fit_bin_gnms <- function(forms, df) {
  stopifnot('mtbs_n' %in% names(df))
  
  afgNonLin <- function(afgAGB){
    list(predictors = list(beta1 = 1),
         variables = list(substitute(afgAGB)),
         term = function(predLabels, varLabels) {
           sprintf("-1*exp(%s*%s)",
                   predLabels[1], varLabels[1])
         })
  }
  class(afgNonLin) <- "nonlin"
  
  
  glm_list <- map(forms, function(form) {
    char_form <- form
    
    # number of variables after the first 
    # + normally seperates variables (or if interaction, then *)
    num_other_vars <- str_count(char_form, '\\+|\\*') +
      # plus add in every 2nd order polynomial (which adds a term)
      # note this wouldn't work if poly(x, 3), were used in the formula
      str_count(char_form, 'poly\\(')
    
    # wan't to provide a starting value for the non linear term
    # which should be the first variable (after the intercept)
    start <- c(NA, -0.2, rep(NA, num_other_vars))
    
    form <- as.formula(form)
    # some of these won't fit so returns NA if throws error
    # not using purrr::safely() didn't seem to work, maybe b/ 
    # of environment issues?
    out <- tryCatch(gnm(formula = form, data = df, 
                        family = 'binomial', 
                        weights = mtbs_n,
                        start = start),
                    error = function(e) NA)
    if (any(is.na(out))) message(paste(char_form,  "model couldn't fit \n"))
    out
  })
  # removing models that couldn't be fit b/ they through an error
  out <- keep(glm_list, function(x) all(!is.na(x)))
  out
}



#' fit glms with varying number of variables transformed
#' 
#' @description In the first step this
#'  is an algorithm that given a number of predictor
#' variables fits glms where each predictor variable (on its own)  is
#' transformed using each of the function in the transform_funs list 
#' (defined in modeling_functions.R), it returns all models, AIC etc
#' for this first step. If the best model is one where a variable was
#' transformed then it moves on to step 2. Where it tries transforming an 
#' an additional variable, so at the end of step 2 you have models with 2 
#' transformed variables, this goes on until all variables are transformed
#' or transformations no longer help
#'
#' @param preds vector of names of predictor variables
#' @param df data frame used for model fitting
#' @param response_var name of the predictor variable
#' @param max_steps this
#' the max number of variables that could be transformed in the final step. Note
#' that this could be more than the number of original variables if
#' @param delta_aic how many aic units better the model with an extra transformed
#' model needs to be to consider it better
#' @param fit_mod the function that will do the model fitting
#' @param ... args passed to fit mod
#'
#' @return list, containing a sub list for each step, which in turn 
#' contains 'glm' element of all the glm objects, 'which is the model aic's'
#' and 'best_mod' which is the best model for that step
#' Also contains an element 'final_formula; which is the formula of the 
#' best model. 
glms_iterate_transforms <- function(preds, df, response_var,
                                    max_steps = NULL, delta_aic = 4,
                                    fit_mod = fit_bin_glms, 
                                    ...) {
  stopifnot(
    is.data.frame(df),
    is.character(preds),
    is.character(response_var)
  )
  # max steps just makes sure the while loop doesn't explode
  # (run for ever) if something goes wrong
  if (is.null(max_steps)) {
    max_steps <- 30
  }
  if (is.na(max_steps) | max_steps > 30) {
    stop("two many iterations will be required consider shorter preds vector")
  }
  
  out <- list()
  i = 1 # this is the step number
  
  # iterating through number of total variables transformed in the model
  steps_left <- length(preds)
  while (i <= max_steps & steps_left > 0) {

    step_name <- paste0('step', i)
    print(step_name)
    out[[step_name]] <- list() # list of output for this step
    
    # model formulas with an additional predictor variable transformed
    pred_transforms1 <- transform_preds(preds = preds)
    # pasting in response variable
    pred_transforms2 <- paste(response_var,  pred_transforms1) 
    names(pred_transforms2) <- names(pred_transforms1)
    
    # fitting glm's for each formula (all model objects)
    glm_list <- fit_mod(forms = pred_transforms2,df = df) #, ...)
    
    # sorting AIC
    aic_sorted <- map_dbl(glm_list, AIC) %>% 
      sort() 
    
    # putting output into list
    best_mod <- names(aic_sorted[1])# name of model with lowest aic
    
    # model with no transformations is considered best unless other
    # model is delta_aic better
    cat('\n', aic_sorted['convert_none'], "\n")
    cat(aic_sorted[best_mod], "\n")
    cat('delta aic cutoff', delta_aic, "\n")
    if((aic_sorted['convert_none'] - aic_sorted[best_mod]) < delta_aic) {
      best_mod <- 'convert_none'
    }
    out[[step_name]]$best <- best_mod 
    # for memory reasons now not saving the actually model objects
    #out[[step_name]]$glm <- glm_list # model objects
    out[[step_name]]$aic <- aic_sorted # AIC values sorted
    
    # preparing for next cycle through the loop
    i <- i + 1
    
    # parsing the predictor variables
    # of the best model into a vector
    preds_out <- pred_transforms1[[best_mod]] %>% 
      str_replace_all("[ ]|~", "") %>%  # remove spaces and ~
      # split based on presence of + (but b/ add constants in some places
      # don't split + if followed by a digit)
      str_split("\\+(?!\\d)") %>% 
      unlist() %>% 
      self_name()
    
    # transformation that took place this step. 
    # transformation of interactions depends on what transformation
    # happend to the main term, and is done in the same step
    # matching single but not double ::, (because e.g. stats::poly() is ok)
    preds_main <- str_subset(preds, pattern = '(?<!\\:)\\:(?!\\:)', negate = TRUE) # not interaction terms
    preds_out_main <- str_subset(preds_out, pattern = '(?<!\\:)\\:(?!\\:)', negate = TRUE) # not interaction terms
    if(all(preds_out_main %in% preds_main)) {
      diff = NA_character_
    } else {
      diff <- preds_out_main[!preds_out_main %in% preds_main]
      if(length(diff) > 1) {
        stop('issue with figuring out which var was transformed')
      }
    }
    out[[step_name]]$var_transformed <- diff
    
    # check if variable parsing worked
    # this line now commented out so that transformation 
    # can include replacing x with x +x^2
    # stopifnot(length(preds_out) == length(preds))
    preds <- preds_out
    
    # how many untransformed variables are left?
    
    steps_left <- sum(str_detect(preds, "\\(", negate = TRUE))
    # determine whether to go to the next step
    # if the best model is the one where no more  transformations
    # were done then don't continue
    
    if(out[[step_name]]$best == 'convert_none') {
      break
    }
  }
  # the formula of the best model in the final step
  out$final_formula <- pred_transforms2[best_mod]
  out
}


# misc --------------------------------------------------------------------


# this is just a copy of the partialPlot function from random forest
# (https://github.com/cran/randomForest/blob/master/R/partialPlot.R)
# but I deleted the code that leads to maddening behaviour where
# a variable can't be passed to x.var, i.e. now I just pass
# x.var straing to xname
myPartialPlot <-
  function (x, pred.data, x.var, which.class, w, plot=TRUE, add=FALSE,
            n.pt = min(length(unique(pred.data[, xname])), 51), rug = TRUE,
            xlab=deparse(substitute(x.var)), ylab="",
            main=paste("Partial Dependence on", deparse(substitute(x.var))),
            ...)
  {
    classRF <- x$type != "regression"
    if (is.null(x$forest))
      stop("The randomForest object must contain the forest.\n")

    xname <- x.var
    xv <- pred.data[, xname]
    n <- nrow(pred.data)
    if (missing(w)) w <- rep(1, n)
    if (classRF) {
      if (missing(which.class)) {
        focus <- 1
      }
      else {
        focus <- charmatch(which.class, colnames(x$votes))
        if (is.na(focus))
          stop(which.class, "is not one of the class labels.")
      }
    }
    if (is.factor(xv) && !is.ordered(xv)) {
      x.pt <- levels(xv)
      y.pt <- numeric(length(x.pt))
      for (i in seq(along = x.pt)) {
        x.data <- pred.data
        x.data[, xname] <- factor(rep(x.pt[i], n), levels = x.pt)
        if (classRF) {
          pr <- predict(x, x.data, type = "prob")
          y.pt[i] <- weighted.mean(log(ifelse(pr[, focus] > 0,
                                              pr[, focus], .Machine$double.eps)) -
                                     rowMeans(log(ifelse(pr > 0, pr, .Machine$double.eps))),
                                   w, na.rm=TRUE)
        } else y.pt[i] <- weighted.mean(predict(x, x.data), w, na.rm=TRUE)
        
      }
      if (add) {
        points(1:length(x.pt), y.pt, type="h", lwd=2, ...)
      } else {
        if (plot) barplot(y.pt, width=rep(1, length(y.pt)), col="blue",
                          xlab = xlab, ylab = ylab, main=main,
                          names.arg=x.pt, ...)
      }
    } else {
      if (is.ordered(xv)) xv <- as.numeric(xv)
      x.pt <- seq(min(xv), max(xv), length = n.pt)
      y.pt <- numeric(length(x.pt))
      for (i in seq(along = x.pt)) {
        x.data <- pred.data
        x.data[, xname] <- rep(x.pt[i], n)
        if (classRF) {
          pr <- predict(x, x.data, type = "prob")
          y.pt[i] <- weighted.mean(log(ifelse(pr[, focus] == 0,
                                              .Machine$double.eps, pr[, focus]))
                                   - rowMeans(log(ifelse(pr == 0, .Machine$double.eps, pr))),
                                   w, na.rm=TRUE)
        } else {
          y.pt[i] <- weighted.mean(predict(x, x.data), w, na.rm=TRUE)
        }
      }
      if (add) {
        lines(x.pt, y.pt, ...)
      } else {
        if (plot) plot(x.pt, y.pt, type = "l", xlab=xlab, ylab=ylab,
                       main = main, ...)
      }
      if (rug && plot) {
        if (n.pt > 10) {
          rug(quantile(xv, seq(0.1, 0.9, by = 0.1)), side = 1)
        } else {
          rug(unique(xv, side = 1))
        }
      }
    }
    invisible(list(x = x.pt, y = y.pt))
  }
