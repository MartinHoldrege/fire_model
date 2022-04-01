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
  
  
  gridExtra::marrangeGrob(plots, nrow = 2, ncol = 2)
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
