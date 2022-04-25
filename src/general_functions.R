# Martin Holdrege

# Script started March 1, 2022

# Purpose: Misc. functions to be used in other scripts


#' calculate yearly fire probability
#'
#' @param long_term_prob Probability of 1 or more fires over a number of years 
#' (nyears)
#' @param nyears The number of years the long_term_prob refers to
#'
#' @return probability of fire in any given year. 
calc_yearly_prob <- function(long_term_prob, nyears = 35) {
  
  # input must be a probability between 0 and 1
  stopifnot(
    max(long_term_prob) <=1,
    min(long_term_prob) >=0
  )
  
  # probability of no first over nyears
  long_term_no_fire <- 1 - long_term_prob
  
  # probability of no fire in a given year
  yearly_no_fire <- long_term_no_fire^(1/nyears)
  
  # probability of fire in a given year
  yearly_fire <- 1 - yearly_no_fire
  
  yearly_fire
}


#' calculate variance explained of glm, calculated on the response scale
#' 
#' @description I created this fun to work for glm's fit woth a gaussian
#' family but a non-identity link, it won't make sense for all GLMS
#'
#' @param mod glm object
#'
#' @return R squared
var_explained <- function(mod) {
  stopifnot('glm' %in% class(mod))
  y <- mod$y
  SST <- sum((y - mean(y))^2)
  # Note here that type = response, this means this is measuring how
  # well the model fit on the response scale (i.e. this wouldn't make
  # sense for many types of GLMS)
  pred <- predict(mod, type = 'response')
  SSE <- sum((pred - y)^2)
  R2 <- 1 - SSE/SST
  R2 # R squared
}


#' flatten and rename list elements
#'
#' @param x a list where each list item is a sublist
#'
#' @return a list where each item are elements of the original sublist,
#' but with the names of the two levels of the original list pasted together
#' @examples
#' x = list(upper1 = list('a' = 1), upper2 = list('a' = 21, 'b' = 27))
#' flatten_rename(x)
flatten_rename <- function(x) {
  out <- purrr::flatten(x) # flatten list
  top_names <- names(x) # names of top level of list
  n_lower <- map_dbl(x, length)
  n_top <- length(x)
  # repeat the name of the highest list level, for each
  # component of the sublist
  rep_top_names <- top_names[rep.int(1:n_top, times = n_lower)] 
  lower_names <- names(out)
  stopifnot(length(lower_names) == length(rep_top_names))
  names(out) <- paste(rep_top_names, lower_names, sep = "_")
  out
}


#' convert longform dataframe to means of predictor deciles
#'
#' @param df with columns of name (name of predictor variable), 'value'
#' (value of predictor variable), and 1 or more response variable columns
#' @param response_vars character vector, names of the response variables
#'
#' @return For each predictor variable calculate the mean of each decile
#' and the corresponding mean (of those same rows) of the response variable
longdf2deciles <- function(df, response_vars) {
  
  stopifnot(c("name", "value", response_vars) %in% names(df))
  out <- df %>% 
    # the named vector in select was selecting by the names
    # not the vector values!
    select(name, value, unname(response_vars)) %>% 
    group_by(name) %>% 
    nest() %>% 
    # empirical cdf
    mutate(cdf = map(data, function(df) ecdf(df$value)),
           # calculate the percentile of each data point based on the ecdf
           percentile = map2(data, cdf, function(df, f) f(df$value))) %>% 
    select(-cdf) %>% 
    unnest(cols = c("data", "percentile")) %>% 
    group_by(name) %>% 
    mutate(decile = cut(percentile, seq(0, 1, 0.01),
                        labels = 1:100)) %>% 
    # calculate mean of response variables for each decile of each predictor
    # variable
    group_by(name, decile) %>% 
    summarize(across(unname(response_vars), mean),
              mean_value = mean(value), # mean of predictor for that decile
              .groups = 'drop')
  out
}

# names elements of x with the values of x
self_name <- function(x) {
  if(!is.null(names(x))){
    warning('x is already named are you sure you want to rename')
  }
  names(x) <- x
  x
}
# figure making functions -------------------------------------------------

#' create dotplot of data summarized to deciles, faceted by predictor variable
#'
#' @param yvar name of the yvar to be plotted (string) (must be present in df) 
#' @param df dataframe longform with 'mean_value' column
#' (i.e. mean value of the predictor variable for the given decile) and 'name' 
#' column which gives the name of the predictor variable
#' @param method string, to be pasted into subtitle (method used to convert
#' polygons to rasters)
#' @param ylab string--y axis label
#' @param add_predicted logical, whether to also add model predicted data
#' to the plot. this requires the dataframe to 
decile_dotplot <- function(yvar, df, method, ylab = 'fire probability (per year)',
                           add_predicted = FALSE) {
  g <- ggplot(df, aes_string(x = 'mean_value', y = yvar)) +
    geom_point(aes(color = "Observed")) +
    facet_wrap(~name, scales = 'free_x') +
    labs(x = "mean of decile of predictor variable",
         y = ylab,
         caption = "each panel shows a different predictor variable",
         subtitle = paste0('y variable is ', yvar, " (", method, " method)")) +
    theme(legend.position = 'top',
          legend.title = element_blank()) 
  g
  
  col_values <- c("Observed" = "black")
  
  # whether to also add dots for predicted probability
  # this requires predicted value columns to have the same name as yvar
  # but followed by _pred
  if(add_predicted) {
    yvar_pred <- paste0(yvar, "_pred")
    
    g <- g +
      geom_point(aes_string(y = yvar_pred), color = "blue", alpha = 0.5) 
    col_values <- c("Observed" = "black", "Predicted" = "blue")
  }
  
  out <- g +
    scale_color_manual(values = col_values)
  out
}
