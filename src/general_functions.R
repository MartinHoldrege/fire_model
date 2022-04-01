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
  
  # probility of no first over nyears
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
