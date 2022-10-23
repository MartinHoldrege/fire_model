# Martin Holdrege

# functions useful for re-sampling/stratifying the fire probability predictor
# variables, to try and deal with the imbalence problem. 
 
library(tidyr)
library(dplyr)

#' Bin values into n equal sized bins
#' 
#' @description breaks x into n bins, where the bins by default
#' are 10 equal spaced categories between the 1st and 99th percentile of
#' x (values below 1st percentile and above 99th percentile get put in the
#' first and last bin, respectivel)
#'
#' @param x numeric vector
#' @param xlab string--label to be pasted into names of the bins
#' @param n_categories number of bins to create
#'
#' @return vector
#' @examples
#' x <- rnorm(20)
#' cut_bins(x)
cut_bins <- function(x, xlab = 'x', n_categories = 10) {
  lower <- quantile(x, 0.01)
  upper <- quantile(x, 0.99)
  
  break_points <- seq(lower, upper, length.out = n_categories + 1)
  
  # making the first ad last cut points infinity so that 
  # all data at the tails is included
  break_points[1] <- -Inf
  break_points[length(break_points)] <- Inf
  
  labels <- paste0(xlab, "-", 1:n_categories)

  out <- cut(x, breaks = break_points, labels = labels)
  out <- as.character(out)
  out
}


#' Bin columns of a dataframe
#'
#' @param df dataframe
#' @param cols columns to bin
#' @param n_categories number of groups to break each column into
#'
#' @return dataframe with new column bin_all column which is the
#' concatenation of all the bins
#' @examples
#' n = 1000
#' df <- tibble(
#'   x1 = rnorm(n),
#'   x2 = rnorm(n),
#'   x3 = runif(n),
#'   x4 = 3*x3 + runif(n, 0, 0.1),
#'   x5 = runif(n, 100, 200)
#' )
#' bin_df(df, names(df))
bin_df <- function(df, cols, n_categories = 10) {
  new_cols <- paste0('bin_', cols)
  
  out <- df
  for (i in seq_along(cols)) {
    col <- cols[i]
    new <- new_cols[i]
    out[[new]] <- cut_bins(out[[col]],
                           xlab = col,
                           n_categories = n_categories)
  }
  out <- unite(out, col = 'bin_all', all_of(new_cols), 
               sep = "_")
  out
}


#' Re-sample df based on bins
#' 
#' @param df data frame (output from bin_df())
#' @param return_target_bin_size =
#'
#' @return df with ~ same num of rows as the input, but each unique bin_all
#' value has the same number of rows
#' @example 
#' n = 40
#' df <- tibble(
#'   x3 = runif(n),
#'   x4 = 3*x3 + runif(n, 0, 0.01)
#' )
#' df2 <- bin_df(df, names(df))
#' resample_bins(df2)
#' resample_bins(df2, return_target_bin_size = TRUE)
#' 
resample_bins <- function(df, return_target_bin_size = FALSE) {
  
  stopifnot(
    "bin_all" %in% names(df)
  )
  
  n_bins <- length(unique(df$bin_all))
  n_obs <- nrow(df)
  
  n <- round(n_obs/n_bins)
  
  if(return_target_bin_size) {
    return(n)
  }
  out <- df %>% 
    group_by(bin_all) %>% 
    slice_sample(
      # making each bin the same size so end up with
      # ~ as many rows as the original dataset
      n = n,
      replace = TRUE)
  out
}
