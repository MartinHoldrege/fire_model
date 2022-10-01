# Martin Holdrege

# Script started March 1, 2022

# Purpose: Misc. functions to be used in other scripts


# misc. -------------------------------------------------------------------


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


# names elements of x with the values of x
self_name <- function(x) {
  if(!is.null(names(x))){
    warning('x is already named are you sure you want to rename')
  }
  names(x) <- x
  x
}

#' replace poly(x,2) in a formula string
#'
#' @param form string, representation of a formula
#' @param keep_response whether to keep the response variable in the output
#' formula (i.e whether to remove the left side of the formula)
#'
#' @return the formula with poly(x,2), replaced by x + I(x^2),
#' doing this because predict.gnm seems to through errors with poly()
#' @examples
#' replace_poly("y ~x+poly(z,2)+sqrt(w) +poly(q,2)"),
#' replace_poly("y ~x+poly(z,2)+sqrt(w) +poly(q,2)", FALSE)
replace_poly <- function(form, keep_response = TRUE) {
  
  # split elements apart
  form_split <- form %>% 
    # remove response variable, and spaces
    str_replace_all("(^.*~)|(\\s)", "") %>% 
    str_split("\\+") %>% 
    unlist()
  
  left_side <- str_extract(form, "^.*~")
  # replace poly(x, 2) with x + I(x^2), this appears to be necessary
  # because of an issue with predict.gnm method
  is_poly <- str_detect(form_split, "poly\\(")
  
  # checking that these are all poly(x, 2), if poly(x, 3) etc.
  # this function would be incorrect
  stopifnot(all(str_detect(form_split[is_poly], '2')))
  
  poly_element <- form_split[is_poly] %>% 
    str_extract("(?<=poly\\()[A-z]+")
  
  sqr_elements <- paste0(poly_element, " + ", "I(", poly_element, "^2)")
  
  form_split2 <- form_split
  form_split2[is_poly] <- sqr_elements
  
  out <- paste(form_split2, collapse = " + ")
  
  #add left size of formula back in
  if(keep_response) {
    out <- paste0(left_side, out)
  }
  
  out
}

#' extract values from rasters
#'
#' @param lyr name of the layer
#' @param r raster
#'
#' @return dataframe containing value, cell_num and lyr columns,
#' NA rows removed
get_values <- function(lyr, r) {
  x <- values(r[[lyr]]) %>% 
    as.numeric()
  
  out <- tibble(
    value = x,
    cell_num = 1:length(x)
  ) %>% 
    filter(!is.na(value)) %>% 
    mutate(lyr = lyr)
  
  out 
}

#' create legend labels
#'
#' @param x numeric vector of break points
#'
#' @return character vector, where last category is just
#' > x[n-1] instead of showing a range

#' @examples
#' label_creator(1:5)
label_creator <- function(x) {
  n <- length(x)
  labels <- vector(mode = 'character', length = n-1)
  
  for (i in 1:(n-1)) {
    if(i < n -1) {
      labels[i] <- paste(x[i], "to", x[i+1])
    } else {
      labels[i] <- paste(">", x[i])
    }
  }
  
  labels
}

# quantile plots ----------------------------------------------------------

#' filter rows by climate variables
#' 
#' @description filters the dataframe by percentiles of the climate variable
#' columns. So the output includes rows corresponding the bottom 2 deciles and 
#' the top two deciles of each climate variable. 
#'
#' @param df dataframe that needs to have MAP, MAT, and prcpPropSum column
#'
#' @return dataframe with same columns as df but also filter_var,
#' percentile_category, which give the names of the climate variable filtered 
#' by and the percentile cut-off used that the given row fits in
filter_by_climate <- function(df) {
  
  # percentile cuttoffs to use, keeping values below low, and above high
  low <- 0.2
  high <- 0.8
  
  # creating total herbacious biomass category
  df$herbAGB <- df$afgAGB + df$pfgAGB
  
  clim_vars <- c("MAT", "MAP", "prcpPropSum")
  names(clim_vars) <- clim_vars
  stopifnot(clim_vars %in% names(df))
  
  # fitting empirical cdf's so that percentiles of the climate variables
  # can be computed
  ecdf_list <- map(df[clim_vars], ecdf)
  
  # dataframe, where each column provides the percentiles of a climate variable
  # percentiles correspond to rows in df
  percentiles <- map_dfc(clim_vars, function(var) {
    ecdf_list[[var]](df[[var]])
  })
  
  # only keep rows falling in the low or high category, for each climate var
  df_filtered <- map_dfr(clim_vars, function(var) {
    df_low <- df[percentiles[[var]] < low, ]
    df_low$percentile_category <- paste0("<", low*100, "th")
    df_high <- df[percentiles[[var]] > high, ] 
    df_high$percentile_category <- paste0(">", high*100, "th")
    out <- bind_rows(df_low, df_high)
    out$filter_var <- var
    out$percentile_category <- as.factor(out$percentile_category)
    out
  })
  
  df_filtered
}

# df <- filter_by_climate(pred_glm1$paint)

# the functions below, are useful for preparing data for and making 
# quantile dotplots

#' Make long format dataframe with predictor variable becoming a column
#'
#' @param df dataframe
#' @param response_vars names of response variables
#' @param pred_vars names of predictor variables
#' @param filter_var logical--whether this dataframe also includes 
#' filter_var and percentile_category columns that should be kept
#'
#' @return longform dataframe
predvars2long <- function(df, response_vars, 
                          pred_vars = c("afgAGB", "pfgAGB", "MAT", "MAP", 
                                        "prcpPropSum"),
                          filter_var = FALSE) {
  
  stopifnot(c('afgAGB', 'pfgAGB') %in% names(df))
  
  new_pred_vars <- c(pred_vars)
  # creating total herbacious biomass category
  if(!'herbAGB'%in% pred_vars) {
    df$herbAGB <- df$afgAGB + df$pfgAGB
    new_pred_vars <- c(new_pred_vars, 'herbAGB')
  }

  new_pred_vars <- c(pred_vars, 'herbAGB')
  
  select_cols <- c(new_pred_vars, response_vars)
  
  if(filter_var) {
    select_cols <- c(select_cols, c("filter_var", "percentile_category"))
  }
  
  # for weighted means (if relevant)
  if('numYrs' %in% names(df)){
    select_cols <- c(select_cols, 'numYrs')
  }
  
  out <- df[, select_cols] %>% 
    pivot_longer(cols = all_of(new_pred_vars))
  
  # turn into an ordered factor
  ordered <- c("afgAGB", "pfgAGB", "herbAGB", "MAT", "MAP", 
               "prcpPropSum")
  if(all(new_pred_vars %in% ordered) & all(ordered %in% new_pred_vars)) {
    out$name <- factor(out$name, levels = ordered)
  
    # otherwise just order the veg variables
  } else if(all(ordered[1:3] %in% new_pred_vars)) {
    levels <- c(ordered[1:3], new_pred_vars[!new_pred_vars %in% ordered[1:3]])
    out$name <- factor(out$name, levels = levels)
  }
  out
}




#' convert longform dataframe to means of predictor quantiles
#'
#' @param df with columns of name (name of predictor variable), 'value'
#' (value of predictor variable), and 1 or more response variable columns.
#' The output of predvars2long() creates a correctly formatted df to use here
#' @param response_vars character vector, names of the response variables
#' @param filter_var logical--whether this dataframe also includes 
#' filter_var and percentile_category columns that should be kept
#' @param weighted_mean logical, whether to take the weighted mean
#' of the observed fire probability (currently requires presence of
#' numYrs column).
#' 
#' @return For each predictor variable calculate the mean of each decile
#' and the corresponding mean (of those same rows) of the response variable
longdf2deciles <- function(df, response_vars, filter_var = FALSE,
                           weighted_mean = FALSE) {
  
  if(weighted_mean & !'numYrs' %in% names(df)) {
    stop('numYrs column not present (needed for weighted mean)')
  }

  stopifnot(c("name", "value", response_vars) %in% names(df))
  
  group_vars <- 'name'
  if(filter_var) {
    group_vars <- c(group_vars, c("filter_var", "percentile_category"))
  } 
  
  if(!filter_var & 'filter_var' %in% names(df)) {
    warning('dataframe includes a column named filter_var, the
            filter_var argument should probably be set to TRUE')
  }
  
  out0 <- df %>% 
    # the named vector in select was selecting by the names
    # not the vector values!
    select(all_of(group_vars), value, unname(response_vars), 
           # using matches here b/ if column not present
           # this will still work
           matches('numYrs')) %>% 
    group_by(across(all_of(group_vars))) %>% 
    nest() %>% 
    # empirical cdf
    mutate(cdf = map(data, function(df) ecdf(df$value)),
           # calculate the percentile of each data point based on the ecdf
           percentile = map2(data, cdf, function(df, f) f(df$value))) %>% 
    select(-cdf) %>% 
    unnest(cols = c("data", "percentile")) %>% 
    group_by(across(all_of(group_vars))) %>% 
    mutate(decile = cut(percentile, seq(0, 1, 0.01),
                        labels = 1:100)) %>% 
    # calculate mean of response variables for each decile of each predictor
    # variable
    group_by(across(all_of(c(group_vars, 'decile')))) 
  
  if(weighted_mean) {
    out <- out0 %>% 
      summarize(across(unname(response_vars), weighted.mean, w = numYrs),
                mean_value = mean(value), # mean of predictor for that decile
                .groups = 'drop')
    
  } else {
    out <- out0 %>% 
      summarize(across(unname(response_vars), mean),
                mean_value = mean(value), # mean of predictor for that decile
                .groups = 'drop')
  }

  out
}



#' wide format data frame, to longformat dataframe summarized to quantiles
#'
#' @description wrapper around predvars2long and longdf2deciles
#'
#' @param df 
#' @param response_vars character vector of response vars
#' @param pre_vars character vector of response vars
#' @param filter_var whether to create a filter var (i.e. filter by the
#' climate variables, so group data into high and low percentiles
#' of each climate variable)
predvars2deciles <- function(df, response_vars, pred_vars,
                             filter_var = FALSE,
                             weighted_mean = TRUE) {
  
  if (filter_var) {
    df <- filter_by_climate(df)
  }
  
  # longformat df
  long_df <- predvars2long(df, response_vars = response_vars, 
                           pred_vars = pred_vars,
                           filter_var = filter_var)
  # mean of deciles
  out <- longdf2deciles(long_df, response_vars = response_vars,
                        filter_var = filter_var,
                        weighted_mean = weighted_mean)
  out
}

#' calculate rmse for decile plot
#'
#' @param df dataframe with name column (containing names of predictor variables)
#' and columns corresponding to yvar and yvar_pred, where yvar is a fire
#' response variable
#' @param yvar string (e.g. mtbs_prop)
#'
#' @return dataframe, giving root mean squared error of quantile level
#' values
rmse4dotplot <- function(df, yvar) {
  df_list <- split(df, df$name) 
  observed <- yvar
  predicted <- paste0(yvar, "_pred")
  rmse_vector <- map_dbl(df_list, function(df){
    squared_error = (df[[observed]] - df[[predicted]])^2
    rmse <- sqrt(mean(squared_error))
  })
  rmse_vector
  
  # convert to dataframe for use in ggplot
  out <- tibble(name = names(rmse_vector),
                rmse = rmse_vector)
  out$rmse <- formatC(out$rmse, digits = 2, format = 'e')
  out
}

# rmse4dotplot(df, 'mtbs_prop')

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
decile_dotplot <- function(yvar, df, method = NULL, ylab = 'fire probability (per year)',
                           add_predicted = FALSE, title = NULL,
                           size = 0.75,
                           add_rmse = TRUE,
                           subtitle = NULL) {
  
  if('filter_var' %in% names(df)) {
    stop('filter_var column present, you should used decile_dotplot_filtered()')
  }
  
  caption <- "Each panel shows a different predictor variable"
  if(is.null(subtitle) & !is.null(method)) {
    subtitle <- paste0('y variable is ', yvar, " (", method, " method)")
  }
  
  g <- ggplot(df, aes_string(x = 'mean_value', y = yvar)) +
    geom_point(aes(color = "Observed", shape = "Observed"),
               size = size) +
    facet_wrap(~name, scales = 'free_x') +
    labs(x = "mean of quantile of predictor variable",
         y = ylab,
         subtitle = subtitle,
         title = title) +
    theme(legend.position = 'top',
          legend.title = element_blank()) 
  g
  
  col_values <- c("Observed" = "black")
  shape_values <- c("Observed" = 19)
  
  # whether to also add dots for predicted probability
  # this requires predicted value columns to have the same name as yvar
  # but followed by _pred
  if(add_predicted) {
    yvar_pred <- paste0(yvar, "_pred")
    
    g <- g +
      geom_point(aes_string(y = yvar_pred), color = "blue", alpha = 0.5,
                 shape = 17, size = size) 
    col_values <- c("Observed" = "black", "Predicted" = "blue")
    shape_values <- c(shape_values, "Predicted" = 17)
  }
  
  if (add_predicted & add_rmse) {
    rmse_df <- rmse4dotplot(df = df, yvar = yvar)
    
    if(is.factor(df$name)){
      rmse_df$name <- factor(rmse_df$name, levels = levels(df$name))
    }
    caption = paste0(caption, 
                    "\nRMSE of quantile averages shown in each panel")
    g <- g +
      geom_text(data = rmse_df, 
                aes(x = -Inf, y = Inf, label = rmse, hjust = -0.05,
                    vjust = 1.5), size = 3)
    g
  }
  
  out <- g +
    scale_color_manual(name = 'legend', values = col_values) +
    scale_shape_manual(name = 'legend', values = shape_values) +
    labs(caption = caption)
  out
}

#' create dotplot of data summarized to quantiles
#' 
#' @description for each vegetation 
#' variable, only showing data that falls in the highest or lowest deciles
#' of each of the climate variables
#'
#' @param yvar name of the yvar to be plotted (string) (must be present in df) 
#' @param df dataframe longform in longform, should be output of 
#' predvars2deciles with the filter_var argument set to TRUE
#' @param method string, to be pasted into subtitle (method used to convert
#' polygons to rasters)
#' @param add_smooth logical--whether to add splines
#' @param title plot title
#' @param size size of points
#' @param ylab string--y axis label
decile_dotplot_filtered <- function(yvar, df, method = NULL, ylab = 'fire probability (per year)',
                           add_smooth = FALSE,
                           title = NULL,
                           size = 0.75,
                           subtitle = NULL) {
  # all_of() seems to break down if yvar has name different than the values
  yvar <- unname(yvar) 
  df2 <- df %>% 
    filter(name %in% c("afgAGB", "pfgAGB", "herbAGB")) %>% 
    select(name, filter_var, percentile_category, decile, mean_value,
           all_of(yvar), all_of(paste0(yvar, "_pred"))) %>% 
    pivot_longer(cols = all_of(c(yvar, paste0(yvar, "_pred"))),
                 names_to = 'source',
                 values_to = 'probability') %>% 
    mutate(source = ifelse(str_detect(source, "_pred$"),
                           "predicted", "observed"),
           percentile_category = paste0(percentile_category, " (", source, ")"))
  
  if(is.null(subtitle) & !is.null(method)) {
    subtitle <- paste0('y variable is ', yvar, " (", method, " method)")
  }
  
  g <- ggplot(df2, aes(x = mean_value, y = probability)) +
    geom_point(aes(color = percentile_category,
                   shape = percentile_category),
               size = size) +
    facet_grid(filter_var~name, scales = 'free_x') +
    labs(x = "mean of quantile of predictor variable",
         y = ylab,
         caption = paste0("Columns show the predictor variable\n",
                          "Rows show the variable data was filtered by", 
                          " (i.e., only pixels falling in the lowest or highest two deciles were kept)"),
         subtitle = subtitle,
         title = title) +
    theme(legend.position = 'top',
          legend.title = element_blank()) +
    # different colors for each combination of percentile and observed vs predicted,
    # shapes are observed (circles) vs predicted (triangles)
    scale_colour_manual(name = 'percentile_category',
                        values = c("#f03b20","#feb24c", "#0570b0", "#74a9cf"))+
    scale_shape_manual(name = 'percentile_category',
                       values = c(19, 17, 19, 17))
  if(add_smooth) {
    g <- g +
      geom_smooth(aes(color = percentile_category),
                  se = FALSE)
  }
  g
}

# df <- predvars2deciles(filter_by_climate(pred_glm1$paint),
#                        response_vars = response_vars,
#                        pred_vars = pred_vars,
#                        filter_var = TRUE) 



