# Martin Holdrege

# Script started March 1, 2022

# Purpose: Misc. functions to be used in other scripts
library(patchwork)
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

#' extract values as a vector from raster
#'
#' @param r raster
#'
#' @return vector of cell values, NAs removed
values_nona <- function(r) {
  x <- as.numeric(values(r)) 
  out <- x[!is.na(x)]
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
label_creator <- function(x, convert2percent = FALSE) {
  if(convert2percent) x <- x*100
  
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


#' convert variable abbreviation to a better label
#'
#' @param x character vector
#' @param units_md whehter to include units in markdown format
#' @param add_letters whether to add letters to factor levels
#' @param include_hmod whether to include human modification as a level
#' 
#' @return vector same length as x, with fuller labels. Except if x 
#' is null, then just the lookup vector returned
#'
#' @examples
#'  var2lab(rev(factor(c("afgAGB", "prcpPropSum", 'MAT', 'MAP', 'pfgAGB'))))
#'  var2lab("prcpPropSum")
var2lab <- function(x = NULL, units_md = FALSE, add_letters = FALSE,
                    include_hmod = FALSE) {
  
  
  # Including units that are written using markdown formating
  lookup_md <- c(
    "MAT" = "MAT (\u00b0C)",
    "MAP" = "MAP (mm)",
    "prcpPropSum" = "Proportion summer ppt",
    "afgAGB" = "Annual biomass (g/m<sup>2</sup>)",
    "pfgAGB" = "Perennial biomass (g/m<sup>2</sup>)"
  )
  
  lookup_name_only <- c(
    "MAT" = "MAT",
    "MAP" = "MAP",
    "prcpPropSum" = "Proportion summer ppt",
    "afgAGB" = "Annual biomass",
    "pfgAGB" = "Perennial biomass"
  )
  
  # if human modification layer included in the input,
  # add it here (if not present, it won't be included as 
  # a factor level)
  if("hmod" %in% x | include_hmod) {
    lookup_md <- c(lookup_md, "hmod" = "Human modification")
    lookup_name_only <- c(lookup_name_only, "hmod" = "Human modification")
    
  }
  stopifnot(as.character(x) %in% names(lookup_md))


  
  lookup <- if(units_md) {
    lookup_md
  } else {
    lookup_name_only
  }
  
  if(add_letters) {
    new_lookup <- paste(fig_letters[1:length(lookup)], lookup)
    names(new_lookup) <- names(lookup)
    lookup <- new_lookup
  }
  
  if(is.null(x)) {
    return(lookup)
  }
  
  out <- lookup[as.character(x)]
  
  # convert to a factor (for ordering in figures)
  out <- factor(out, levels = lookup)

  out
}

#' spatially group cell numbers
#'
#' @param r spatraster, to be used as a template. Needs to have
#' the same extent, projection, resolution etc. as the rasters
#' from which data of interest came from
#' @param fact passed to aggregate(). Number of cells (in each direction) to
#' aggregate into a bigger cell
#'
#' @return table with two columns cell_num (the original cell_num) and
#' group_cell_num giving the number of the larger 'grouped' cell that the
#' specific cell_num belongs to
#' 
#' @example 
#' r <- rast(nrow = 11, ncol = 26)
#' r[] <- 1:ncell(r)
#' plot(r)
#' group_cell_nums(r, fact = 5)
group_cell_nums <- function(r, fact = 100) {
  r <- r[[1]] # in case r is multi-layered
  cell_num <- 1:terra::ncell(r) 
  r[] <- cell_num
  
  r_agg <- terra::aggregate(r, fact = fact, fun = 'min')
  
  r_agg[] <- 1:terra::ncell(r_agg)
  
  # convert back to the original resolution, but with
  # grid cellsof the aggregrated
  # raster
  r_disagg <- terra::disagg(r_agg, fact = fact)
  
  # aggregate can increase the extent when fact doesn't divid perfectly into
  # the dimensions, therefore r_disagg can also be bigger than r
  # here cropping back so that r_disagg has the same number of
  # cells as r
  r_disagg2 <- terra::crop(r_disagg, r)
  out <- tibble::tibble(cell_num = cell_num,
                group_cell_num = as.numeric(terra::values(r_disagg2)))
  out
}

# quantile plots ----------------------------------------------------------

#' filter rows by climate variables
#' 
#' @description filters the dataframe by percentiles of the climate variable
#' columns. So the output includes rows corresponding the bottom 2 deciles and 
#' the top two deciles of each climate variable. Note this function has
#' been updated to filter by an arbitrary number of columns (not just climate)
#'
#' @param df dataframe that needs to have MAP, MAT, and prcpPropSum column
#' @param add_mid also add a seperate category of the center 2 deciles of 
#' each climate variable
#' @param filter_vars variables (usually climate variables), to split the others
#' into response and predicted
#'
#' @return dataframe with same columns as df but also filter_var,
#' percentile_category, which give the names of the climate variable filtered 
#' by and the percentile cut-off used that the given row fits in

filter_by_climate <- function(df, add_mid = FALSE,
                              filter_vars = c('MAT', 'MAP', 'prcpPropSum')) {
  
  # percentile cuttoffs to use, keeping values below low, and above high
  low <- 0.2
  high <- 0.8
  
  # creating total herbacious biomass category
  df$herbAGB <- df$afgAGB + df$pfgAGB
  
  names(filter_vars) <- filter_vars
  stopifnot(filter_vars %in% names(df))
  
  # fitting empirical cdf's so that percentiles of the climate variables
  # can be computed
  ecdf_list <- map(df[filter_vars], ecdf)
  
  # dataframe, where each column provides the percentiles of a climate variable
  # percentiles correspond to rows in df
  percentiles <- map_dfc(filter_vars, function(var) {
    ecdf_list[[var]](df[[var]])
  })
  
  # only keep rows falling in the low or high category, for each climate var
  df_filtered <- map_dfr(filter_vars, function(var) {
    df_low <- df[percentiles[[var]] < low, ]
    df_low$percentile_category <- paste0("<", low*100, "th")
    df_high <- df[percentiles[[var]] > high, ] 
    df_high$percentile_category <- paste0(">", high*100, "th")
    
    out <- bind_rows(df_low, df_high)
    
    out$percentile_category <- as.factor(out$percentile_category)
    
    # adding seperate category for the middle of the data
    if(add_mid){
      df_mid <- df[percentiles[[var]] < .6 & percentiles[[var]] > .4, ]
      df_mid$percentile_category <- "40th-60th"
      
      # create correct factor order
      levels <- levels(out$percentile_category)
      levels <- c(levels[1], "40th-60th", levels[2])
      # convert to character for binding
      out$percentile_category <- as.character(out$percentile_category)
      out <- bind_rows(out, df_mid)
      out$percentile_category <- factor(out$percentile_category,
                                         levels = levels)
      
    }
    out$filter_var <- var
    
    out
  })
  df_filtered$filter_var <- factor(df_filtered$filter_var, levels = filter_vars)
  df_filtered
}

# df <- filter_by_climate(pred_glm1$paint)

# the functions below, are useful for preparing data for and making 
# quantile dotplots

#' Make long format dataframe with predictor variable becoming a column
#'
#' @param df dataframe (could be output from filter_by_climate)
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
#' @param return_mean logical. if false return the dataframe before
#' means have been calculated for each quantile
#' 
#' @return For each predictor variable calculate the mean of each decile
#' and the corresponding mean (of those same rows) of the response variable
longdf2deciles <- function(df, response_vars, filter_var = FALSE,
                           weighted_mean = FALSE,
                           return_means = TRUE,
                           cut_points = seq(0, 1, 0.01)) {
  
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
    mutate(decile = cut(percentile, cut_points,
                        labels = 1:(length(cut_points) - 1))) %>% 
    # calculate mean of response variables for each decile of each predictor
    # variable
    group_by(across(all_of(c(group_vars, 'decile')))) 
  
  if(!return_means) {
    return(out0)
  }
  
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
#' @param filter_vars vector of names of columns to use
#' as filter variables (only relevant if filter_var = TRUE)
#' @param add_mid whether to also filter by the middle percentiles of the 
#' climate vars
#' @param cut_points how to group/cut the percentiles before avging. By default
#' compute the average for each percentile
predvars2deciles <- function(df, response_vars, pred_vars,
                             filter_var = FALSE,
                             filter_vars = c('MAT', 'MAP', 'prcpPropSum'),
                             weighted_mean = TRUE,
                             add_mid = FALSE,
                             cut_points = seq(0, 1, 0.01)) {
  
  stopifnot(
    is.logical(filter_var)
  )
  
  if (filter_var) {
    stopifnot(is.character(filter_vars))
    df <- filter_by_climate(df, add_mid = add_mid,
                            filter_vars = filter_vars)
  }
  
  # longformat df
  long_df <- predvars2long(df, response_vars = response_vars, 
                           pred_vars = pred_vars,
                           filter_var = filter_var)
  # mean of deciles
  out <- longdf2deciles(long_df, response_vars = response_vars,
                        filter_var = filter_var,
                        weighted_mean = weighted_mean,
                        cut_points = cut_points)
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

#' publication quality  dotplot of data summarized to quantiles
#'
#' @param df dataframe longform with 'mean_value' column
#' (i.e. mean value of the predictor variable for the given decile) and 'name' 
#' column which gives the name of the predictor variable
decile_dotplot_pq <- function(df, size = 0.5) {
  
  if('filter_var' %in% names(df)) {
    stop('filter_var column present, you should used decile_dotplot_filtered()')
  }
  
  
  # convert k to c
    if(max(df[df$name == "MAT", ]$mean_value) > 150) {
      df[df$name == "MAT", ]$mean_value <- df[df$name  == "MAT", ]$mean_value - 273.15
    }
  
  
  df2 <- df %>% 
    filter(!name %in% 'herbAGB') %>% 
    mutate(name = var2lab(name, units_md = TRUE)) %>% 
    arrange(name) 
  
  
  

  yvar <- "cwf_prop"
  yvar_pred <- paste0(yvar, "_pred")
  
  # convert to %
  df2[[yvar]] <- df2[[yvar]]*100
  df2[[yvar_pred]] <- df2[[yvar_pred]]*100
  
  letter_df <- tibble(
    letter = fig_letters[1:length(unique(df2$name))],
    name = factor(levels(df2$name)),
    x = -Inf,
    y = Inf
  )
  
  g <- ggplot(df2, aes(x = mean_value, y = cwf_prop)) +
    geom_point(aes(color = "Observed", shape = "Observed"),
               size = size, alpha = 0.6) +
    geom_point(aes(y = cwf_prop_pred, color = 'Predicted', alpha = 0.5,
               shape = 'Predicted'), size = size, alpha = 0.6) +
    geom_text(data = letter_df, aes(x = x, y = y, label = letter),
              hjust = -0.8,
              vjust = 1) +
    facet_wrap(~name, scales = 'free_x', strip.position = "bottom") +
    # using annotate to add in line segements because lemon package (facet_rep_wrap)
    # isn't being maintained anymore
    annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, size = 1) +
    labs(y = lab_fireProbPerc) +
    theme(legend.position = 'top',
          legend.title = element_blank(),
          #strip.text = element_text(),
          strip.background = element_blank(),
          strip.text = ggtext::element_markdown(),
          strip.placement = "outside",
          axis.title.x = element_blank(),
          axis.line.y = element_blank())+
    scale_color_manual(name = 'legend', values = c("Observed" = "black", "Predicted" = "blue")) +
    scale_shape_manual(name = 'legend', values =  c("Observed" = 19, "Predicted" = 17))
  
  g
}

#' add observed vs predicted inset to quantile dotplot
#'
#' @param g ggplot object created in decile_dotplot_pq
#' @param df dataframe (same dataframe used for decile_dotplot_pq)
#' @param add_smooth whether to add a smoother to the inset
add_dotplot_inset <- function(g, df, add_smooth = FALSE) {
  max <- max(df$cwf_prop, df$cwf_prop_pred)*100
  inset <- ggplot(df, aes(cwf_prop_pred*100, cwf_prop*100)) +
    geom_point(shape = 4, alpha = 0.2, size = 0.3) +
    geom_abline(slope = 1, size = 0.5) +
    labs(y = "Observed probability (%)",
         x = "Predicted probability (%)") +
    geom_text(data = tibble(
      letter = fig_letters[6],
      x = -Inf,
      y = Inf),
      aes(x = x, y = y, label = letter),
      hjust = -0.8, vjust = 1) +
    theme(axis.title = element_text(size = 6),
          axis.text = element_text(size = 6))+
    coord_cartesian(xlim = c(0, max),
                    ylim = c(0, max))
  
  if(add_smooth) {
    inset <- inset +
      geom_smooth(se = FALSE, color = 'gray')
  }
  
  
  g2 <- g +
    inset_element(inset, left = 0.7, bottom = -0.15,
                  right = 1, top = 0.4)
  g2
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


#' create dotplot of data summarized to quantiles, publication quality
#' 
#' @description for each vegetation 
#' variable, only showing data that falls in the highest or lowest deciles
#' of each of the climate variables. 
#'

#' @param df dataframe longform in longform, should be output of 
#' predvars2deciles with the filter_var argument set to TRUE
#' @param add_smooth logical--whether to add splines
#' @param size size of points
#' @param xvars = vector of variables to show along the x axis
decile_dotplot_filtered_pq <- function(df,
                                    add_smooth = TRUE,
                                    size = 0.5,
                                    return_df = FALSE,
                                    xvars = c("afgAGB", "pfgAGB")
) {

  yvar <- "cwf_prop"
  
  # convert k to c
  df[df$name == "MAT", ]$mean_value <- 
    df[df$name == "MAT", ]$mean_value - 273.15
  df2 <- df %>% 
    filter(name %in% xvars) %>% 
    select(name, filter_var, percentile_category, decile, mean_value,
           all_of(yvar), all_of(paste0(yvar, "_pred"))) %>% 
    pivot_longer(cols = all_of(c(yvar, paste0(yvar, "_pred"))),
                 names_to = 'source',
                 values_to = 'probability') %>% 
    mutate(source = ifelse(str_detect(source, "_pred$"),
                           "predicted", "observed")) %>% 
    arrange(percentile_category, source) %>%  # so factor is ordered
    mutate(percentile_category = paste0(percentile_category, " (", source, ")"),
           percentile_category = factor(percentile_category, levels = unique(percentile_category)),
           probability = probability*100) # convert to %)
  
  letter_df <- expand_grid(filter_var = unique(df2$filter_var), 
                           name = unique(df2$name)) %>% 
    mutate(letter = fig_letters[1:n()],
           x = -Inf,
           y = Inf)
  
  if(return_df) {
    return(df2)
  }
  
  # more colors for when mid category is included
  if (length(unique(df2$percentile_category)) == 6) {
    # reds, greens, blues
    colors <- c("#f03b20","#feb24c", "#31a354", "#addd8e", "#0570b0", "#74a9cf")
    shapes <- c(19, 17, 19, 17, 19, 17)
  } else {
    colors <- c("#f03b20","#feb24c", "#0570b0", "#74a9cf")
    shapes <- c(19, 17, 19, 17)
  }
  
  g <- ggplot(df2, aes(x = mean_value, y = probability)) +
    geom_point(aes(color = percentile_category,
                   shape = percentile_category),
               size = size) +
    facet_grid(filter_var~name, scales = 'free_x', switch = 'x'
               ,labeller = labeller(filter_var = ~var2lab(.x, FALSE),
                                   name = ~var2lab(.x, TRUE))
               ) +
    labs(#x = "mean of quantile of predictor variable",
         y = lab_fireProbPerc,
         tag = 'Filtering variable') +
    # using annotate to add in line segements because lemon package (facet_rep_wrap)
    # isn't being maintained anymore
    annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, size = 0.7) +
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, size = 0.7) +
    geom_text(data = letter_df, aes(x = x, y = y, label = letter),
              hjust = -0.8,
              vjust = 1)+
    labs(y = lab_fireProbPerc) +
    theme(legend.position = 'top',
          legend.title = element_text(size = 9),
          #strip.text = element_text(),
          strip.background = element_blank(),
          strip.text = ggtext::element_markdown(),
          strip.placement = "outside",
          axis.title.x = element_blank(),
          axis.line = element_blank(),
          plot.tag.position = c(1.01, 0.5),
          plot.tag = element_text(angle = 270),
          plot.margin = unit(c(5.5, 20, 5.5, 5.5), "points")) +
    # different colors for each combination of percentile and observed vs predicted,
    # shapes are observed (circles) vs predicted (triangles)
    scale_colour_manual(name = "Percentile of filtering variable",
                        values = colors)+
    scale_shape_manual(name = "Percentile of filtering variable",
                       values = shapes) +
    guides(colour = guide_legend(title.position="top", title.hjust = 0.5)) +
    coord_cartesian(clip = 'off')
  
  if(add_smooth) {
    g <- g +
      geom_smooth(aes(color = percentile_category),
                  se = FALSE,
                  size = 0.7)
  }
  g
}

# helper function for generating legends
legend_generator <- function(df, percentile_name, variable_name) {
  legend_name = paste(percentile_name, variable_name)
  
  colors <- c("#f03b20","#feb24c", "#0570b0", "#74a9cf")
  shapes <- c(19, 17)
  
  if(percentile_name == "Low") {
    colors <- colors[1:2]
  } else {
    colors <- colors[3:4]
  }
  
  
  g1 <- df %>% 
    filter(percentile_name == percentile_name) %>% 
    ggplot(aes(mean_value, probability, color = source, shape = source)) +
    geom_point() +
    geom_line() +
    scale_colour_manual(name = legend_name,
                        values = colors)+
    scale_shape_manual(name =  legend_name,
                       values = shapes) +
    guides(color = guide_legend(title.position = "left"),
           shape = guide_legend(title.position = "left")) +
    theme(legend.title = element_text(size = 8),
          legend.text = element_text(size = 6))
  
  cowplot::get_legend(g1)
}

# generate a list of legends (to be used decile_dotplot_filter_pq2)
generate_all_legends <- function(df) {
  var_names <- c("MAT", "MAP", "prop. sum. ppt.")
  percentile_names <- c("Low", "High")
  df_names <- expand_grid(var_names, percentile_names)
  
  legends <- list()
  for(i in 1:nrow(df_names)) {
    legends[[i]] <- legend_generator(df = df, 
                                     percentile_name = df_names$percentile_names[i],
                                     variable_name = df_names$var_names[i])
  }
  legends
}



#' create dotplot of data summarized to quantiles, publication quality.
#' 2nd version that has seperate legends for each row of figures
#' 
#' @description for each vegetation 
#' variable, only showing data that falls in the highest or lowest deciles
#' of each of the climate variables. 
#'

#' @param df dataframe longform in longform, should be output of 
#' predvars2deciles with the filter_var argument set to TRUE
#'
#' @param insets_left list of inset elements (from patchwork) to
#' add to the left column of plots
#' @param insets_right list of inset elements to add to right column of plots
#' @param size size of points
decile_dotplot_filtered_pq2 <- function(df,
                                        size = 0.5,
                                        insets_left = NULL,
                                        insets_right = NULL
) {
  
  df2 <- decile_dotplot_filtered_pq(df, return_df = TRUE) %>% 
    mutate(percentile_name = case_when(
      str_detect(percentile_category, "<") ~ "Low",
      str_detect(percentile_category, ">") ~ "High",
      TRUE ~ "Other"
    ))
  
  letter_df <- expand_grid(filter_var = unique(df2$filter_var), 
                           name = unique(df2$name)) %>% 
    mutate(letter = fig_letters[1:n()],
           x = -Inf,
           y = Inf)
  
  # more colors for when mid category is included
  if(length(unique(df2$percentile_category)) != 4) {
    stop('This function only works w/ low and high percentile categories (no
          more)')
  }
  
  colors <- c("#f03b20","#feb24c", "#0570b0", "#74a9cf")
  shapes <- c(19, 17, 19, 17)
  
  # base for filtered plot
  base_filt <- function() {
    list(
      geom_point(aes(color = percentile_category,
                     shape = percentile_category),
                 size = size),
      facet_grid(filter_var~name),
      # using annotate to add in line segements because lemon package (facet_rep_wrap)
      # isn't being maintained anymore
      annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, size = 0.7),
      annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, size = 0.7),
      theme(legend.position = 'none',
            strip.background = element_blank(),
            strip.text = element_blank(),
      ),
      # different colors for each combination of percentile and observed vs predicted,
      # shapes are observed (circles) vs predicted (triangles)
      scale_colour_manual(values = colors),
      scale_shape_manual(values = shapes),
      geom_smooth(aes(color = percentile_category),
                  se = FALSE,
                  size = 0.7),
      coord_cartesian(ylim = range(df2$probability))
    )
    
    
  }
  g_pfg <- df2 %>% 
    filter(name == "pfgAGB") %>% 
    ggplot(aes(x = mean_value, y = probability)) +
    base_filt() +
    geom_text(data = filter(letter_df, name == "pfgAGB") , aes(x = x, y = y, label = letter),
              hjust = -0.8,
              vjust = 1) +
    theme(axis.text.y = element_blank(),
          #axis.ticks.y = element_blank(),
          axis.title.x = ggtext::element_markdown()) +
    labs(y = NULL,
         x = var2lab('pfgAGB', TRUE))
  
  g_afg <- df2 %>% 
    filter(name == "afgAGB") %>% 
    ggplot(aes(x = mean_value, y = probability)) +
    base_filt() +
    geom_text(data = filter(letter_df, name == "afgAGB") , aes(x = x, y = y, label = letter),
              hjust = -0.8,
              vjust = 1) +
    theme(axis.title.x = ggtext::element_markdown()) +
    labs(y = lab_fireProbPerc,
         x = var2lab('afgAGB', TRUE))
  
  # create seperate legends to be added onto the figure
  legends <- generate_all_legends(df2)
  legends2 <- map(legends, ggplotify::as.ggplot)
  
  # combining into grid
  # grid layout
  m <- 2 # middle (left to right
  b <- 9 # bottom
  ll <- 4 # left of legend
  lr <- 5 # right of legend
  layout <- c(
    # terra also has area function
    patchwork::area(1, 1, b, m),
    patchwork::area(1, m+1, b, 4),
    patchwork::area(1, ll, r = lr),
    patchwork::area(2, ll, r = lr),
    patchwork::area(4, ll, r = lr),
    patchwork::area(5, ll, r = lr),
    patchwork::area(7, ll, r = lr),
    patchwork::area(8, ll, r = lr)
  )
  # plot(layout)
  out <- g_afg + g_pfg 
  
  if(!is.null(insets_left) & !is.null(insets_right)) {
    out <- g_afg + insets_left +  g_pfg + insets_right

  }
  
  out <- out + 
    legends2[[1]] + legends2[[2]] +
    legends2[[3]] + legends2[[4]] +
    legends2[[5]] + legends2[[6]] +
    plot_layout(design = layout, 
                widths = c(1, 0.2, 1, 0.2, 0.8))
  
  out
}

#' create inset obs vs predicted plots for 
#' for the filtered quantile plots. single inset created
#'
#' @param df dataframe, long form, quantile averages, for a given predictor variable
#' and filter 
#' 
#'
#' @return ggplot scatterplot (observed vs predicted, colored by high/low
#' of the climate variable)
create_inset_filt <- function(df) {
  # red, blue. these are the mid point (https://meyerweb.com/eric/tools/color-blend/_)
  # of the two red and blue colors
  # used for obs/predicted in the main figure)
  colors <- c("<20th" = "#F77736",  ">80th" = "#3D8DC0")
  ggplot(df, aes(x = cwf_prop_pred*100, y = cwf_prop*100)) +
    geom_abline(slope = 1, linewidth = 0.5, color = 'grey') +
    geom_point(aes(color = percentile_category), 
               shape = 4, size = 0.3) +
    # setting lims to 3 makes 1 point not visible
    # but rest of plot much clearer
    coord_cartesian(xlim = c(0, 3), ylim = c(0, 3)) +
    theme(legend.position = 'none',
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_text(size = 5.5, vjust = 4),
          axis.title.y = element_text(size = 5.5, vjust = -3),
          plot.margin = margin()
    ) +
    labs(y = "Observed",
         x = "Predicted") +
    scale_color_manual(values = colors)
}
