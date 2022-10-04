# martin Holdrege

# Script started March 1, 2022

# Purpose:
# Parameters for figure making

source("src/general_functions.R")
library(RColorBrewer)
# misc --------------------------------------------------------------------

# so consistent letter theme is used throughout (for multiple
# panel pub quality figs)
fig_letters <- paste(letters, ")", sep = "")

# axis labels -------------------------------------------------------------

lab_fireProb <- c("fireProb" = "Long term fire probability (0-1)", 
                  "fireProbYr" = "Probability of fire in a given year (0-1)",
                  mtbsOccur = "Probability of fire in a given year (0-1)")

lab_fireProbPerc <- "Probability of fire in a given year (%)"

# biomass
lab_bio0 <- expression("Biomass ("*gm^-2*")")

# legend labels

lab_delta <- expression(Delta~Probability)

# colors ------------------------------------------------------------------

# colors for prcpPropSum
cols_prop <- c("#ffeda0","#feb24c", "#f03b20", # reds (from 3-class YlOrRd))
               "#fbb4b9", "#f768a1","#ae017e", # purples (from 4-class RdPu)
               "#bdc9e1", "#74a9cf", "#0570b0") # blues (from 4-class PuBu)

# change in fire probability with 5 c warming
breaks_delta0 <- c( 0.001, .002, .003, .005, .01, 0.1)
breaks_delta <- c(-rev(breaks_delta0), 0, breaks_delta0)

labels_delta <- label_creator(breaks_delta)
labels_delta[1] <- paste0("< ", breaks_delta[2])

cols_delta <- c(rev(brewer.pal(8, 'Greens')[-(1:2)]),
               brewer.pal(8, 'OrRd')[-(1:2)])

# colors and breaks for pfg and afg
breaks_bio1 <- c(0, 10, 20, 50, 100, 200, 300) # pfg
palette_bio1 <- brewer.pal(length(breaks_bio1), 'YlGn')
breaks_bio2 <- c(0, 5, 10, 20, 30, 50, 100, 200) #afg
palette_bio2 <- brewer.pal(length(breaks_bio1), 'YlGn')
