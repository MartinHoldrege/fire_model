# martin Holdrege

# Script started March 1, 2022

# Purpose:
# Parameters for figure making


# axis labels -------------------------------------------------------------

lab_fireProb <- c("fireProb" = "Long term fire probability (0-1)", 
                  "fireProbYr" = "Probability of fire in a given year (0-1)",
                  mtbsOccur = "Probability of fire in a given year (0-1)")

lab_fireProbPerc <- "Probability of fire in a given year (%)"

# biomass
lab_bio0 <- expression("Biomass ("*gm^-2*")")


# colors ------------------------------------------------------------------

# colors for prcpPropSum
cols_prop <- c("#ffeda0","#feb24c", "#f03b20", # reds (from 3-class YlOrRd))
               "#fbb4b9", "#f768a1","#ae017e", # purples (from 4-class RdPu)
               "#bdc9e1", "#74a9cf", "#0570b0") # blues (from 4-class PuBu)
