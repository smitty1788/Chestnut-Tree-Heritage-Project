library(tidyverse)

setwd("G:/Chestnut Tree Heritage Project/Projects/Carbon_Sequestration/Data/Tab")

# Calculates Carbon Sequestered per year
# H = Height
# D = DBH
# C = coeficient
carbon_sequest <- function(H, D, C){
  green_weight <- C * (D ^ 2) * H
  green_weight_roots <- green_weight * 1.2
  dry_weight <- green_weight_roots * .725
  total_carbon <- dry_weight * .5
  (total_carbon * 3.6663)
}

# Calculate Random Locations and DBH
Lat <- runif(400, 39.795402, 39.798537)
Long <- runif(400, -77.281096, -77.276645)
DBH <- round(runif(400, 1, 20), digits = 2)

# Create dataframe
chestnut <- tibble(
  Lat,
  Long,
  DBH) 

# Calculate random heights, coefficent, and carbon sequestered
chestnut_heights <- chestnut %>%
  mutate(Height = round(if_else(between(DBH, 0, 5), runif(400, 8, 10),
                          if_else(between(DBH, 5, 10), runif(400, 10, 20),
                                  if_else(between(DBH, 10, 15), runif(400, 20, 40),
                                          runif(400, 40, 60)))), digits = 2),
         coef = if_else(DBH < 11, .25, .15),
         c02_sequest = carbon_sequest(Height, DBH, coef))

# Write to csv
write.csv(chestnut_heights, "Tidy/Chestnut_Trees.csv", row.names = FALSE)
