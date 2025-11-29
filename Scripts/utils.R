############################### Final Output ###################################

library(dplyr)
library(tidyverse)
library(gt)

source("Scripts/estimation.R")
source("Scripts/simulation.R")
source("Scripts/placement.R")

############################### Output Function ################################

#' Final Output Table 
#' 
#' @description This functions takes the optimization output and formats it in 
#' a legible table for key placement of bikes
#' 
#' @param final_optimization this is the output from the `optimize_placement()` 
#' function  
#' 
#' @return a gt table with fleet size, stations, and number of bikes to be placed
#' at beginning of day


final_rec <- function(final_optimization){
  
  final_rec <- final_optimization[[1]] %>% 
    filter(num_bikes > 0) %>% 
    arrange(station) %>% 
    gt() %>%
    tab_header(title = "Start each day with the following number of bikes
               at each station to maximize customers' happiness",
               subtitle = paste0("Fleet size: ",
                                 sum(final_optimization[[1]]$num_bikes ))) %>%
    cols_label(station = "Station",
               num_bikes = "Number of Bikes")

  
  return(final_rec)
  
}

# 20 bikes 
final_optimization_20 <- optimize_placement(arrival_rates, 20, 2, seed = 123)

gt_20 <- final_rec(final_optimization_20)

gtsave(gt_20, "results/final_rec_20_bikes.png")

# 50 bikes 
final_optimization_50 <- optimize_placement(arrival_rates, 50, 2, seed = 123)

gt_50 <- final_rec(final_optimization_50)

gtsave(gt_50, "results/final_rec_50_bikes.png")

# 200 bikes 

final_optimization_200 <- optimize_placement(arrival_rates, 200, 2, seed = 123)

gt_200 <- final_rec(final_optimization_200)

gtsave(gt_200, "results/final_rec_200_bikes.png")


