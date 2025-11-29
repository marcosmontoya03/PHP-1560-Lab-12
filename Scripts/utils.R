############################### Final Output ###################################

library(dplyr)
library(tidyverse)
library(gt)

source("Scripts/estimation.R")
source("Scripts/simulation.R")
source("Scripts/placement.R")

############################### Output Function ################################

final_rec <- function(final_optimization){
  
  final_rec <- final_optimization[[1]] %>% 
    filter(num_bikes > 0) %>% 
    arrange(desc(num_bikes)) %>% 
    gt() %>%
    tab_header(title = "Start each day with the following number of bikes
               at each station to maximize customers' happiness") %>%
    cols_label(station = "Station",
               num_bikes = "Number of Bikes")
  
  return(final_rec)
  
}

final_rec(final_optimization)

