############################### Optimization #####################################

library(dplyr)
library(tidyverse)
library(lubridate)
library(testthat)

source("Scripts/estimation.R")
source("Scripts/simulation.R")

######################## Optimization Function ################################

#' A function to optimize bike placements
#' 
#' @param arrival_rates A dataframe of each station and the estimated arrival rates
#' @param tot_bikes Number of bikes in fleet 
#' @param num_sims The total number of simulations run to measure rider happiness
#' @return A list containing the optimal bike placements for the given rates,
#' the outputed happiness list for testing purposes, and the list of unhappiest
#' stations for testing purposes

optimize_placement <- function(arrival_rates, 
                              tot_bikes, 
                              num_sims, 
                              testing = F, 
                              day_sim_test,
                              seed = NULL){
  
  #Adds functionality to input your own simulated day
  if(testing == F){
  day_sim <- simulated_demand(arrival_rates, seed)
  }
  else{
    day_sim <- day_sim_test
  }
  
  #create a baseline empty bike placement
  default_place <- data.frame(station = seq(1, 24, 1), num_bikes = rep(0, 24))
  vec_most_unhappy <- c()
  
  for(i in 1:tot_bikes){
    
    # Will create a dataframe that appends several simulated days together with
    # the added happiness measure
    output_df = data.frame()
    
    for(j in 1:num_sims){
      if(testing == F){
      day_sim <- simulated_demand(arrival_rates, seed)
      } else {
        day_sim <- day_sim_test
      } 
      
      output_df <- rbind(output_df, happy_customers(day_sim, default_place)[[1]])
    }
    
  #Finds the the unhappiest station unhappiness
  most_unhappy_station <- output_df %>% 
    group_by(start_station, hour) %>% 
    summarize(avg_happy = mean(mood), .groups = "drop") %>% 
    arrange(avg_happy, hour) %>% 
    ungroup() %>% 
    slice(1) %>% 
    pull(start_station)
  
  vec_most_unhappy <- c(vec_most_unhappy, most_unhappy_station)
  
  #Adds a bike to the unhappiest station
  default_place[default_place$station == most_unhappy_station,]$num_bikes <- 
    default_place[default_place$station == most_unhappy_station,]$num_bikes  + 1
  
  #Run the simulation again, now with the new placement
  
  }
  
  return(list(default_place, output_df, vec_most_unhappy))
}

final_optimization <- optimize_placement(arrival_rates, 10, 1)

final_optimization <- optimize_placement(arrival_rates, 100, 3)


