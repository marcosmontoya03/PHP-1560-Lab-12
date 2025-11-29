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
#'
#' @return A list containing the optimal bike placements for the given rates,
#' the outputed happiness list for testing purposes, and the list of unhappiest
#' stations for testing purposes

optimize_placement <- function(arrival_rates, 
                              tot_bikes, 
                              num_sims, 
                              testing = F, 
                              day_sim_test = NULL,
                              seed = NULL){
  
  #Adds functionality to input your own simulated day
  if(testing == F){
    day_sim <- simulated_demand(arrival_rates, seed)
    
  } else{
    day_sim <- day_sim_test
  }
  
  #create a baseline empty bike placement
  default_place <- data.frame(station = seq(1, 24, 1), num_bikes = rep(0, 24))
  vec_most_unhappy <- integer(tot_bikes)
  
  for(i in 1:tot_bikes){
    
    # preallocate a list to store the simulation rather than rbind
    output_list <- vector("list", num_sims)
    
    for(j in 1:num_sims){
      sim_day <- if(testing == F) {
        simulated_demand(arrival_rates, seed)
      } else {
        day_sim_test
      }
      
      output_list[[j]] <- happy_customers(sim_day, default_place)[[1]]
    }
  
  # Combine all simulations at once (fast)
  output_df <- bind_rows(output_list)
    
  #Finds the the unhappiest station unhappiness
  most_unhappy_station <- output_df %>% 
    group_by(start_station, hour) %>% 
    summarize(avg_happy = mean(mood), .groups = "drop") %>% 
    arrange(avg_happy, hour) %>% 
    slice(1) %>% 
    pull(start_station)
  
  vec_most_unhappy[i] <- most_unhappy_station
  
  #Adds a bike to the unhappiest station
  default_place[default_place$station == most_unhappy_station,]$num_bikes <- 
    default_place[default_place$station == most_unhappy_station,]$num_bikes  + 1
  
  #Run the simulation again, now with the new placement
  
  }
  
  return(list(optimized_placement = default_place, succesful_trips = output_df, 
              unhappiest_stations = vec_most_unhappy))
}

final_optimization <- optimize_placement(arrival_rates, 20, 2)

