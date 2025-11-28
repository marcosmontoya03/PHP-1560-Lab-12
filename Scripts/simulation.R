############################### Simulation #####################################

library(dplyr)
library(tidyverse)

source("Scripts/estimation.R")

########################### Simulation Function ################################

#' Simulating Bike Demand 
#' 
#' @description This function simulates a day of bike data using estimated 
#' arrival rates from original bike_data 
#' 
#' @param arrival_rates estimated arrival rates using the estimate_arrival_rate
#' function defined before 
#' 
#' @param seed sets a seed
#' 
#' @return simulated data for one day of bike demand 

simulation <- function(arrival_rates, seed){
  
  set.seed(seed)
  
  all_demand <- data.frame()
  
  # get unique station pairs
  unique_pairs <- unique(arrival_rates[, c("start_station", "end_station")])
  
  # loop through each pairs 
  for(i in 1:nrow(unique_pairs)){
    
    # select the pairs 
    s <- unique_pairs$start_station[i]
    e <- unique_pairs$end_station[i]
    
    # subset arrival_rates for this pair
    pair_data <- subset(arrival_rates, start_station == s & end_station == e)
    
    # skip if no data for this pair at all
    if(nrow(pair_data) == 0) next
    
    # set up for while loop 
    arrivals <- c()
    current_time <- 0
   
    rate <- max(pair_data$mu_hat, na.rm = TRUE) 
    next_arrival <- rexp(1, rate)
    
    # while loop for 24 hour day 
    while(current_time + next_arrival < 24){
      
      # update current time 
      current_time <- current_time + next_arrival
      
      # determine the hour of this arrival
      arrival_hour <- floor(current_time)
      
      ########## thinning process ###########
      
      # subset for this hour
      hour_data <- subset(pair_data, hour == arrival_hour)
      
      # if there is no data for this hour, thinning probability = 0
      if(nrow(hour_data) == 0){
        p_thin <- 0
      } else {
        # thinning probability = mu_hat / max(mu_hat in this hour subset)
        if(rate == 0){
          stop("Rate is equal to Zero")
        }
        p_thin <- hour_data$mu_hat / max(hour_data$mu_hat, na.rm = TRUE)
      }
      
      # thinning step
      keep <- rbinom(1, 1, prob = p_thin)  # size=1 for one trial
      
      if(keep == 1){
        arrivals <- c(arrivals, current_time)
      }
      
      # next arrival (reset)
      next_arrival <- rexp(1, rate)
    }
    
    if(length(arrivals) > 0){
      demand <- data.frame(
        start_station = s,
        end_station = e,
        hour = floor(arrivals)
      )
      all_demand <- rbind(all_demand, demand)
    }
    
  }
  
  return(all_demand)
}

simulated_data <- simulation(arrival_rates, 13)
