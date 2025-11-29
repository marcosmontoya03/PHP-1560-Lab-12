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

simulated_demand <- function(arrival_rates, seed = NULL){
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
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
    
    # set up while loop 
    arrivals <- c()
    current_time <- 0
    
    # skip if all mu_hat are NA
    if(all(is.na(pair_data$mu_hat))){
      next
    }
    
    # calculate rate 
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
        p_thin <- hour_data$mu_hat / rate
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

simulated_data <- simulated_demand(arrival_rates)



############################## successful trips ###############################


#' Function to test the number of happy customers
#' 
#' @param simulated_data A simulated data frame with time, starting station, and ending station 
#' @param df_place A starting placement of the bikes, should be a dataframe of
#' stations and the number of bikes in them
#' 
#' @return A list containing the simulated_data dataset, with an added mood column where 1 means the ride
#' was fulfilled and 0 means that ride was not fulfilled, and the inputed placements

happy_customers <- function(simulated_data, df_place = NULL){
  
  #Makes a empty placement of bikes if no placement is inputted
  if(is.null(df_place)){
    df_place <- data.frame(station = seq(1,24,1), num_bikes = rep(1, 24))
  }
  
  #Loops through the simulated trips dataframe, moving a bike if one is available,
  # and creating a happiness output
  for(i in 1:nrow(simulated_data)){
    start <- simulated_data$start_station[i]
    end <- simulated_data$end_station[i]
    
    
    if(df_place[df_place$station == start,]$num_bikes >= 1){
      df_place[df_place$station == start,]$num_bikes <- 
        df_place[df_place$station == start,]$num_bikes - 1
      
      df_place[df_place$station == end,]$num_bikes <- 
        df_place[df_place$station == end,]$num_bikes + 1
      
      simulated_data$mood[i] <- 1
    }
    else
    {
      simulated_data$mood[i] <- 0
    }
  }
  return(list(simulated_data, df_place))
}

simulated_trips <- happy_customers(simulated_data)









