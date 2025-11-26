############################### Optimization #####################################

library(dplyr)
library(tidyverse)
library(lubridate)

source("estimation.R")
source("modeling.R")



#' Function to test the number of happy customers
#' 
#' @param df_sim A data frame with time, starting station and ending station 
#' @param placement A starting placement of the bikes, should be a dataframe of
#' stations and the number of bikes in them
#' @return A list containing the df_sim dataset, with an added mood column where 1 means the ride
#' was fulfilled and 0 means that ride was not fulfilled, and the inputed placements
#' 
simulate_bikes <- function(df_sim, df_place){
  
  #Makes a empty placement of bikes if no placement is inputted
  if(is.null(df_place)){
    df_place <- data.frame(station = seq(1,24,1), num_bikes = rep(0,24))
  }
  
  #Loops through the simulated trips dataframe, moving a bike if one is available,
  # and creating a happiness output
  for(i in 1:length(df_sim)){
    start <- df_sim$start_station[i]
    end <- df_sim$end_station[i]
    
    
    if(df_place[df_place$station == start,]$num_bikes >= 1){
      df_place[df_place$station == start,]$num_bikes <- 
        df_place[df_place$station == start,]$num_bikes - 1
      
      df_place[df_place$station == end,]$num_bikes <- 
        df_place[df_place$station == end,]$num_bikes + 1
      
      df_sim$mood[i] <- 1
    }
    else
      {
        df_sim$mood[i] <- 0
    }
  }
  return(df_sim)
}

### Testing my simulation bike functions, seems to be working

test_sim = data.frame(start_station = c(1,1,2,3),
                      end_station = c(2,2,3,4),
                      time = c(1,2,3,4))
test_place = data.frame(station = seq(1,24,1), num_bikes = c(rep(1,24)))

test_sim$start_station[1]
test_sim[1]
test_place[test_place$station == 23,]$num_bikes


test_out <- simulate_bikes(test_sim,test_place)




# 

#' A function to optimize bike placements
#' 
#' @param df_rates A dataframe of each station and the expected rates
#' @param tot_bikes The total number of allowed bikes 
#' @param num_sims The total number of simulations run to measure rider happiness
#' @return The optimal bike placements for the given rates.
#' 
optimze_placement <- function(df_rates, tot_bikes, num_sims){
  

  day_sim <- simulate_day(df_rates)
  
  #create a baseline empty bike placement
  default_place <- data.frame(station = seq(1,24,1), num_bikes = rep(0,24))
  
  for(i in 1:tot_bikes){
    
    # Will create a dataframe that appends several simulated days together with
    # the added happiness measure
    output_df = NULL
    
    for(j in 1:num_sims){
      day_sim <- simulate_day(df_rates)
       output_df <- rbind(output_df, simulate_bikes(day_sim, default_place))
    }
    
  #Finds the the unhappiest station unhappiness
  most_unhappy_station <- output_df %>% 
    group_by(start_station) %>% 
    summarize(avg_happy = mean(mood)) %>% 
    arrange(avg_happy) %>% 
    slice(1) %>% 
    pull(start_station)
  
  #Adds a bike to the unhappiest station
  default_place[default_place$station == most_unhappy_station,]$num_bikes <- 
    default_place[default_place$station == most_unhappy_station,]$num_bikes  + 1
  
  #Run the simulation again, now with the new placement
  
  }
  
  return(default_place)
}


### Testing the optimize placement function
# most_unhappy_station <- test_out %>% 
#   group_by(start_station) %>% 
#   summarize(avg_happy = mean(mood)) %>% 
#   arrange(avg_happy) %>% 
#   slice(1) %>% 
#   pull(start_station)
# 
# test_place[test_place$station == 13,]$num_bikes <- 
#   test_place[test_place$station == 13,]$num_bikes  + 1
# 
# output_df = NULL
# 
# for(j in 1:10){
#   day_sim <- simulate_day(df_rates)
#   output_df <- rbind(output_df, test_out)
# }


