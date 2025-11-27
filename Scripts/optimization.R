############################### Optimization #####################################

library(dplyr)
library(tidyverse)
library(lubridate)
library(testthat)

source("Scripts/estimation.R")
# source("Scripts/simulation.R")

######################## Optimization Function ################################


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
  for(i in 1:nrow(df_sim)){
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
  return(list(df_sim, df_place))
}



### Testing my simulation bike functions, seems to be working

## Unit Test for simulate_bikes

test_sim = data.frame(start_station = c(1,1,2,3),
                      end_station = c(2,2,3,4),
                      time = c(1,2,3,4))
test_place = data.frame(station = seq(1,24,1), num_bikes = c(rep(1,24)))

test_out <- simulate_bikes(test_sim,test_place)

testthat::expect_equal(mean(test_out[[1]]$mood),0.75)

## Another Unit Test for simulate_bikes
test_sim = data.frame(start_station = c(1,1,2,3),
                      end_station = c(2,2,3,4),
                      time = c(1,2,3,4))
test_place = data.frame(station = seq(1,24,1), num_bikes = c(rep(0,24)))
test_out <- simulate_bikes(test_sim,test_place)

testthat::expect_equal(mean(test_out[[1]]$mood),0)

## Another Unit test for simulate Bikes
test_sim = data.frame(start_station = c(2,2,2,1,1,1), 
                      end_station = c(4,4,4,5,5,5),
                      hour = c(1,1.1,1.2,1.3,1.4,1.5))
test_place = data.frame(station = seq(1,24,1), num_bikes = c(rep(1,24)))



c(1:nrow(test_sim))

test_out <- simulate_bikes(test_sim, test_place)

testthat::expect_equal(sum(test_out[[2]]$num_bikes),24)

# test_sim$mood[4] <- 1
# test_sim


# 

#' A function to optimize bike placements
#' 
#' @param df_rates A dataframe of each station and the expected rates
#' @param tot_bikes The total number of allowed bikes 
#' @param num_sims The total number of simulations run to measure rider happiness
#' @return A list containing The optimal bike placements for the given rates,
#' the outputed happiness list for testing purposes, and the list of unhappiest
#' stations for testing purposes
#' 
optimize_placement <- function(df_rates, 
                              tot_bikes, 
                              num_sims, 
                              testing = F, 
                              day_sim_test){
  
  #Adds functionality to input your own simulated day
  if(testing == F){
  day_sim <- simulate_day(df_rates)
  }
  else{
    day_sim <- day_sim_test
  }
  
  #create a baseline empty bike placement
  default_place <- data.frame(station = seq(1,24,1), num_bikes = rep(0,24))
  vec_most_unhappy <- c()
  
  for(i in 1:tot_bikes){
    
    # Will create a dataframe that appends several simulated days together with
    # the added happiness measure
    output_df = data.frame()
    
    for(j in 1:num_sims){
      if(testing == F){
      day_sim <- simulate_day(df_rates)
      } else {
        day_sim <- day_sim_test
      } 
      
      output_df <- rbind(output_df, simulate_bikes(day_sim, default_place)[[1]])
    }
    
  #Finds the the unhappiest station unhappiness
  most_unhappy_station <- output_df %>% 
    group_by(start_station,hour) %>% 
    summarize(avg_happy = mean(mood), .groups = "drop") %>% 
    arrange(avg_happy,hour) %>% 
    ungroup() %>% 
    slice(1) %>% 
    pull(start_station)
  
  vec_most_unhappy <- c(vec_most_unhappy,most_unhappy_station)
  
  #Adds a bike to the unhappiest station
  default_place[default_place$station == most_unhappy_station,]$num_bikes <- 
    default_place[default_place$station == most_unhappy_station,]$num_bikes  + 1
  
  #Run the simulation again, now with the new placement
  
  }
  
  return(list(default_place,output_df,vec_most_unhappy))
}


### Testing the optimize placement function

## Unit Testing 

# day_1 <- simulate(df_rates)

test_day_sim <- data.frame(start_station = c(2,6,2,1,3,7), 
                           end_station = c(6,10,10,6,5,5),
                           hour = c(1,1.1,1.2,1.3,1.4,1.5))
test_out <- optimize_placement(df_rates = arrival_rates,
                   tot_bikes = 5,
                   num_sims = 3,
                   testing = T,
                   day_sim_test = test_day_sim)

testthat::expect_equal(length(test_out[[3]]),5)
testthat::expect_equal(sum(test_out[[1]]$num_bikes),5)
testthat::expect_equal(test_out[[3]],c(2,2,1,3,7))



## Misc Testing

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


