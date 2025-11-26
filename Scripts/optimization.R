############################### Optimization #####################################

library(dplyr)
library(tidyverse)
library(lubridate)

source("estimation.R")
source("modeling.R")

simulate_bikes(df_simulation)

#' Function to test the number of happy customers
#' 
#' @param df_sim A data frame with time, starting station and ending station 
#' @param placement A starting placement of the bikes, should be a dataframe of
#' stations and the number of bikes in them
simulate_bikes <- function(df_sim, df_place){
  
  #Makes a empty placement of bikes if no placement is inputted
  if(is.null(df_place)){
    df_place <- data.frame(station = seq(1,24,1), bikes = rep(0,24))
  }
  
  #Loops through the simulated trips dataframe, moving a bike if one is available,
  # and creating a happiness output
  for(i in 1:length(df_sim)){
    start <- df_sim$start_station[i]
    end <- df_sim$end_station[i]
    
    
    if(df_place[start]$num_bikes >= 1){
      df_place[start]$num_bikes <-  df_place[start]$num_bikes - 1
      df_place[end]$num_bikes <- df_place[end]$num_bikes + 1
      df_sim[i]$feelings <- "happy"
    }
    else{
      df_sim[i]$feelings <- "unhappy"
    }
  }
}

# A function to simulate several days, and using simulate_bikes each time
# where an input is the number of days you want to simulate





optimize <- function(num_bikes, df_rates){
  start_place <- #Make this random
  happy_score <- c()
    
  for(i in 1:5){  
  modeling_output <-  #See the other script for this function
  
  #' Will take in the starting bike placement and compare it with the modeled bike
  #' usage
  happy_results <- happy_test(start_place, modeling_output)  
  happy_score <- c(happy_score,mean(happy_results$happiness))
  
  #'Will take in a dataframe of each station pairing and how many bikes and how 
  #'many happy people, and then will create a new placement
  start_place <- adjust_place(happy_results)
  
  #'Then we can loop through this an arbitrary number of times, and break out 
  #'earlier if the happiness scores are not improving
  if(i >= 2 & (abs(happy_score[i] - happy_score[i-1]) <= 0.05)
    {
       break
  }
  }
  return(list[[happy_results, start_place]])
}

