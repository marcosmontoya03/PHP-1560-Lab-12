############################### Optimization #####################################

library(dplyr)
library(tidyverse)
library(lubridate)

optimize <- function(num_bikes, df_rates){
  start_place <- #Make this shit random
  happy_score <- c()
    
  for(i in 1:5){  
  modeling_output <- source("modeling.R") #See the other script for this function
  
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

