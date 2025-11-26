#Thinning Notes
# only AB pair in example, need to repeat for each pair
# - find max rate in the day for mu, make that lambda max
# - arrival time iid exp(2) 
# - keep adding exponential variables till end of the day. 
# - then thin based on the mus


source("estimation.R")

simulation <- function(df, seed = 123){
  
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
        p_thin <- hour_data$mu_hat / max(hour_data$mu_hat, na.rm = TRUE)
      }
      
      # thinning step
      keep <- rbinom(1, 1, prob = p_thin)  # size=1 → one trial
      
      if(keep == 1){
        arrivals <- c(arrivals, current_time)
      }
      
      # next interarrival
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




tests <- simulation(arrival_rates)
