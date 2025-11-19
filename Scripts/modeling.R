#Thinning Notes
# only AB pair in example, need to repeat for each pair
# - find max rate in the day for mu, make that lambda max
# - arrival time iid exp(2) 
# - keep adding exponential variables till end of the day. 
# - then thin based on the mus

model_1_day <- function(df){
  lamb_max <- get_lambda_max(df)
  model <- model_use_max(lamb_max)
  model <- thinning(full_mod)
}

return(model_1_day(df_rates))