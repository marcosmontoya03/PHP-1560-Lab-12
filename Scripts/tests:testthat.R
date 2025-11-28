############################### TESTS #####################################

source("Scripts/estimation.R")
source("Scripts/simulation.R")
source("Scripts/optimization.R")

########################### Estimation Function TEST ###########################

TBD 



########################### Simulation Function TEST ###########################
TBD 


########################### Happy Customers Function TEST #####################

## Unit Test for happy_customers

test_sim = data.frame(start_station = c(1, 1, 2, 3),
                      end_station = c(2, 2, 3, 4),
                      time = c(1, 2, 3, 4))
test_place = data.frame(station = seq(1, 24, 1), num_bikes = c(rep(1, 24)))

test_out <- happy_customers(test_sim, test_place)

testthat::expect_equal(mean(test_out[[1]]$mood), 0.75)

## Another Unit Test for happy_customers
test_sim = data.frame(start_station = c(1, 1, 2, 3),
                      end_station = c(2, 2, 3, 4),
                      time = c(1, 2, 3, 4))
test_place = data.frame(station = seq(1, 24, 1), num_bikes = c(rep(0,24)))
test_out <- happy_customers(test_sim, test_place)

testthat::expect_equal(mean(test_out[[1]]$mood), 0)

## Another Unit test for happy_customers
test_sim = data.frame(start_station = c(2,2,2,1,1,1), 
                      end_station = c(4,4,4,5,5,5),
                      hour = c(1,1.1,1.2,1.3,1.4,1.5))
test_place = data.frame(station = seq(1,24,1), num_bikes = c(rep(1,24)))

test_out <- happy_customers(test_sim, test_place)

testthat::expect_equal(sum(test_out[[2]]$num_bikes),24)



