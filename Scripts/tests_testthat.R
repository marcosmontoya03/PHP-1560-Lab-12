############################### TESTS #####################################

source("Scripts/estimation.R")
source("Scripts/simulation.R")
source("Scripts/optimization.R")

########################### Estimation Function TEST ###########################
test_bike_data <- data.frame(start_station = c("1","1","1","2","2","3"),
                             end_station = c("2","2","2","3","3","4"),
                             start_time = c("2025-11-28 08:00:00",
                                            "2025-11-28 08:10:00",
                                            "2025-11-28 08:20:00",
                                            "2025-11-28 09:00:00",
                                            "2025-11-28 09:15:00",
                                            "2025-11-28 10:00:00"),
                             end_time      = c("2025-11-28 08:05:00",
                                               "2025-11-28 08:15:00",
                                               "2025-11-28 08:25:00",
                                               "2025-11-28 09:05:00",
                                               "2025-11-28 09:20:00",
                                               "2025-11-28 10:30:00"))


# should have no average availability for 1 --> 2 
testthat::expect_equal(estimate_arrival_rates(test_bike_data)$avg_avail[1], 0)

# right number of columns (6)
testthat::expect_equal(ncol(estimate_arrival_rates(test_bike_data)), 6)


########################### Simulation Function TEST ###########################

test_arrival_rates <- data.frame(start_station = c(1, 2, 3),
                                 end_station = c(2, 3, 4),
                                 hour = c(8, 9, 10),
                                 avg_trips = c(5, 5, 5),
                                 avg_avail = c(3, 3, 3),
                                 mu_hat = c(0.5, 1, 0.5))

test_simulated_demand <- simulated_demand(test_arrival_rates, seed = 10)

# check right # columns 
testthat::expect_equal(ncol(test_simulated_demand), 3)

# check right class 
testthat::expect_s3_class(test_simulated_demand, "data.frame")


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

test_place = data.frame(station = seq(1, 24, 1), num_bikes = c(rep(0, 24)))

test_out <- happy_customers(test_sim, test_place)

testthat::expect_equal(mean(test_out[[1]]$mood), 0)

## Another Unit test for happy_customers
test_sim = data.frame(start_station = c(2, 2, 2, 1, 1, 1), 
                      end_station = c(4, 4, 4, 5, 5, 5),
                      hour = c(1, 1.1 ,1.2 ,1.3 ,1.4 ,1.5))

test_place = data.frame(station = seq(1, 24, 1), num_bikes = c(rep(1, 24)))

test_out <- happy_customers(test_sim, test_place)

testthat::expect_equal(sum(test_out[[2]]$num_bikes), 24)


######################## Optimization Function TEST #############################

test_day_sim <- data.frame(start_station = c(2, 6, 2, 1, 3, 7), 
                           end_station = c(6, 10, 10, 6, 5, 5),
                           hour = c(1, 1.1, 1.2, 1.3, 1.4, 1.5))

test_out <- optimize_placement(arrival_rates = arrival_rates,
                               tot_bikes = 5,
                               num_sims = 3,
                               testing = T,
                               day_sim_test = test_day_sim)

testthat::expect_equal(length(test_out[[3]]), 5)

testthat::expect_equal(sum(test_out[[1]]$num_bikes), 5)

testthat::expect_equal(test_out[[3]], c(2, 2, 1, 3, 7))
















