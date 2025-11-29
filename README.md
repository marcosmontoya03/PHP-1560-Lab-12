---
editor_options: 
  markdown: 
    wrap: 72
---

####################### LAB 12 BY GAVIN AND MARCOS

# PURPOSE

These functions allow managers to input observed bike data and then
optimize the number of bikes placed at each station at the beginning on
the day.

# HOW TO USE

To used these functions, simply insert your bike data into the
estimation function, and then follow the rest of the functions using the
output provided in each script. Specifically, use this order:

1.  Input raw bike data in `estimate_arrival_rates()` and save results
    to `arrival_rates`

2.  Load `arrival_rates` into `optimize_placement()`, specifying fleet
    size and number of simulations desired. This automatically calls the
    `simulate_demand()` function. Save this output as
    `final_optimization`.

3.  Run `happy_customers()` to estimate the numbers of successful trips
    based on bike placement.

4.  Run `final_rec()` to get a nice table of where to place bikes at the
    beginning of the day.

# SCRIPT OVERVIEW

1.  `estimation.R`
    1.  `estimate_arrival_rates()` estimates how many trips will occur
        per hour in each pair of stations
2.  `simulation.R`
    1.  `simulate_demand()` simulates bike demand on a given day
    2.  `happy_customers()` calculates the number of trips demanded that
        were successful (i.e. trip could take place)
3.  `placement.R`
    1.  `optimize_placement()` loops through various starting
        assortments and calculates the optimal bike placement
4.  `utils.R`
    1.  `final_rec()` creates a table the displays the final
        recommendations for bike placement
5.  `tests_testthat.R`
    1.  Test for each function

# OUTPUT

You will receive a table with the fleet size, the stations to populate,
and the number of bikes to be delivered to each station.
