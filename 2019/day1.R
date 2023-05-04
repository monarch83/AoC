# ----
# Day 1 - The Tyranny of the Rocket Equation ---

library(stringr)
library(dplyr)

# Part 1 ----
# Fuel required to launch a given module is based on its mass. 
#  Specifically, to find the fuel required for a module, 
#  take its mass, divide by three, round down, and subtract 2.
# Read Puzzle Input
# What is the sum of the fuel requirements for all of the modules on your spacecraft?

day1_data <- readLines("AoC/2019/data/day1_data")

sum(floor(as.numeric(day1_data)/3) - 2)

# Part 2 ---
# So, for each module mass, calculate its fuel and add it to the total.
#  Then, treat the fuel amount you just calculated as the input mass 
#  and repeat the process, continuing until a fuel requirement is zero 
#  or negative.

day1_data2 <- rep(NA, length(day1_data))

for (i in 1:length(day1_data)) {
  
  input <- as.numeric(day1_data[i])
  temp_vec <- c()
  
  while(floor(input/3) > 0) {
    input <- floor(input/3) - 2
    if (input > 0) {
      temp_vec <- c(temp_vec, input)
    }
    #print(temp_vec)
  }
  day1_data2[i] <- sum(temp_vec)
  input <- NA
  
}

sum(day1_data2)
