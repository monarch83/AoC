# AoC 2015 Day 20 ----
# Infinite Elves and Infinite Houses ----

input <- 29000000

# What is the lowest house number of the house to get at least as many presents as the number in your puzzle input?

house <- 0

start_time <- Sys.time()

repeat {
    
  house <- 1 + house
    
  # presents <- sum(which(house%%seq_along(1:house) == 0)*10)
    
  if (sum(DescTools::Divisors(house)[[1]] * 11) >= input) break
  # print(house)
    
}

end_time <- Sys.time()

end_time - start_time

cat("What is the lowest house number of the house to get at least as many presents as the number in your puzzle input? ",
    house)

# Function with SAPPLY took almost 2 hours, and it stopped
# Time difference of 1.826727 hours
# Answer ==> 5799994

# Part 2 ----
# The Elves decide they don't want to visit an infinite number of houses. 
# Instead, each Elf will stop after delivering presents to 50 houses. 
# To make up for it, they decide to deliver presents equal to eleven times their number at each house.

# With these changes, what is the new lowest house number of the house to get at least as many presents 
# as the number in your puzzle input?

house <- 0

counter <- rep(0, input)

start_time <- Sys.time()

repeat {
  
  house <- 1 + house
  
  div <- DescTools::Divisors(house)[[1]]
  
  counter[div] <- counter[div] + 1
  
  if ((sum((counter[div] <= 50) * div) * 11) >= input) break
  # print(house)
  
}

end_time <- Sys.time()

end_time - start_time

cat("What is the lowest house number of the house to get at least as many presents as the number in your puzzle input? ",
    house)

