# ----
# Day 1 
# Part 1 - what is the resulting frequency after all of the changes in frequency have been applied?

library(stringr)
library(dplyr)

# Read Puzzle Input
day1_data <- read.table(file = "AoC/2018/data/day1_data")

day1_data <- day1_data %>%
  mutate(csum = cumsum(day1_data$V1))

cat("what is the resulting frequency after all of the changes in frequency have been applied?: ",
    cumsum(day1_data$V1)[nrow(day1_data)])

# Part 2 - What is the first frequency your device reaches twice?
freq <- rep(day1_data$V1, times = 200)
freq_csum <- cumsum(freq)

start_time <- Sys.time()

for (i in 2:length(freq)) {
  #print(day1_data$csum[i] == day1_data$csum[1:(i-1)])
  if (any(freq_csum[i] == freq_csum[1:(i-1)])) { 
    cat("What is the first frequency your device reaches twice? \n", 
        "Iteration: ", ceiling(i/length(day1_data$V1)), "\n", 
        "Frequency: ", freq_csum[i], "\n")
    break
  }
  
}

end_time <- Sys.time()

end_time - start_time
