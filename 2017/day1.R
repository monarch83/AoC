# ----
# Day 1 - Inverse Captcha ---
# Part 1 - The captcha requires you to review a sequence of digits (your puzzle input) and
# find the sum of all digits that match the next digit in the list. The list is circular, 
# so the digit after the last digit is the first digit in the list.
# What is the solution to your captcha?

library(stringr)
library(dplyr)

# Read Puzzle Input
day1_data <- readLines("AoC/2017/data/day1_data")

day1_data <- unlist(strsplit(as.character(day1_data), split=character(0)))

#day1_data <- c("11211")
day1_data <- as.numeric(unlist(strsplit(as.character(day1_data), split=character(0))))

day1_data_lag <- c(day1_data[length(day1_data)], day1_data[1:length(day1_data)-1])

df <- data.frame(day1_data, day1_data_lag)

df <- df %>%
  mutate(match = ifelse(day1_data == day1_data_lag, day1_data, NA),
         matchfl = ifelse(day1_data == day1_data_lag, 1, 0))

# Part 2 ---- 
#day1_data <- c("12131415")
day1_data <- as.numeric(unlist(strsplit(as.character(day1_data), split=character(0))))

split1 <- day1_data[1:(length(day1_data)/2)]
split2 <- day1_data[(length(day1_data)/2+1):length(day1_data)]

sum(split1[which(split1 == split2)]*2)
