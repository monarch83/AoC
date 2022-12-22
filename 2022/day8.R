# ----
# Day 8: Treetop Tree House ---

library(stringr)
library(dplyr)

# Read Puzzle Input
day8_data <- readLines("~/AoC/2022/data/day8_data")

# Test Data
# day8_data <- c("30373", "25512", "65332", "33549", "35390")

day8_mtr <- matrix(NA, length(day8_data), nchar(day8_data)[1])

for (i in 1:length(day8_data)) {
  
  day8_mtr[i, ] <- as.numeric(unlist(strsplit(day8_data[i], "")))
  
}

# Part 1 ----
# Find visible trees
day8_part1 <- matrix(FALSE, length(day8_data), nchar(day8_data)[1])

for (i in 1:nrow(day8_mtr)) {
  for (j in 1:ncol(day8_mtr)) {
    
    # Edge are always visible
    if (i == 1) {
      day8_part1[1, ] <- TRUE
    }
    else if (j == 1) {
      day8_part1[, 1] <- TRUE
    }
    else if (i == nrow(day8_mtr)) {
      day8_part1[nrow(day8_mtr), ] <- TRUE
    }
    else if (j == nrow(day8_mtr)) {
      day8_part1[, nrow(day8_mtr)] <- TRUE
    }
    else {
      day8_part1[i,j] <- any(
                          # right
                          all(day8_mtr[i, (j+1):nrow(day8_mtr)] < day8_mtr[i, j]) | 
                          # down
                          all(day8_mtr[(i+1):nrow(day8_mtr), j] < day8_mtr[i, j]) |
                          # left
                          all(day8_mtr[i, 1:(j-1)] < day8_mtr[i, j]) |
                          # up
                          all(day8_mtr[1:(i-1), j] < day8_mtr[i, j])
                          )
    }
  }
}

cat("how many trees are visible from outside the grid?:", sum(day8_part1))

# Part 2 ----
# A tree's scenic score is found by multiplying together its viewing distance in each of the four directions.
day8_part2 <- matrix(FALSE, length(day8_data), nchar(day8_data)[1])

for (i in 1:nrow(day8_mtr)) {
  for (j in 1:ncol(day8_mtr)) {
    
    # right
    if (all(day8_mtr[i, min(j+1, nrow(day8_mtr)):nrow(day8_mtr)] < day8_mtr[i, j])) {
      right <- sum(day8_mtr[i, min(j+1, nrow(day8_mtr)):nrow(day8_mtr)] < day8_mtr[i, j])
    } else {
      right <- which(!day8_mtr[i, min(j+1, nrow(day8_mtr)):nrow(day8_mtr)] < day8_mtr[i, j])[1]
    }
    
    # down
    if (all(day8_mtr[min(i+1, nrow(day8_mtr)):nrow(day8_mtr), j] < day8_mtr[i, j])) {
      down <- sum(day8_mtr[min(i+1, nrow(day8_mtr)):nrow(day8_mtr), j] < day8_mtr[i, j])
    } else {
      down <- which(!day8_mtr[min(i+1, nrow(day8_mtr)):nrow(day8_mtr), j] < day8_mtr[i, j])[1]
    }
    
    # left
    if (all(day8_mtr[i, 1:(j-1)] < day8_mtr[i, j] )) {
      left <- sum(day8_mtr[i, 1:(j-1)] < day8_mtr[i, j])
    } else {
      left <- which(!rev(day8_mtr[i, 1:(j-1)]) < day8_mtr[i, j])[1]
    }
    
    # up
    if (all(day8_mtr[1:(i-1), j] < day8_mtr[i, j])) {
      up <- sum(day8_mtr[1:(i-1), j] < day8_mtr[i, j])
    } else {
      up <- which(!rev(day8_mtr[1:(i-1), j]) < day8_mtr[i, j])[1]
    }
    
    day8_part2[i,j] <- 
          right * down * left * up
    
  }
}

cat("What is the highest scenic score possible for any tree?:", 
    max(day8_part2[2:98, 2:98]),
    "The highest scenic score possible tree is ", 
    which(day8_part2 == max(day8_part2[2:98, 2:98])))
