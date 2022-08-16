# Day 18 AoC 2015
# Like a GIF For Your Yard ----
# A # means "on", and a . means "off".
# Each light's next state (either on or off) depends on its current state and the current states of the eight 
# lights adjacent to it (including diagonals). Lights on the edge of the grid might have fewer than eight neighbors; 
# the missing ones always count as "off".

library(dplyr)
library(stringr)
library(gtools)

day18_data <- readLines("~/AoC/2015/data/day18_data")

# Example Data
# day18_data <- c(".#.#.#","...##.","#....#","..#...","#.#..#","####..")

day18_data <-  strsplit(day18_data, split=character(0))

day18_mtr <- do.call(rbind, day18_data)

# # = On . = Off
day18_mtr[day18_mtr == "#"] = as.numeric(1)
day18_mtr[day18_mtr == "."] = as.numeric(0)

day18_mtr <- apply(day18_mtr, 2 ,as.numeric)

colnames(day18_mtr) <- NULL
rownames(day18_mtr) <- NULL

day18_mtr_upd <- matrix(NA,nrow(day18_mtr),ncol(day18_mtr))

max_row <- nrow(day18_mtr)
max_col <- ncol(day18_mtr)

# Part 1 ----
# In your grid of 100x100 lights, given your initial configuration, how many lights are on after 100 steps?

for (steps in 1:100) {
  for (row in 1:(nrow(day18_mtr))) {
    for (col in 1:(ncol(day18_mtr))) {
      
      temp <- day18_mtr[row,col]
      
      day18_mtr[row,col] <- NA
      
      # Sum of Neighbours 
      sum_nbrs <- sum(day18_mtr[seq(max(row-1, 1), min(row+1, max_row)), seq(max(col-1,1), min(col+1, max_col))], na.rm=TRUE)
      
      # The light is On
      if  (temp == 1) {
        if (sum_nbrs %in% c(2,3)) {
          
          day18_mtr_upd[row,col] = 1
          
        }
        else day18_mtr_upd[row,col] = 0
      }
      
      # The Light is Off
      else if (temp == 0) {
        if (sum_nbrs == 3) {
          
          day18_mtr_upd[row,col] = 1
          
        }
        else day18_mtr_upd[row,col] = 0
      }
      
      day18_mtr[row,col] <- temp
      
    }
    
  }
  
  # print(day18_mtr_upd)
  day18_mtr <- day18_mtr_upd
  
}

cat("In your grid of 100x100 lights, given your initial configuration, how many lights are on after 100 steps? ", 
    sum(day18_mtr[1:row, 1:col], na.rm=TRUE))

# Part 2 ----
# 4 lights on the corner are stuck on, cant be turned off
# In your grid of 100x100 lights, given your initial configuration, but with the 
# four corners always in the on state, how many lights are on after 100 steps?

# Make 4 corner lights always on
day18_mtr2 <- day18_mtr

day18_mtr2[1,1] = 1
day18_mtr2[1,max_col] = 1
day18_mtr2[max_row,max_col] = 1
day18_mtr2[max_row,1] = 1

day18_mtr_upd <- matrix(NA,nrow(day18_mtr2),ncol(day18_mtr2))

max_row <- nrow(day18_mtr2)
max_col <- ncol(day18_mtr2)

for (steps in 1:100) {
  for (row in 1:(nrow(day18_mtr2))) {
    for (col in 1:(ncol(day18_mtr2))) {
      
      temp <- day18_mtr2[row,col]
      
      day18_mtr2[row,col] <- NA
      
      # Sum of Neighbours 
      sum_nbrs <- sum(day18_mtr2[seq(max(row-1, 1), min(row+1, max_row)), seq(max(col-1,1), min(col+1, max_col))], na.rm=TRUE)
      
      # The light is On
      if  (temp == 1) {
        if (sum_nbrs %in% c(2,3)) {
          
          day18_mtr_upd[row,col] = 1
          
        }
        else day18_mtr_upd[row,col] = 0
      }
      
      # The Light is Off
      else if (temp == 0) {
        if (sum_nbrs == 3) {
          
          day18_mtr_upd[row,col] = 1
          
        }
        else day18_mtr_upd[row,col] = 0
      }
      
      day18_mtr2[row,col] <- temp
      
      # Reset corner to always on
      day18_mtr_upd[1,1] = 1
      day18_mtr_upd[1,max_col] = 1
      day18_mtr_upd[max_row,max_col] = 1
      day18_mtr_upd[max_row,1] = 1
      
    }
    
  }
  
  # print(day18_mtr_upd)
  day18_mtr2 <- day18_mtr_upd
  
}

cat("In your grid of 100x100 lights, given your initial configuration and four corner always on state, how many lights are on after 100 steps? ", 
    sum(day18_mtr2[1:row, 1:col], na.rm=TRUE))
