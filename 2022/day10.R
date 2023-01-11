# Day 10: Cathode-Ray Tube ---

library(stringr)
library(dplyr)

# Read Puzzle Input
day10_data <- readLines("~/AoC/2022/data/day10_data")

cycle <- c(1)

for (i in 1:length(day10_data)) {
  
  if (str_detect(day10_data[i], "noop")) {
    cycle <- c(cycle, 0)
  }
  else if (str_detect(day10_data[i], "addx")) {
    cycle <- c(cycle, 0, as.numeric(str_extract(day10_data[i], "[-0-9]+")))
  }
  
  print(cycle)
  
}

cycle_cumsum <- cumsum(cycle)

sig_str <- (cycle_cumsum[20] * 20) + (cycle_cumsum[60] * 60) + (cycle_cumsum[100] * 100) +
           (cycle_cumsum[140] * 140) + (cycle_cumsum[180] * 180) + (cycle_cumsum[220] * 220)

cat("Find the signal strength during the 20th, 60th, 100th, 140th, 180th, and 220th cycles. 
    What is the sum of these six signal strengths? ", sig_str)

# Part 2 ----

cycle <- vector()
# Initial Position of Sprite
sprite <- 1
curr_cycle <- 1

# day10_data <- c("addx 15",
#                 "addx -11",
#                 "addx 6",
#                 "addx -3",
#                 "addx 5",
#                 "addx -1",
#                 "addx -8",
#                 "addx 13",
#                 "addx 4",
#                 "noop",
#                 "addx -1",
#                 "addx 5",
#                 "addx -1")

for (i in 1:length(day10_data)) {

  if (str_detect(day10_data[i], "noop")) {
    if (sprite == curr_cycle%%40 | sprite + 1 == curr_cycle%%40 | sprite + 2 == curr_cycle%%40) {
       cycle <- c(cycle, 1)
    }
    else {
      cycle = c(cycle, 0)
    }
    curr_cycle <- curr_cycle + 1
  }
  else if (str_detect(day10_data[i], "addx")) {
    if (sprite == curr_cycle%%40 | sprite + 1 == curr_cycle%%40 | sprite + 2 == curr_cycle%%40) {
      cycle <- c(cycle, 1)
      curr_cycle <- curr_cycle + 1
      
      if (sprite == curr_cycle%%40 | sprite + 1 == curr_cycle%%40 | sprite + 2 == curr_cycle%%40) {
        cycle <- c(cycle, 1)
        curr_cycle <- curr_cycle + 1
      }
      else if (!(sprite == curr_cycle%%40 & sprite + 1 == curr_cycle%%40 & sprite + 2 == curr_cycle%%40)) {
        cycle <- c(cycle, 0)
        curr_cycle <- curr_cycle + 1
      }
    }
    else if (!(sprite == curr_cycle%%40 & sprite + 1 == curr_cycle%%40 & sprite + 2 == curr_cycle%%40)) {
      cycle <- c(cycle, 0)
      curr_cycle <- curr_cycle + 1
      
      if (sprite == curr_cycle%%40 | sprite + 1 == curr_cycle | sprite + 2 == curr_cycle%%40) {
        cycle <- c(cycle, 1)
        curr_cycle <- curr_cycle + 1
      }
      else if (!(sprite == curr_cycle%%40 & sprite + 1 == curr_cycle%%40 & sprite + 2 == curr_cycle%%40)) {
        cycle <- c(cycle, 0)
        curr_cycle <- curr_cycle + 1
      }
    }
    
    sprite <- sprite +  as.numeric(str_extract(day10_data[i], "[-0-9]+"))
    
  }
}

cycle <- replace(cycle,cycle==1, "#")
cycle <- replace(cycle,cycle==0, ".")

cat("", cycle[1:40], "\n", cycle[41:80], "\n", cycle[81:120], "\n", cycle[121:160], "\n", cycle[161:200], "\n", cycle[201:240])

