# ----
# Day 2: Dive! ---

# Part 1 ----
# What do you get if you multiply your final horizontal position by your final depth?

library(stringr)
library(dplyr)

# Read Puzzle Input
day2_data <- read.table(file = "~/AoC/2021/data/day2_data", blank.lines.skip=F)

instructions <- day2_data$V1
units <- day2_data$V2

start_pos <- c(0,0)

for (i in 1:length(instructions)) {
  
  if (instructions[i] == "forward") { start_pos[1] = start_pos[1] + units[i] }
  else if (instructions[i] == "down") { start_pos[2] = start_pos[2] + units[i] }
  else if (instructions[i] == "up") { start_pos[2] = start_pos[2] - units[i] }
  else { cat("Different instructions then what is expected") }
  
}

cat(paste0("What do you get if you multiply your final horizontal position by your final depth?: ", start_pos[1] * start_pos[2]))

# Part 2 ----
# What do you get if you multiply your final horizontal position by your final depth?

start_pos <- c(0,0)
aim <- 0

for (i in 1:length(instructions)) {
  
  if (instructions[i] == "forward") { 
    
    start_pos[1] = start_pos[1] + units[i] 
    start_pos[2] = start_pos[2] + (aim * units[i])
    
  }
  
  else if (instructions[i] == "down") { 
    
    aim = aim + units[i] 
    
  }
  
  else if (instructions[i] == "up") { 
    
    aim = aim - units[i] 
    
  }
  
  else { 
    
    cat("Different instructions then what is expected") 
    
  }
  
}

cat(paste0("What do you get if you multiply your final horizontal position by your final depth?: ", start_pos[1] * start_pos[2]))
