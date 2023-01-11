# ----
# Day 9: Rope Bridge ---

library(stringr)
library(dplyr)

# Read Puzzle Input
day9_data <- readLines("~/AoC/2022/data/day9_data")

# Test Data
# day9_data <- c("R 4" ,"U 4" ,"L 3" ,"D 1" ,"R 4" ,"D 1" ,"L 5" ,"R 2")
day9_dir <- str_extract(day9_data, "[A-Z]")
day9_steps <- as.numeric(str_extract(day9_data, "[0-9]+"))

# PART 1 ---
head.cord <- c(0,0)
tail.cord <- c(0,0)

all.tail.cord <- c(0,0)

for (i in 1:length(day9_dir)) {
  
  for (j in 1:day9_steps[i]) {
    
      # Save Prior Head Coordinates
      head.cord.pr <- head.cord
      
      # Increment head based on direction
      if (day9_dir[i] == "R") {
        head.cord <- c(head.cord[1] + 1, head.cord[2])
      } 
      else if (day9_dir[i] == "U") {
        head.cord <- c(head.cord[1], head.cord[2] + 1)
      }
      else if (day9_dir[i] == "L") {
        head.cord <- c(head.cord[1] - 1, head.cord[2])
      } 
      else if (day9_dir[i] == "D") {
        head.cord <- c(head.cord[1], head.cord[2] - 1)
      }  
      
      # Update tail based on head position 
      if (any(abs(head.cord - tail.cord) > 1)) {
        tail.cord <- head.cord.pr
      }
      
      # Save the tail cordinates
      all.tail.cord <- rbind(all.tail.cord, tail.cord)
      # print(all.tail.cord)
  }
  
}

cat("How many positions does the tail of the rope visit at least once?", nrow(unique(all.tail.cord)))


# Part 2 ----
# Test data
day9_data <- c("R 5","U 8","L 8","D 3","R 17","D 10","L 25","U 20")
day9_data <- day9_data[1:3]
day9_dir <- str_extract(day9_data, "[A-Z]")
day9_steps <- as.numeric(str_extract(day9_data, "[0-9]+"))

head.cord <- c(0,0)
cord.1 <- c(0,0)
cord.2 <- c(0,0)
cord.3 <- c(0,0)
cord.4 <- c(0,0)
cord.5 <- c(0,0)
cord.6 <- c(0,0)
cord.7 <- c(0,0)
cord.8 <- c(0,0)
cord.9 <- c(0,0)


all.tail.cord <- c(0,0)

for (i in 1:length(day9_dir)) {
  cat(day9_data[i], "\n")
  
  for (j in 1:day9_steps[i]) {
    
    # Save Prior Coordinates
    head.cord.pr <- head.cord
    
    # Increment head based on direction
    if (day9_dir[i] == "R") {
      head.cord <- c(head.cord[1] + 1, head.cord[2])
    } 
    else if (day9_dir[i] == "U") {
      head.cord <- c(head.cord[1], head.cord[2] + 1)
    }
    else if (day9_dir[i] == "L") {
      head.cord <- c(head.cord[1] - 1, head.cord[2])
    } 
    else if (day9_dir[i] == "D") {
      head.cord <- c(head.cord[1], head.cord[2] - 1)
    }  
    
    if (any(abs(head.cord - cord.1) > 1)) {
      cord.1 <- head.cord.pr
      
      # Up
      if ((cord.1 - cord.2)[2] == 2) { cord.2 <- c(cord.2[1]+1, cord.2[2]+1) }
      if ((cord.2 - cord.3)[2] == 2) { cord.3 <- c(cord.3[1]+1, cord.3[2]+1) }
      if ((cord.3 - cord.4)[2] == 2) { cord.4 <- c(cord.4[1]+1, cord.4[2]+1) }
      if ((cord.4 - cord.5)[2] == 2) { cord.5 <- c(cord.5[1]+1, cord.5[2]+1) }
      if ((cord.5 - cord.6)[2] == 2) { cord.6 <- c(cord.6[1]+1, cord.6[2]+1) }
      if ((cord.6 - cord.7)[2] == 2) { cord.7 <- c(cord.7[1]+1, cord.7[2]+1) }
      if ((cord.7 - cord.8)[2] == 2) { cord.8 <- c(cord.8[1]+1, cord.8[2]+1) }
      if ((cord.8 - cord.9)[2] == 2) { cord.9 <- c(cord.9[1]+1, cord.9[2]+1) }
      
      # Down
      if ((cord.1 - cord.2)[2] == -2) { cord.2 <- c(cord.2[1]-1, cord.2[2]-1) }
      if ((cord.2 - cord.3)[2] == -2) { cord.3 <- c(cord.3[1]-1, cord.3[2]-1) }
      if ((cord.3 - cord.4)[2] == -2) { cord.4 <- c(cord.4[1]-1, cord.4[2]-1) }
      if ((cord.4 - cord.5)[2] == -2) { cord.5 <- c(cord.5[1]-1, cord.5[2]-1) }
      if ((cord.5 - cord.6)[2] == -2) { cord.6 <- c(cord.6[1]-1, cord.6[2]-1) }
      if ((cord.6 - cord.7)[2] == -2) { cord.7 <- c(cord.7[1]-1, cord.7[2]-1) }
      if ((cord.7 - cord.8)[2] == -2) { cord.8 <- c(cord.8[1]-1, cord.8[2]-1) }
      if ((cord.8 - cord.9)[2] == -2) { cord.9 <- c(cord.9[1]-1, cord.9[2]-1) }
      
    }
      
      print(paste0("Iteration:", j))
      print(head.cord)
      print(cord.1)
      print(cord.2)
      print(cord.3)
      print(cord.4)
      print(cord.5)
      print(cord.6)
      print(cord.7)
      print(cord.8)
      print(cord.9)
    
    # Save the tail cordinates
    all.tail.cord <- rbind(all.tail.cord, cord.5)
    #print(all.tail.cord)
  }
  
}

cat("How many positions does the tail of the rope visit at least once?", nrow(unique(all.tail.cord)))

