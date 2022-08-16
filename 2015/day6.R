# Day 6 AoC 

library(dplyr)
library(stringr)
library(readr)

day6_data <- readLines("~/AoC/2015/data/day6_data")

# day6_data <- "turn on 0,0 through 999,999"
# day6_data <- "toggle 0,0 through 999,0"

instruction <- rep(NA, length(day6_data))
x1 <- rep(NA, length(day6_data))
y1 <- rep(NA, length(day6_data))
x2 <- rep(NA, length(day6_data))
y2 <- rep(NA, length(day6_data))

for (i in 1:length(day6_data)) {
  
  instruction[i] <- sub("[0-9].*", "", day6_data[i])
  x1[i] <- stringr::str_extract_all(day6_data[i], "[0-9]+", simplify = TRUE)[,1]
  y1[i] <- stringr::str_extract_all(day6_data[i], "[0-9]+", simplify = TRUE)[,2]
  x2[i] <- stringr::str_extract_all(day6_data[i], "[0-9]+", simplify = TRUE)[,3]
  y2[i] <- stringr::str_extract_all(day6_data[i], "[0-9]+", simplify = TRUE)[,4]
  
}

grid_inst <- data.frame(cbind(day6_data, instruction, x1, y1, x2, y2))
grid_inst <- grid_inst %>%
  mutate(x1 = as.numeric(as.character(x1)) + 1,
         x2 = as.numeric(as.character(x2)) + 1,
         y1 = as.numeric(as.character(y1)) + 1,
         y2 = as.numeric(as.character(y2)) + 1,
         instruction = trimws(instruction)
  )

# Initiate 1000x1000 Grid with All Lights off
x  <- matrix(0, 1000, 1000)

# Par 1 ----
# Lights in your grid are numbered from 0 to 999 in each direction; the lights at each corner are at 0,0, 0,999, 999,999, and 999,0. 
# The instructions include whether to turn on, turn off, or toggle various inclusive ranges given as coordinate pairs. Each coordinate 
# pair represents opposite corners of a rectangle, inclusive; a coordinate pair like 0,0 through 2,2 therefore refers to 9 lights in a 3x3 square. 
# The lights all start turned off.

# To defeat your neighbors this year, all you have to do is set up your lights by doing the instructions Santa sent you in order.

# how many lights are lit?

for (i in 1:length(day6_data)) {
 
  # Go through Instructions 1 by 1
  x1 <- grid_inst[i, "x1"]
  y1 <- grid_inst[i, "y1"]
  x2 <- grid_inst[i, "x2"]
  y2 <- grid_inst[i, "y2"]
  
  # if instruction is turn off then turn off the lights
  if (grid_inst[i,"instruction"] == "turn off") {
    x[x1:x2, y1:y2] = 0
  }
  
  # if instruction is turn on then turn on the lights 
  else if (grid_inst[i,"instruction"] == "turn on") {
    x[x1:x2, y1:y2] = 1
  }
  
  # if instruction is toggle, then turn on or turn off lights depending on light status 
  else if (grid_inst[i,"instruction"] == "toggle") {
    x[x1:x2, y1:y2] = apply(x[x1:x2, y1:y2], 1:2, function(x) {ifelse((x==1), 0, 1)})
  }
  
  x1 <- 0
  x2 <- 0
  y1 <- 0
  y2 <- 0
  
}

cat("How many lights are lit after going through Santa's instruction? : ", sum(x,na.rm = TRUE))

# Part 2 ----
# The light grid you bought actually has individual brightness controls; each light can have a brightness of zero or more. 
# The lights all start at zero.
# The phrase turn on actually means that you should increase the brightness of those lights by 1.
# The phrase turn off actually means that you should decrease the brightness of those lights by 1, to a minimum of zero.
# The phrase toggle actually means that you should increase the brightness of those lights by 2.
# What is the total brightness of all lights combined after following Santa's instructions?

x  <- matrix(0, 1000, 1000)

for (i in 1:length(day6_data)) {
  
  x1 <- grid_inst[i, "x1"]
  y1 <- grid_inst[i, "y1"]
  x2 <- grid_inst[i, "x2"]
  y2 <- grid_inst[i, "y2"]
  
  # instruction is turn off then lower the brightness by 1 
  if (grid_inst[i,"instruction"] == "turn off") {
    x[x1:x2, y1:y2] = apply(x[x1:x2, y1:y2], c(1,2), function(x) {ifelse((x>0), x-1, 0)})
  }
  
  # instruction is turn on then increase the brightness by 1
  else if (grid_inst[i,"instruction"] == "turn on") {
    x[x1:x2, y1:y2] = apply(x[x1:x2, y1:y2], c(1,2), function(x) {ifelse((x>=0), x+1, 0)})
  }
  
  # instruction is toggle, increase brightness by 2
  else if (grid_inst[i,"instruction"] == "toggle") {
    x[x1:x2, y1:y2] = apply(x[x1:x2, y1:y2], c(1,2), function(x) {ifelse((x>=0), x+2, 0)})
  }
  
  x1 <- 0
  x2 <- 0
  y1 <- 0
  y2 <- 0
}

cat("How many lights are lit after going through Santa's updated instruction? : ", sum(x,na.rm = TRUE))

