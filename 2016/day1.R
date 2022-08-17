# ----
# Day 1 
# No Time for a Taxicab ---

library(stringr)

# Read Puzzle Input
day1_data <- read.table(file = "~/AoC/2016/data/day1_data", sep=",")

# day1_data <- data.frame(V1 = "R2", V2 = "L3")
# day1_data <- data.frame(V1 = "R2", V2 = "R2", V3 = "R2")
# day1_data <- data.frame(V1 = "R5", V2 = "L5", V3 = "R5", V4 = "R3")

instruction <- stringr::str_extract(unname(unlist(day1_data[1, ])), "R|L")
blocks <- as.numeric(stringr::str_extract(unname(unlist(day1_data[1, ])), "[0-9]+"))

cordinates <- c(0,0)

# Always start with North Position
pos <- c(0, 1)

# Part 1 ----
for (i in 1:ncol(day1_data)) {
  
  if (instruction[i] == "R") {
    pos <- rev(pos) * c(1,-1)
  }
  else if (instruction[i] == "L") {
    pos <- rev(pos * c(1,-1))
  }
  cordinates <- cordinates + pos * blocks[i]
  print(cordinates)
}

paste0("Shortest Distance: ", abs(cordinates[1] + cordinates[2]))

# Part 2 ----

cordinates <- matrix(0, nrow = 1, ncol = 2)

pos <- matrix(c(0,1), nrow=1, ncol=2)

last <- function(x) x[nrow(x), ]

for (i in 1:ncol(day1_data)) {
  
  if (instruction[i] == "R") {
    pos <- rev(pos) * c(1,-1)
  }
  else if (instruction[i] == "L") {
    pos <- rev(pos * c(1,-1))
  }
  
  for (j in seq_len(blocks[i])) {
    
    cordinates <- rbind(cordinates, pos + last(cordinates))
    
  }
}

sum(abs(cordinates[which(duplicated(cordinates))[1], ]))
