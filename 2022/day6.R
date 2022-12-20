# ----
# Day 6: Tuning Trouble ---

library(stringr)
library(dplyr)

# Read Puzzle Input
day6_data <- read.table("~/AoC/2022/data/day6_data")

head(day6_data)

day6_vec <- unlist(strsplit(day6_data$V1, ""))

# Test cases
# day6_vec <- unlist(strsplit("mjqjpqmgbljsphdztnvjfqwrcgsmlb", ""))
# day6_vec <- unlist(strsplit("bvwbjplbgvbhsrlpgdmjqwftvncz", ""))
# day6_vec <- unlist(strsplit("nppdvjthqldpwncqszvftbrmjlhg", ""))
# day6_vec <- unlist(strsplit("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", ""))
# day6_vec <- unlist(strsplit("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", ""))

fourchar <- rep(NA, 4)

# Part 1 ----

for (i in 1:length(day6_vec)) {
  
  if ((any(day6_vec[i] == day6_vec[(i+1):(i+3)]) == FALSE) &
      (any(day6_vec[i+1] == day6_vec[(i+2):(i+3)]) == FALSE) &
      (any(day6_vec[i+2] == day6_vec[(i+3)]) == FALSE)) {
    
    fourchar <- paste0(day6_vec[i], day6_vec[i+1], day6_vec[i+2], day6_vec[i+3])
    marker_pos <- i + 3
    
    # print(fourchar)
    # print(marker_pos)
    
    stop(cat("How many characters need to be processed before the first start-of-packet marker is detected?", fourchar, marker_pos, "\n"))
    
  }
}

# Part 2 ---- 

fourtchar <- rep(NA, 14)

for (i in 1:length(day6_vec)) {
  
  if (length(unique(day6_vec[seq(i, i + 13)])) == 14) {
    
    fourtchar <- paste0(day6_vec[seq(i, i+13)])
    marker_pos <- i + 13
    
    stop(cat("How many characters need to be processed before the first start-of-message marker is detected?", fourtchar, marker_pos, "\n"))
    
  }

}
