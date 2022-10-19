# ----
# Day 8
# Two-Factor Authentication ----

library(stringr)
library(tidyverse)

# Read Puzzle Input
day8_data <- read.table(file = "~/AoC/2016/data/day8_data", sep=",")

# Screen is 50 pixel wide and 6 pixel tall

screen <- matrix(0, 6, 50)
input <- day8_data$V1[1:4]

for (i in 1:length(input)) {
  if (str_detect(input[i], "rect")) {
    
    # Turn on the top left ZxZ grid
    col <- str_extract_all(input[i], "[0-9]+", simplify = TRUE)[,1]
    row <- str_extract_all(input[i], "[0-9]+", simplify = TRUE)[,2]
    
    screen[1:row, 1:col] <- 1 
  }
  else if (str_detect(input[i], "rotate")) {
    
    if (str_detect(input[i], "row")) {
      rownum <- str_extract_all(input[i], "[0-9]+", simplify = TRUE)[,1]
      rowby <- str_extract_all(input[i], "[0-9]+", simplify = TRUE)[,2]
      
      print(rownum)
      print(rowby)
    }
    else if (str_detect(input[i], "column")) {
      colnum <- str_extract_all(input[i], "[0-9]+", simplify = TRUE)[,1]
      colby <- str_extract_all(input[i], "[0-9]+", simplify = TRUE)[,2]
      
      print(colnum)
      print(colby)
    }
  }
}
