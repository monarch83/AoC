# ----
# Day 8
# Two-Factor Authentication ----

library(stringr)
library(tidyverse)

# Read Puzzle Input
day8_data <- read.table(file = "~/AoC/2016/data/day8_data", sep=",")

rect_fun <- function(rect, row, col) {
  rect[seq_len(row), seq_len(col)] <- 1
  return(rect)
}

rotate_fun <- function(x, by) {
  len <- length(x)
  x[c(seq(len - by + 1, len), seq(1, len -by))]
}

rotate_column <- function(rect, column, by) {
  rect[, column] <- rotate_fun(rect[, column], by)
  return(rect)
}

rotate_row <- function(rect, row, by) {
  rect[row, ] <- rotate_fun(rect[row, ], by)
  return(rect)
}

# Screen is 50 pixel wide and 6 pixel tall

screen <- matrix(0, 6, 50)
input <- day8_data$V1
input <- c("rect 3x2", "rotate column x=1 by 1", "rotate row y=0 by 4", "rotate column x=1 by 1")

for (i in 1:length(input)) {
  if (str_detect(input[i], "rect")) {
    
    # Turn on the top left ZxZ grid
    col <- as.numeric(str_extract_all(input[i], "[0-9]+", simplify = TRUE)[,1])
    row <- as.numeric(str_extract_all(input[i], "[0-9]+", simplify = TRUE)[,2])
    
    screen <- rect_fun(screen, row, col)
    
  }
  else if (str_detect(input[i], "rotate")) {
    
    if (str_detect(input[i], "row")) {
      rownum <- as.numeric(str_extract_all(input[i], "[0-9]+", simplify = TRUE)[,1]) + 1
      rowby <- as.numeric(str_extract_all(input[i], "[0-9]+", simplify = TRUE)[,2])
      
      screen <- rotate_row(screen, rownum, rowby) 
      
    }
    else if (str_detect(input[i], "column")) {
      colnum <- as.numeric(str_extract_all(input[i], "[0-9]+", simplify = TRUE)[,1]) + 1
      colby <- as.numeric(str_extract_all(input[i], "[0-9]+", simplify = TRUE)[,2])
      
      screen <- rotate_column(screen, colnum, colby)
      
    }
  }
}
