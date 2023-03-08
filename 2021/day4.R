# ----
# Day 4: Giant Squid ---

# Part 1 ----
# What will your final score be if you choose that board?

library(stringr)
library(dplyr)

# Read Puzzle Input
day4_data <- read.table(file = "~/AoC/2021/data/day4_data", blank.lines.skip=F, colClasses=c("character"))

num_drawn_list <- as.numeric(unlist(strsplit(as.character(day4_data$V1[1]), split=",")))

day4_data2 <- day4_data[-c(1,2),]
day4_data2 <- day4_data2[-c((1:100)*6), ]

matrix_NA <- matrix(NA, nrow=5, ncol=5)

matrix.list <- rep(list(matrix_NA), 100)

start <- seq(1,500,by=5)
end <- seq(5,500,by=5)

for (i in 1:100) {
  
  matrix.list[[i]] <- sapply(day4_data2[start[i]:end[i], ], as.numeric)
  
}

for (i in 1:length(num_drawn_list)) {
  
  # Search through each matrix
  for (j in 1:length(matrix.list)) {
    
    row <- which(matrix.list[[j]] == num_drawn_list[i], arr.ind = TRUE)[1]
    col <- which(matrix.list[[j]] == num_drawn_list[i], arr.ind = TRUE)[2]
    
    matrix.list[[j]][row, col] <- NA
    
    print(num_drawn_list[i])
    
    # Break if rows, columns are all TRUE
    if (any(rowSums(matrix.list[[j]], na.rm=TRUE) == 0) | any(colSums(matrix.list[[j]], na.rm=TRUE) == 0)) {
      
      called_num <- num_drawn_list[i]
      winning_board <- j
      stop("Let's break out!")
    } 
  }
}

cat(paste0("What will your final score be if you choose that board? "), 
    sum(matrix.list[[winning_board]], na.rm = TRUE) * called_num)

# Part 2 ----
# Figure out which board will win last. Once it wins, what would its final score be?

matrix_NA <- matrix(NA, nrow=5, ncol=5)

matrix.list <- rep(list(matrix_NA), 100)

start <- seq(1,500,by=5)
end <- seq(5,500,by=5)

winning_board <- rep(NA,100)

for (i in 1:100) {
  
  matrix.list[[i]] <- sapply(day4_data2[start[i]:end[i], ], as.numeric)
  
}

for (i in 1:length(num_drawn_list)) {
  
  # Search through each matrix
  for (j in 1:length(matrix.list)) {
    
    row <- which(matrix.list[[j]] == num_drawn_list[i], arr.ind = TRUE)[1]
    col <- which(matrix.list[[j]] == num_drawn_list[i], arr.ind = TRUE)[2]
    
    matrix.list[[j]][row, col] <- NA
    
    print(num_drawn_list[i])
    
    if (any(rowSums(matrix.list[[j]], na.rm=TRUE) == 0) | any(colSums(matrix.list[[j]], na.rm=TRUE) == 0)) {
     
       winning_board[j] <- TRUE 
    }
    
    if (!any(is.na(winning_board))) {
      # Break if rows, columns are all TRUE
      called_num <- num_drawn_list[i]
      winning_board <- j
      stop("Let's break out!")
    } 
  }
}


cat(paste0("Figure out which board will win last. Once it wins, what would its final score be? "), 
    sum(matrix.list[[winning_board]], na.rm = TRUE) * called_num)
