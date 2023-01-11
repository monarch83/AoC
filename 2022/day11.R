# Day 11: Monkey in the Middle ---

library(stringr)
library(dplyr)

# Read Puzzle Input
day11_data <- readLines("~/AoC/2022/data/day11_data")

monkeys <- as.numeric(str_extract(day11_data[seq(1,length(day11_data),by=7)], "[0-9+]"))

start_items <- str_extract_all(day11_data[seq(2,length(day11_data),by=7)], "[0-9]+")

operation <- unlist(str_extract_all(day11_data[seq(3,length(day11_data),by=7)], "[*+-/]"))

operation_by <- unlist(lapply(str_extract_all(day11_data[seq(3,length(day11_data),by=7)], "[a-z A-Z 0-9]+"), tail, n = 1L))

test_divisible <- as.numeric(unlist(str_extract_all(day11_data[seq(4,length(day11_data),by=7)], "[0-9]+")))
true_ope <- as.numeric(unlist(str_extract_all(day11_data[seq(5,length(day11_data),by=7)], "[0-9]+")))
false_ope <- as.numeric(unlist(str_extract_all(day11_data[seq(6,length(day11_data),by=7)], "[0-9]+")))

# Total # of inspected items
monk_ispect <- rep(0, length(monkeys))

supermod <- prod(test_divisible)

# Part 1 ----

for (r in 1:20) {

  for (i in 1:length(monkeys)) {
    
    # Do for each items for that monkey
    
    # Inspect worry level from each of the item
    if (length(start_items[[i]] > 0)) {
      for (j in 1:length(start_items[[i]])) {
        
        operation_byt <- operation_by
        if (operation_by[i] == " old") {
          operation_byt[i] <- start_items[[i]][j]
        } else {
          operation_byt[i] <- operation_by[i]
        }
        
        # perform operation
        if (operation[i] == "*") {
          worry <- as.numeric(start_items[[i]][j]) * as.numeric(operation_byt[i])
        } else if (operation[i] == "+") {
          worry <- as.numeric(start_items[[i]][j]) + as.numeric(operation_byt[i])
        }
        
        # Bored divide by 3, take floor
        worry <- floor(worry / 3)
        
        # perform test, 
        # True/False throw 
        if (worry %% test_divisible[i] == 0) {
          start_items[[true_ope[i]+1]] <- c(start_items[[true_ope[i]+1]], worry)
        } else if (worry %% test_divisible[i] != 0) {
          start_items[[false_ope[i]+1]] <- c(start_items[[false_ope[i]+1]], worry)
        }
        
      }
      
      monk_ispect[i] <- monk_ispect[i] + length(start_items[[i]])
      
    }
    #print(start_items[[i]])
    start_items[[i]] <- vector()
    
  }
  
  print(paste0("Round: ", r))
  print(start_items)
  
}


# Top two monkeys
cat("What is the level of monkey business after 20 rounds of stuff-slinging simian shenanigans?", "\n", 
    sort(monk_ispect, decreasing = TRUE)[1] * sort(monk_ispect, decreasing = TRUE)[2])

    
# Part 2 ----

for (r in 1:10000) {
  
  for (i in 1:length(monkeys)) {
    
    # Do for each items for that monkey
    
    # Inspect worry level from each of the item
    if (length(start_items[[i]] > 0)) {
      for (j in 1:length(start_items[[i]])) {
        
        operation_byt <- operation_by
        if (operation_by[i] == " old") {
          operation_byt[i] <- start_items[[i]][j]
        } else {
          operation_byt[i] <- operation_by[i]
        }
        
        # perform operation
        if (operation[i] == "*") {
          worry <- as.numeric(start_items[[i]][j]) * as.numeric(operation_byt[i])
        } else if (operation[i] == "+") {
          worry <- as.numeric(start_items[[i]][j]) + as.numeric(operation_byt[i])
        }
        
        # Bored divide by 3, take floor
        worry <- worry %% supermod
        
        # perform test, 
        # True/False throw 
        if (worry %% test_divisible[i] == 0) {
          start_items[[true_ope[i]+1]] <- c(start_items[[true_ope[i]+1]], worry)
        } else if (worry %% test_divisible[i] != 0) {
          start_items[[false_ope[i]+1]] <- c(start_items[[false_ope[i]+1]], worry)
        }
        
      }
      
      monk_ispect[i] <- monk_ispect[i] + length(start_items[[i]])
      
    }
    #print(start_items[[i]])
    start_items[[i]] <- vector()
    
  }
  
  print(monk_ispect)
  
}


# Top two monkeys
cat("what is the level of monkey business after 10000 rounds?", "\n", 
    sort(monk_ispect, decreasing = TRUE)[1] * sort(monk_ispect, decreasing = TRUE)[2])


