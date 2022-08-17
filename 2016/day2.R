# ----
# Day 2
# Bathroom Security ---

library(stringr)

# Read Puzzle Input
day2_data <- read.table(file = "~/AoC/2016/data/day2_data", sep=",")

day2_data <- str_split(day2_data$V1, "", simplify = T) 

# Part 1 ----
# Keypad
# 1 2 3
# 4 5 6
# 7 8 9

# instruction <- c("U", "U", "U", "U", "D")

instruction <- list(instruction1 = day2_data[1, ], 
                    instruction2 = day2_data[2, ], 
                    instruction3 = day2_data[3, ], 
                    instruction4 = day2_data[4, ], 
                    instruction5 = day2_data[5, ])

# Always Start at 5
numpad <- 5

code <- c(NA, NA, NA, NA, NA)

for (j in 1:length(instruction)) {
  for (i in 1:length(instruction[[j]])) {
    
    if (instruction[[j]][i] == "R") {
      if (numpad %in% c(3, 6, 9)) numpad <- numpad 
      else numpad <- numpad + 1
      print(numpad)
    }
    else if (instruction[[j]][i] == "L") {
      if (numpad %in% c(1, 4, 7)) numpad <- numpad 
      else numpad <- numpad - 1
      print(numpad)
    }
    else if (instruction[[j]][i] == "D") {
      if (numpad %in% c(7, 8, 9)) numpad <- numpad 
      else numpad <- numpad + 3    
      print(numpad)
    }
    else if (instruction[[j]][i] == "U") {
      if (numpad %in% c(1, 2, 3)) numpad <- numpad 
      else numpad <- numpad - 3
      print(numpad)
    }
  }
  
  code[j] <- numpad
  numpad <- as.numeric(code[j])
  
}

# Key 47978
paste0("Bathroom Code ", as.numeric(paste0(code[1], code[2], code[3], code[4], code[5])))

# Part 2 ----    
# Keypad
#     1
#   2 3 4
# 5 6 7 8 9
#   A B C
#     D

# A - 10
# B - 11
# C - 12
# D - 13

# Always Start at 5
numpad <- 5

code <- c(NA, NA, NA, NA, NA)

for (j in 1:length(instruction)) {
  for (i in 1:length(instruction[[j]])) {
    
    if (instruction[[j]][i] == "R") {
      if (numpad %in% c(1, 4, 9, 12, 13)) numpad <- numpad 
      else numpad <- numpad + 1
      print(numpad)
    }
    else if (instruction[[j]][i] == "L") {
      if (numpad %in% c(1, 2, 5, 10, 13)) numpad <- numpad 
      else numpad <- numpad - 1
      print(numpad)
    }
    else if (instruction[[j]][i] == "D") {
      if (numpad %in% c(5, 10, 12, 13, 9)) numpad <- numpad 
      else if (numpad %in% c(1, 11)) numpad <- numpad + 2
      else numpad <- numpad + 4
      print(numpad)
    }
    else if (instruction[[j]][i] == "U") {
      if (numpad %in% c(1, 2, 3)) numpad <- numpad 
      else if (numpad %in% c(3, 13)) numpad <- numpad - 2
      else numpad <- numpad - 4
      print(numpad)
    }
  }
  
  code[j] <- numpad
  numpad <- as.numeric(code[j])
  
}

code <- replace(code, code == 10, "A")
code <- replace(code, code == 11, "B")
code <- replace(code, code == 12, "D")
code <- replace(code, code == 13, "D")

# Key 659AD
paste0("Bathroom Code ", as.character(paste0(code[1], code[2], code[3], code[4], code[5])))

