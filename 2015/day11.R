# Day 11 AoC Corporate Policy ----
#
# Puzzle Input - hxbxwxba
#

library(stringr)
library(tidyverse)

# Function to find next character
next.char = function(string, char.index) {
  if (string[char.index] != "z") letters[which(string[char.index] == letters) + 1]
  else if (string[char.index] == "z") "a"
}

next.str.fun = function(string) { 
  
  ln <- length(string)
  
  if (string[ln] != "z") {
    next.string = c(string[1:(ln-1)], next.char(string,ln))
  }
  else if (string[ln] == "z" & string[ln-1] != "z") {
    next.string = c(string[1:(ln-2)], next.char(string, ln-1), "a")
  }
  else if (string[ln] == "z" & string[ln-1] == "z" & string[ln-2] != "z") {
    next.string = c(string[1:(ln-3)], next.char(string, ln-2),"a", "a")
  }
  else if (string[ln] == "z" & string[ln-1] == "z" & string[ln-2] == "z" & string[ln-3] != "z") {
    next.string = c(string[1:(ln-4)], next.char(string, ln-3), "a", "a", "a")
  }
  else if (string[ln] == "z" & string[ln-1] == "z" & string[ln-2] == "z" & string[ln-3] == "z" & string[ln-4] != "z") {
    next.string = c(string[1:(ln-5)], next.char(string, ln-4), "a", "a", "a", "a")
  }
  else if (string[ln] == "z" & string[ln-1] == "z" & string[ln-2] == "z" & string[ln-3] == "z" & string[ln-4] == "z" & string[ln-5] != "z") {
    next.string = c(string[1:(ln-6)], next.char(string, ln-5),"a", "a", "a", "a", "a")
  }
  else if (string[ln] == "z" & string[ln-1] == "z" & string[ln-2] == "z" & string[ln-3] == "z" & string[ln-4] == "z" & string[ln-5] == "z" & string[ln-6] != "z") {
    next.string = c(string[1:(ln-7)], next.char(string, ln-6), "a", "a", "a", "a", "a", "a")
  }
  else if (string[ln] == "z" & string[ln-1] == "z" & string[ln-2] == "z" & string[ln-3] == "z" & string[ln-4] == "z" & string[ln-5] == "z" & string[ln-6] == "z" & string[ln-7] != "z") {
    next.string = c(next.char(string, ln-7), "a", "a", "a", "a", "a", "a", "a")
  }
  
  
  return(next.string)
  
}

# Part 1 ----
# Unfortunately for Santa, a new Security-Elf recently started, and he has imposed some additional password requirements:
# Passwords must include one increasing straight of at least three letters, like abc, bcd, cde, and so on, up to xyz. 
#   They cannot skip letters; abd doesn't count.
# Passwords may not contain the letters i, o, or l, as these letters can be mistaken for other characters and are therefore confusing.
# Passwords must contain at least two different, non-overlapping pairs of letters, like aa, bb, or zz.

input <- "hxbxwxba"
next.str.list <- list()

tmp_str <- input

for (i in 1:1000000) {
  
  next_str <- next.str.fun(as.character(unlist(strsplit(tmp_str, split=character(0)))))
  next.str.list[[i]] <- paste0(next_str, collapse = "")
  
  # Change tmp_str to next string
  tmp_str <- next_str
  next_str <- ""
  
  # Pass cant contain i, o or l
  if (!(grepl("i|o|l", next.str.list[[i]])) &
      
      # Pass must contain 1 inc sraight of at least three letters
      (grepl("abc|bcd|cde|def|efg|fgh|ghi|hij|ijk|jkl|klm|lmn|mno|nop|opq|pqr|qrs|rst|stu|tuv|uvw|uwx|wxy|xyz", next.str.list[[i]])) &
      
      # Pass must contain two diff non overlapping pairs of letters     
      (grepl("(\\w)\\1.*(\\w)\\2", next.str.list[[i]]))) {
    
    # If the password is found then stop. 
    break
    
  }
  
}

cat(paste0("The next String is ", next.str.list[[length(next.str.list)]]))

# Part 2 ----
# Santa's password expired again. What's the next one? 

new_pass <- next.str.list[[length(next.str.list)]] 

next.str.list <- list()

tmp_str <- new_pass

for (i in 1:1000000) {
  
  next_str <- next.str.fun(as.character(unlist(strsplit(tmp_str, split=character(0)))))
  next.str.list[[i]] <- paste0(next_str, collapse = "")
  
  # Change tmp_str to next string
  tmp_str <- next_str
  next_str <- ""
  
  # Pass cant contain i, o or l
  if (!(grepl("i|o|l", next.str.list[[i]])) &
      
      # Pass must contain 1 inc sraight of at least three letters
      (grepl("abc|bcd|cde|def|efg|fgh|ghi|hij|ijk|jkl|klm|lmn|mno|nop|opq|pqr|qrs|rst|stu|tuv|uvw|uwx|wxy|xyz", next.str.list[[i]])) &
      
      # Pass must contain two diff non overlapping pairs of letters     
      (grepl("(\\w)\\1.*(\\w)\\2", next.str.list[[i]]))) {
    
    # If the password is found then stop. 
    break
    
  }
  
}

cat(paste0("The next String is ", next.str.list[[length(next.str.list)]]))
