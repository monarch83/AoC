# AoC 2015 Day 23
# Opening the Turing Lock ----

library(dplyr)
library(stringr)
library(gtools)

day23_data <- readLines("~/AoC/2015/data/day23_data")

# hlf r sets register r to half its current value, then continues with the next instruction.
# tpl r sets register r to triple its current value, then continues with the next instruction.
# inc r increments register r, adding 1 to it, then continues with the next instruction.
# jmp offset is a jump; it continues with the instruction offset away relative to itself.
# jie r, offset is like jmp, but only jumps if register r is even ("jump if even").
# jio r, offset is like jmp, but only jumps if register r is 1 ("jump if one", not odd)

# Regiser are named with a & b and starts with 0 value

# What is the value in register b when the program in your puzzle input is finished executing?

# example_data <- c("inc a", "jio a, +2", "tpl a", "inc a")

computer_fun <- function(data, a, b) {
  
  # Start with Location 1
  location <- 1
  
  rega <- a
  regb <- b
  
  repeat {
    
    instruction <- str_extract(data[location], "jio|jie|jmp|tpl|inc|hlf")
    integer <- str_extract(data[location], "a|b")
    offset <- as.numeric(str_extract(data[location], "[0-9]+"))
    dir <- str_extract(data[location], "\\+|\\-")
    
    cat(paste0(" a ", rega, " "))
    cat(paste0( "b ", regb, " "))
    cat(paste0(" input ", location, " ", data[location]), "\n")
    
    if (location > length(data)) break
    
    if (instruction == "jmp") {
      
      if (dir == "+") location <- location + offset
      else if (dir == "-") location <- location - offset
      
    }
    else if (instruction == "jio") {
      
      if (rega == 1) {
        if (dir == "+") location <- location + offset
        else if (dir == "-") location <- location - offset
      }
      else {
        location <- location + 1
      }
    }
    else if (instruction == "jie") {
      
      if (rega %% 2 == 0) {
        if (dir == "+") location <- location + offset
        else if (dir == "-") location <- location - offset
      }
      else {
        location <- location + 1
      }
    }
    else if (instruction == "tpl") {
      
      if (integer == "a") rega <- rega * 3
      else if (integer == "b") regb <- regb * 3
      location <- location + 1
      
    }
    else if (instruction == "hlf") {
      
      if (integer == "a") rega <- rega/2
      else if (integer == "b") regb <- regb/2
      location <- location + 1
      
    }
    else if (instruction == "inc") {
      
      if (integer == "a") rega <- rega + 1
      else if (integer == "b") regb <- regb + 1
      location <- location + 1
      
    }
    
    
  }
  
  return(list(rega, regb, location))
}

# computer_fun(data = example_data, a = 0, b = 0)

# Part 1 ----
# What is the value in register b when the program in your puzzle input is finished executing?
part1 <- computer_fun(data = day23_data, a = 0, b = 0) 

cat("What is the value in register b when the program in your puzzle input is finished executing? ",
    "a = ", part1[[1]], "b = ", part1[[2]], "Location/Instruction ", part1[[3]])

# Part 2 ----
# What is the value in register b when the program in your puzzle input is finished executing?
part2 <- computer_fun(data = day23_data, a = 1, b = 0) 

cat("What is the value in register b when the program in your puzzle input is finished executing, a starts with 1? ",
    "a = ", part2[[1]], "b = ", part2[[2]], "Location/Instruction ", part2[[3]])
