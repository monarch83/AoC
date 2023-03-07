# ----
# Day 3: Binary Diagnostic ---

# Part 1 ----
# What is the power consumption of the submarine? (Be sure to represent your answer in decimal, not binary.)

library(stringr)
library(dplyr)

# Read Puzzle Input
day3_data <- read.table(file = "~/AoC/2021/data/day3_data", blank.lines.skip=F, colClasses=c("character"))

instructions <- day3_data$V1

gamma_binary <- rep(NA, nchar(instructions)[1])
epsilon_binary <- rep(NA, nchar(instructions)[1])

for (i in 1:nchar(instructions)[1]) {
  
  gamma_binary[i] <- as.numeric(names(sort(table(as.numeric(substr(instructions, i,i))), decreasing = TRUE)[1]))
  epsilon_binary[i] <-  as.numeric(names(sort(table(as.numeric(substr(instructions, i,i))), decreasing = FALSE)[1]))
  
}

bitsToInt<-function(x) {
  packBits(rev(c(rep(FALSE, 32-length(x)%%32), as.logical(x))), "integer")
}

bitsToInt(gamma_binary)
bitsToInt(epsilon_binary)

cat(paste0("What is the power consumption of the submarine? (Be sure to represent your answer in decimal, not binary.) "), 
    bitsToInt(gamma_binary) * bitsToInt(epsilon_binary))

# Part 2 ----
# What is the life support rating of the submarine? (Be sure to represent your answer in decimal, not binary.)

oxygen_co2_rating <- function(input, most_common, tie){
  
  # Find most common from 1st bit of each number
  temp_inst <- instructions 
  curr_bit <- 1
  
  repeat {
    
    if (length(temp_inst) == 1) break
    
      if (table(as.numeric(substr(temp_inst, curr_bit, curr_bit)))[1] != table(as.numeric(substr(temp_inst, curr_bit, curr_bit)))[2]) {
        
        temp_inst <- temp_inst[which(as.numeric(substr(temp_inst, curr_bit, curr_bit)) == 
                                         as.numeric(names(sort(table(as.numeric(substr(temp_inst, curr_bit, curr_bit))), decreasing = most_common)[1])))]
      
      } else {
        
        temp_inst <- temp_inst[which(as.numeric(substr(temp_inst, curr_bit, curr_bit)) == tie)]
        
      }  
    
    temp_inst
    curr_bit
    
    if (curr_bit %% nchar(instructions)[1] != 0) { 
      curr_bit <- curr_bit + 1
    } else { 
      curr_bit = 1 
    }
    
  }

  return(as.numeric(unlist(strsplit(temp_inst, split=character(0)))))
  
}

oxygen_rat <- oxygen_co2_rating(input = instructions, most_common = TRUE, tie = 1)
co2_rat <- oxygen_co2_rating(input = instructions, most_common = FALSE, tie = 0)


cat(paste0(" What is the life support rating of the submarine? (Be sure to represent your answer in decimal, not binary.) "), 
    bitsToInt(oxygen_rat) * bitsToInt(co2_rat))
