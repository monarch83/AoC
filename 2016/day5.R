# AoC 206 Day 5 ----
# How about a nice game of chess?

# Part 1 ----

# Actual Door ID - uqwqemis (Puzzle Input)

library(digest)
library(stringr)

text = "uqwqemis"

itr <- 0 

pass <- character()

repeat{
  
  itr <- itr + 1
  
  hash <- digest::digest(paste0(text, itr), algo="md5", serialize=F)
  
  if (stringr::str_sub(hash, 1, 5) == "00000") {
    
    pass <- c(pass, stringr::str_sub(hash, 6, 6))
    
    cat("Found new character ", itr, "\n")
    
    if (length(pass) == 8) break
    
  }
  
}

cat("Password: ", paste(pass, collapse=""))

# Part 2 ----

# Actual Door ID - uqwqemis (Puzzle Input)

library(digest)
library(stringr)

text = "uqwqemis"

itr <- 0 

pass_part2 <- rep(NA, 8)

repeat{
  
  itr <- itr + 1
  
  hash <- digest::digest(paste0(text, itr), algo="md5", serialize=F)
  
  if (stringr::str_sub(hash, 1, 5) == "00000") {
    
    if (grepl("[0-7]", stringr::str_sub(hash, 6, 6))) {
      if (is.na(pass_part2[as.numeric(stringr::str_sub(hash, 6, 6))+1])) {
        pass_part2[as.numeric(stringr::str_sub(hash, 6, 6))+1] <- stringr::str_sub(hash, 7, 7)
        print(pass_part2)
      }
    }
    
    cat("Found new character ", itr, "\n")
    cat("Found new hash ", hash, "\n")
    
    if (!any(is.na(pass_part2))) break
    
  }
  
}

cat("Password: ", paste(pass_part2, collapse=""))