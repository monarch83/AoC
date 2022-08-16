# AoC Day 4

# Part 1 ----
# Santa needs help mining some AdventCoins (very similar to bitcoins) to use as gifts for all the economically 
# forward-thinking little girls and boys.

# To do this, he needs to find MD5 hashes which, in hexadecimal, start with at least five zeroes. 
# The input to the MD5 hash is some secret key (your puzzle input, given below) followed by a number in decimal. 
# To mine AdventCoins, you must find Santa the lowest positive number (no leading zeroes: 1, 2, 3, ...) that produces such a hash.
#
# Part 2 ----
# Now find one that starts with six zeroes.

# install.packages("digest")
library(digest)
library(stringr)

# hash.vector <- rep(0,300000)

# test cases
# text = "abcdef"
# text = "pqrstuv"

text = "yzbqklnj"


# List of Iteration
iter <- 10000000

iter_list = seq(1, iter, 1)


# Create Hash 
hash_text_fun <- function(x) {
  
  hash <- digest::digest(paste0(text, iter_list[x]), algo="md5", serialize=F)
  
  # if (stringr::str_sub(hash, 1, 5) == "000000") break;
  
  return(hash)
  
}

hash_list <- unlist(lapply(iter_list, hash_text_fun))


cat("Lowest Number combines with to make an MD5 hash starting with five zeroes is: \n", 
    "MD5 Hash: ", 
    hash_list[stringr::str_sub(hash_list, 1, 5) == "00000"][1], "\n",
    "Key: ",
    which(stringr::str_sub(hash_list, 1, 5) == "00000")[1]
)


cat("Lowest Number combines with to make an MD5 hash starting with six zeroes is: \n", 
    "MD5 Hash: ", 
    hash_list[stringr::str_sub(hash_list, 1, 6) == "000000"][1], "\n",
    "Key: ",
    which(stringr::str_sub(hash_list, 1, 6) == "000000")[1]
)
