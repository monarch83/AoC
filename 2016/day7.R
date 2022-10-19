# ----
# Day 7
# Internet Protocol Version 7 ----

library(stringr)
library(tidyverse)

# Read Puzzle Input
day7_data <- read.table(file = "~/AoC/2016/data/day7_data", sep=",")

# Part 1----

# String without brackets
str_nobrk <- str_replace_all(day7_data$V1, "\\[.+?\\]", " ")

# String within brackets
str_brk <- str_extract_all(day7_data$V1, "\\[.+?\\]")

# ABBA 
abba <- ifelse(str_extract_all(str_nobrk,"([a-z])((?!\\1)[a-z])\\2\\1", simplify = TRUE)[,1] != "" |
                 str_extract_all(str_nobrk,"([a-z])((?!\\1)[a-z])\\2\\1", simplify = TRUE)[,1] != "", TRUE, FALSE)


# ABBA within Bracket 
abba_brk <- unlist(lapply(str_brk, function(x) ifelse(paste(str_extract_all(x,"([a-z])((?!\\1)[a-z])\\2\\1", simplify = TRUE), collapse = "") != "", TRUE, FALSE)))
  
# supports TLS
tlsfl <- ifelse(abba & abba_brk == FALSE, TRUE, FALSE)

paste0("How many IPs in your puzzle input support TLS? ", sum(tlsfl))

# Part 2----

test <- "aba[bab]xyz"

x <- str_split(test, "")[[1]]

# 1st & 3rd
one_three <- lead(x, 2) == x
middle <- lead(x,1) != x

first <- x %in% letters
second <- x %in% letters

which(one_three & middle & first & second)
