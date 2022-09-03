# ----
# Day 6
# ignals and Noise ---

library(stringr)
library(tidyverse)

# Read Puzzle Input
day6_data <- read.table(file = "~/AoC/2016/data/day6_data", sep=",")

day6_data <- day6_data %>%
  mutate(char1 = stringr::str_extract_all(V1, "[a-z]", simplify = TRUE)[,1],
         char2 = stringr::str_extract_all(V1, "[a-z]", simplify = TRUE)[,2],
         char3 = stringr::str_extract_all(V1, "[a-z]", simplify = TRUE)[,3],
         char4 = stringr::str_extract_all(V1, "[a-z]", simplify = TRUE)[,4],
         char5 = stringr::str_extract_all(V1, "[a-z]", simplify = TRUE)[,5],
         char6 = stringr::str_extract_all(V1, "[a-z]", simplify = TRUE)[,6],
         char7 = stringr::str_extract_all(V1, "[a-z]", simplify = TRUE)[,7],
         char8 = stringr::str_extract_all(V1, "[a-z]", simplify = TRUE)[,8])

# Find most common char in each of the columns
common_char1 <- names(sort(table(day6_data$char1), decreasing=TRUE)[1])
common_char2 <- names(sort(table(day6_data$char2), decreasing=TRUE)[1])
common_char3 <- names(sort(table(day6_data$char3), decreasing=TRUE)[1])
common_char4 <- names(sort(table(day6_data$char4), decreasing=TRUE)[1])
common_char5 <- names(sort(table(day6_data$char5), decreasing=TRUE)[1])
common_char6 <- names(sort(table(day6_data$char6), decreasing=TRUE)[1])
common_char7 <- names(sort(table(day6_data$char7), decreasing=TRUE)[1])
common_char8 <- names(sort(table(day6_data$char8), decreasing=TRUE)[1])

err_version <- 
 paste0(common_char1,common_char2,common_char3,common_char4,common_char5,common_char6,common_char7,common_char8,collapse = "")

print(paste("Error corrected version: ", err_version))

# Part Two ----

# Find least common char in each of the columns
leastc_char1 <- names(sort(table(day6_data$char1), decreasing=FALSE)[1])
leastc_char2 <- names(sort(table(day6_data$char2), decreasing=FALSE)[1])
leastc_char3 <- names(sort(table(day6_data$char3), decreasing=FALSE)[1])
leastc_char4 <- names(sort(table(day6_data$char4), decreasing=FALSE)[1])
leastc_char5 <- names(sort(table(day6_data$char5), decreasing=FALSE)[1])
leastc_char6 <- names(sort(table(day6_data$char6), decreasing=FALSE)[1])
leastc_char7 <- names(sort(table(day6_data$char7), decreasing=FALSE)[1])
leastc_char8 <- names(sort(table(day6_data$char8), decreasing=FALSE)[1])

err_version2 <- 
  paste0(leastc_char1,leastc_char2,leastc_char3,leastc_char4,leastc_char5,leastc_char6,leastc_char7,leastc_char8,collapse = "")

print(paste("Original Message: ", err_version2))
