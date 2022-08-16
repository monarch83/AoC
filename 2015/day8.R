# Day 8 AoC 2015
# String Literals 


# Part 1 ----
# Disregarding white space what is the number of char of code for string literals minus
# the number of char in memory for the values of the string

library(dplyr)
library(stringr)
library(readr)
library(tidyr)

day8_data <- readLines("~/AoC/2015/data/day8_data")

day8_df <- data.frame(day8_data)

# Remove start and end quotes
day8_df$Input <- gsub('^\\"|\\"$', "", day8_df$day8_data)

day8_df$nchart <- nchar(day8_df$Input)

# Count backslash \ 
day8_df$p1 <- str_count(day8_df$Input, '\\\\[^x^"]')
# Count Quotes
day8_df$p2 <- str_count(day8_df$Input, '\"')

# Replace \\\\ to some other Character so it does not mess up below regex
day8_df$Inputn <- str_replace_all(day8_df$Input, '\\\\[^x^"]', 'Q')

# Special Char Count
day8_df$p3 <- str_count(day8_df$Inputn, "(?<!\\\\)\\\\x[0-9a-f]{2}")

day8_df$memstr <- day8_df$nchart - (day8_df$p1 + day8_df$p2 + day8_df$p3*3 )

day8_df$strcod <- day8_df$nchart + 2

day8_df$part1 <- day8_df$strcod - day8_df$memstr

cat("Number of char of code for string literals minus number of char in memory for values of string ", sum(day8_df$part1))

# Part 2 ----
# encode each code representation as a new string and find the number of characters of the new encoded representation, 
# including the surrounding double quotes.

day8_df$input_p2 <- str_replace_all(day8_df$Input, '\\\\', 'QQQ')

cat("Number of char of the new encoded rep including double quotes: ", 
    (sum(nchar(day8_df$input_p2)) + nrow(day8_df)*3*2 - sum(day8_df$p1)*2 - sum(day8_df$p3)) - sum(day8_df$strcod)
)

