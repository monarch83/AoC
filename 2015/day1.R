# ----
# Day 1 
# Part 1 - n opening parenthesis, (, means he should go up one floor, and a closing parenthesis, ), means he should go down one floor.
# To what floor do the instructions take Santa?

library(stringr)

# Read Puzzle Input
day1_data <- read.table(file = "~/AoC/2015/data/day1_data")

# Compute Total # of parenthesis 
total <- nchar(as.character(day1_data$V1))

# Compute Total # of Open parenthesis 
count_o <- stringr::str_count(as.character(day1_data$V1), fixed('('))

# Compute Total # of Close parenthesis
count_c <- stringr::str_count(as.character(day1_data$V1), fixed(')'))

# Subtract to get the answer 
ans <- as.numeric(count_o - count_c)

cat(paste0("What Floor The instructions take Santa: ", ans))

rm(list=ls())

# -----

# Part 2
# Now, given the same instructions, find the position of the first character 
# that causes him to enter the basement (floor -1). 

library(stringr)

# Read Puzzle Input
day1_data <- read.table(file = "~/AoC/2015/data/day1_data")

# Split into Vector
split_x <- unlist(strsplit(as.character(day1_data$V1), split=character(0)))

# Convert to Numeric Values [Up Floor = 1, Down Floor = -1]
split_num <- ifelse(split_x == "(", 1, 
                    ifelse(split_x == ")", -1, NA))

# Find first time the value went to -1
cat(paste0("What is the position of the character that causes Santa to first enter the basement? "), 
    which(cumsum(split_num) == -1)[1])


# gganimate it
library(ggplot2)
library(gganimate)
library(dplyr)

split_num <- data.frame(split_num)
colnames(split_num) <- "parenthesis"

split_num <- split_num %>%
  mutate(floor_pos = cumsum(parenthesis),
         present_num = row_number())

g <- 
  ggplot(split_num, aes(x = present_num, y = floor_pos)) +
  geom_line(color = "blue", 
            show.legend = FALSE) +
  geom_text(aes(label = paste0("Floor: ", floor_pos)),
            vjust = -1) +
  geom_hline(yintercept = -1,
             color = "red",
             linetype = "dashed") +
  geom_text(aes(0, 0,
                label = paste0("Santa first enter the basement: ", which(floor_pos == -1)[1]), 
                vjust = 2,
                hjust = -.05)) +
  labs(title = "AoC 2015 Day 1 Puzzle",
       y = "Floor in the Apartment",
       x = "Number of Present")

g +
  transition_reveal(present_num) 

