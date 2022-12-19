# ----
# Day 3: Rucksack Reorganization ---

library(stringr)
library(dplyr)

# Read Puzzle Input
day3_data <- read.table(file = "~/AoC/2022/data/day3_data", blank.lines.skip=F)

head(day3_data)

# Part 1 ----
# Find the item type that appears in both compartments of each rucksack. 
#  What is the sum of the priorities of those item types?

# Test
# day3_data <- as.data.frame(rbind("vJrwpWtwJgWrhcsFMMfFFhFp","jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
#                                  "PmmdzqPrVvPwwTWBwg", "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
#                                  "ttgJtRGJQctTZtZT", "CrZsJsPPZsGzwwsLwLmpwMDw"))
# colnames(day3_data) <- "V1"


# Split String into two parts & find common letter

letter_str <- c(letters[1:26], LETTERS[1:26])

day3_data <- day3_data %>%
  rowwise() %>%
  mutate(intersect = as.character(list(intersect(unlist(strsplit(stringr::str_sub(V1, 1, nchar(V1)/2), split=character(0))), 
                      unlist(strsplit(stringr::str_sub(V1, nchar(V1)/2+1, nchar(V1)), split=character(0)))))),
         priority = which(intersect == letter_str))

cat("Find the item type that appears in both compartments of each rucksack. What is the sum of the priorities of those item types?" , sum(day3_data$priority))

# Part 2 ----
# Find the item type that corresponds to the badges of each three-Elf group. What is the sum of the priorities of those item types?

group <- rep(1:3,nrow(day3_data)/3)

day3_data <- cbind(day3_data, group)

day3_data2 <- cbind(day3_data %>% filter(group == 1) %>% select(V1) %>% rename(group1 = V1),
                    day3_data %>% filter(group == 2) %>% select(V1) %>% rename(group2 = V1),
                    day3_data %>% filter(group == 3) %>% select(V1) %>% rename(group3 = V1))

day3_data2 <- day3_data2 %>% 
  rowwise() %>%
  mutate(intersect = as.character(list(intersect(intersect(unlist(strsplit(group1, split=character(0))), 
                                                 unlist(strsplit(group2, split=character(0)))),
                                                 unlist(strsplit(group3, split=character(0)))))),
         priority = which(intersect == letter_str))

cat("Find the item type that corresponds to the badges of each three-Elf group. What is the sum of the priorities of those item types?" , sum(day3_data2$priority))
