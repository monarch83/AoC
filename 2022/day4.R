# ----
# Day 4: Camp Cleanup ---

library(stringr)
library(dplyr)

# Read Puzzle Input
day4_data <- read.table(file = "~/AoC/2022/data/day4_data", blank.lines.skip=F)

head(day4_data)

# Test Data
# day4_data <- data.frame(V1 = rbind("2-4,6-8",
#                                    "2-3,4-5",
#                                    "5-7,7-9",
#                                    "2-8,3-7",
#                                    "6-6,4-6",
#                                    "2-6,4-8"))

# Part 1 ----

day4_data <- day4_data %>%
  mutate(p1_start = as.numeric(str_extract_all(day4_data$V1, "[0-9]+", simplify = TRUE)[,1]),
         p1_end = as.numeric(str_extract_all(day4_data$V1, "[0-9]+", simplify = TRUE)[,2]),
         p2_start = as.numeric(str_extract_all(day4_data$V1, "[0-9]+", simplify = TRUE)[,3]),
         p2_end = as.numeric(str_extract_all(day4_data$V1, "[0-9]+", simplify = TRUE)[,4]))

day4_data <- day4_data %>%
  mutate(fl_part1 = case_when((p2_start >= p1_start & p2_start <= p1_end) & (p2_end >= p1_start & p2_end <= p1_end) ~ "Y",
                              (p1_start >= p2_start & p1_start <= p2_end) & (p1_end >= p2_start & p1_end <= p2_end) ~ "Y",
                         TRUE ~ "N"))

cat("In how many assignment pairs does one range fully contain the other?", as.numeric(table(day4_data$fl_part1)["Y"]))             

# Part 2 ----
day4_data <- day4_data %>%
  mutate(fl_part2 = case_when((p2_start >= p1_start & p2_start <= p1_end) | (p2_end >= p1_start & p2_end <= p1_end) ~ "Y",
                              (p1_start >= p2_start & p1_start <= p2_end) | (p1_end >= p2_start & p1_end <= p2_end) ~ "Y",
                        TRUE ~ "N"))

cat("In how many assignment pairs do the ranges overlap?", as.numeric(table(day4_data$fl_part2)["Y"]))             
