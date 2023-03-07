# ----
# Day 1: Sonar Sweep ---

# Part 1 ----
# How many measurements are larger than the previous measurement?

library(stringr)

# Read Puzzle Input
day1_data <- read.table(file = "~/AoC/2021/data/day1_data", blank.lines.skip=F)

day1_data <- day1_data %>%
  mutate(lag_prev = lag(V1),
         inc_fl = ifelse(V1 > lag_prev, "Y", "N"))

cat(paste0("How many measurments are larger than the previous measurments: ", table(day1_data$inc_fl)[2]))

# Part 2 ----
# Consider sums of a three-measurement sliding window. How many sums are larger than the previous sum?

day1_data <- day1_data %>%
  mutate(lead_1 = lead(V1, n=1),
         lead_2 = lead(V1, n=2),
         roll3_sum = rowSums(select(., "V1", "lead_1", "lead_2")),
         lag2_prev = lag(roll3_sum),
         inc2_fl = ifelse(roll3_sum > lag2_prev, "Y", "N"))

cat(paste0("How many sums are larger than the previous sum?: ", table(day1_data$inc2_fl)[2]))
