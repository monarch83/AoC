# ----
# Day 2: Rock Paper Scissors ---

library(stringr)
library(dplyr)

# Read Puzzle Input
day2_data <- read.table(file = "~/AoC/2022/data/day2_data", blank.lines.skip=F)

# Elf's : A - Rock, B - Paper, C - Scissors 
# Player's : X - Rock [1], Y - Paper [2], Z - Scissors [3]

# day2_data <- data.frame(cbind(V1 = c("A", "B", "C"), V2 = c("Y", "X", "Z")))

head(day2_data)

# Part 1 ----
# What would your total score be if everything goes exactly according to your strategy guide?

day2_data <- day2_data %>%
  mutate(scr_shape = case_when(
    V2 == "X" ~ 1,
    V2 == "Y" ~ 2,
    V2 == "Z" ~ 3
  ))

day2_data <- day2_data %>%
  mutate(scr_gm = ifelse((V1 == "A" & V2 == "X") | (V1 == "B" & V2 == "Y") | (V1 == "C" & V2 == "Z"), 3,
                         ifelse((V1 == "A" & V2 == "Z") | (V1 == "C" & V2 == "Y") | (V1 == "B" & V2 == "X"), 0, 6)))

cat("What would your total score be if everything goes exactly according to your strategy guide?: ",
    sum(day2_data$scr_shape) + sum(day2_data$scr_gm))
  
# Part 2 ----
# what would your total score be if everything goes exactly according to your strategy guide?
# Elf's : A - Rock [1], B - Paper [2], C - Scissors [3]
# Round ends: X = You Loose, Y = Draw, Z = Win
# Rock defeats Scissors, Scissors defeats Paper, and Paper defeats Rock.

day2_data <- day2_data %>%
  mutate(scr_gm_pt2 = case_when(
    V2 == "X" ~ 0,
    V2 == "Y" ~ 3,
    V2 == "Z" ~ 6),
    scr_shape_pt2 = case_when(
      V1 == "A" & V2 == "X" ~ 3,
      V1 == "A" & V2 == "Y" ~ 1,
      V1 == "A" & V2 == "Z" ~ 2,
      V1 == "B" & V2 == "X" ~ 1,
      V1 == "B" & V2 == "Y" ~ 2,
      V1 == "B" & V2 == "Z" ~ 3,
      V1 == "C" & V2 == "X" ~ 2,
      V1 == "C" & V2 == "Y" ~ 3,
      V1 == "C" & V2 == "Z" ~ 1
    ))

cat("what would your total score be if everything goes exactly according to your strategy guide?: ",
    sum(day2_data$scr_shape_pt2) + sum(day2_data$scr_gm_pt2))
