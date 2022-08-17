# ----
# Day 3
# Squares With Three Sides ---

library(stringr)
library(tidyverse)

# Read Puzzle Input
day3_data <- read.table(file = "~/AoC/2016/data/day3_data", sep=",")

day3_data <- day3_data %>%
  mutate(side1 = as.numeric(stringr::str_extract_all(V1, "[0-9]+", simplify = TRUE)[,1]),
         side2 = as.numeric(stringr::str_extract_all(V1, "[0-9]+", simplify = TRUE)[,2]),
         side3 = as.numeric(stringr::str_extract_all(V1, "[0-9]+", simplify = TRUE)[,3]),
         side12 = (side1 + side2),
         side23 = (side2 + side3),
         side13 = (side1 + side3),
         trifl = ifelse((side12 > side3) & (side23 > side1) & (side13 > side2), 1, 0)) %>%
  filter(!is.na(side1))

paste0("how many of the listed triangles are possible? ", table(day3_data$trifl)["1"])

# Part 2 ----

# Use Columns instead of Rows

day3_part2 <- data.frame(c(as.numeric(day3_data$side1), 
                           as.numeric(day3_data$side2), 
                           as.numeric(day3_data$side3)))

colnames(day3_part2) <- c("tri_list")

day3_part2 <- day3_part2 %>%
  mutate(key = rep(c("side1", "side2", "side3"), n()/3),
         num = ceiling(row_number()/3)) %>%
  pivot_wider(names_from = key, values_from = tri_list)

day3_part2 <- day3_part2 %>%
  mutate(side12 = (side1 + side2),
         side23 = (side2 + side3),
         side13 = (side1 + side3),
         trifl = ifelse((side12 > side3) & (side23 > side1) & (side13 > side2), 1, 0)) 

paste0("how many of the listed triangles are possible? ", table(day3_part2$trifl)["1"])
