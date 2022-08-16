# Day 16 AoC 2015
# Aunt Sue ----

library(dplyr)
library(stringr)

day16_data <- readLines("~/AoC/2015/data/day16_data")

day16_df <- data.frame(day16_data)

day16_df <- day16_df %>%
  dplyr::mutate(aunt_sue = as.numeric(str_match(str_match(day16_data, "Sue [0-9]+"), "[0-9]+")),
                children = as.numeric(str_match(str_match(day16_data, "children: [0-9]+"), "[0-9]+")),
                cats = as.numeric(str_match(str_match(day16_data, "cats: [0-9]+"), "[0-9]+")),
                samoyeds = as.numeric(str_match(str_match(day16_data, "samoyeds: [0-9]+"), "[0-9]+")),
                pomeranians = as.numeric(str_match(str_match(day16_data, "pomeranians: [0-9]+"), "[0-9]+")),
                akitas = as.numeric(str_match(str_match(day16_data, "akitas: [0-9]+"), "[0-9]+")),
                vizslas = as.numeric(str_match(str_match(day16_data, "vizslas: [0-9]+"), "[0-9]+")),
                goldfish = as.numeric(str_match(str_match(day16_data, "goldfish: [0-9]+"), "[0-9]+")),
                trees = as.numeric(str_match(str_match(day16_data, "trees: [0-9]+"), "[0-9]+")),
                cars = as.numeric(str_match(str_match(day16_data, "cars: [0-9]+"), "[0-9]+")),
                perfumes = as.numeric(str_match(str_match(day16_data, "perfumes: [0-9]+"), "[0-9]+"))
  )

# Part 1 ----
# What is the number of the Sue that got you the gift?

# children: 3
# children: 3
# cats: 7
# samoyeds: 2
# pomeranians: 3
# akitas: 0
# vizslas: 0
# goldfish: 5
# trees: 3
# cars: 2
# perfumes: 1

day16_df1 <- day16_df %>%
  dplyr::mutate(childfl = ifelse(children == 3, 1, 0),
                catfl = ifelse(cats == 7, 1, 0),
                saymfl = ifelse(samoyeds == 2, 1, 0), 
                pomfl = ifelse(pomeranians == 3, 1, 0),
                akifl = ifelse(akitas == 0, 1, 0),
                vizfl = ifelse(vizslas == 0, 1, 0),
                gfifl = ifelse(goldfish == 5, 1, 0), 
                trefl = ifelse(trees == 3, 1, 0),
                carfl = ifelse(cars == 2, 1, 0), 
                perfl = ifelse(perfumes == 1, 1, 0))

day16_df1[, 13:22][is.na(day16_df1[, 13:22])] <- 0  

day16_df1 <- day16_df1 %>%
  mutate(sum = (childfl + catfl + saymfl + pomfl + akifl + vizfl + gfifl + trefl + carfl + perfl))

cat("What is the number of the Sue that got you the gift: ",
    which.max(day16_df1$sum))

# Part 2 ---
# In particular, the cats and trees readings indicates that there are greater than that many 
# (due to the unpredictable nuclear decay of cat dander and tree pollen), while the pomeranians 
# and goldfish readings indicate that there are fewer than that many (due to the modial interaction 
# of magnetoreluctance).

# What is the number of the real Aunt Sue?

day16_df2 <- day16_df %>%
  dplyr::mutate(childfl = ifelse(children == 3, 1, 0),
                catfl = ifelse(cats > 7, 1, 0),
                saymfl = ifelse(samoyeds == 2, 1, 0), 
                pomfl = ifelse(pomeranians < 3, 1, 0),
                akifl = ifelse(akitas == 0, 1, 0),
                vizfl = ifelse(vizslas == 0, 1, 0),
                gfifl = ifelse(goldfish < 5, 1, 0), 
                trefl = ifelse(trees > 3, 1, 0),
                carfl = ifelse(cars == 2, 1, 0), 
                perfl = ifelse(perfumes == 1, 1, 0))

day16_df2[, 13:22][is.na(day16_df2[, 13:22])] <- 0  

day16_df2 <- day16_df2 %>%
  mutate(sum = (childfl + catfl + saymfl + pomfl + akifl + vizfl + gfifl + trefl + carfl + perfl))

cat("What is the number of the Sue that got you the gift: ",
    which.max(day16_df2$sum))