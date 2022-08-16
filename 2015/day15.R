# AoC 2015 Day 15
# Science for Hungry People ----

# Your recipe leaves room for exactly 100 teaspoons of ingredients. 
#  You make a list of the remaining ingredients you could use to finish the recipe (your puzzle input) 
#  and their properties per teaspoon:
# 
#  capacity (how well it helps the cookie absorb milk)
#  durability (how well it keeps the cookie intact when full of milk)
#  flavor (how tasty it makes the cookie)
#  texture (how it improves the feel of the cookie)
#  calories (how many calories it adds to the cookie)
#
# The total score of a cookie can be found by adding up each of the properties (negative totals become 0) 
#  and then multiplying together everything except calories

library(dplyr)
library(stringr)

day15_data <- readLines("~/AoC/2015/data/day15_data")

day15_df <- data.frame(day15_data)

day15_df <- day15_df %>%
  dplyr::mutate(ingredient = stringr::str_extract(day15_data, "\\w*"),
                capacity = as.numeric(sapply(day15_data, function(x) stringr::str_extract_all(x, "[-0-9]+")[[1]][1])),
                durability = as.numeric(sapply(day15_data, function(x) stringr::str_extract_all(x, "[-0-9]+")[[1]][2])),
                flavor = as.numeric(sapply(day15_data, function(x) stringr::str_extract_all(x, "[-0-9]+")[[1]][3])),
                texture = as.numeric(sapply(day15_data, function(x) stringr::str_extract_all(x, "[-0-9]+")[[1]][4])),
                calories = as.numeric(sapply(day15_data, function(x) stringr::str_extract_all(x, "[-0-9]+")[[1]][5]))
                )
day15_df <- day15_df %>%
  arrange(ingredient)

capacity <- day15_df$capacity
durability <- day15_df$durability
flavor <- day15_df$flavor
texture <- day15_df$texture
calories <- day15_df$calories

day15_comb <- expand_grid(butterscotch = 1:100,
                          candy = 1:100,
                          frosting = 1:100,
                          sugar = 1:100)

day15_comb <- day15_comb %>%
  filter((butterscotch + candy + frosting + sugar) == 100)

day15_comb <- day15_comb %>%
  mutate(
    cap  = (butterscotch*capacity[1]  + candy*capacity[2] + frosting*capacity[3] + sugar*capacity[4] ),
    dur  = (butterscotch*durability[1] + candy*durability[2]  + frosting*durability[3]  + sugar*durability[4] ),
    flav = (butterscotch*flavor[1]  + candy*flavor[2] + frosting*flavor[3]  + sugar*flavor[4]),
    text = (butterscotch*texture[1]  + candy*texture[2] + frosting*texture[3]  + sugar*texture[4]),
    cal  = (butterscotch*calories[1]  + candy*calories[2]  + frosting*calories[3]  + sugar*calories[4] ),
    
    cap = ifelse(cap > 0,  cap,  0),
    dur = ifelse(dur > 0,  dur,  0),
    flav= ifelse(flav > 0, flav, 0),
    text= ifelse(text > 0, text, 0),
    cal = ifelse(cal > 0,  cal,  0),
    
    part1 = (cap*dur*flav*text))


# Part 1 ----
# Given the ingredients in your kitchen and their properties, what is the total score of 
#  the highest-scoring cookie you can make?

day15_comb[which.max(day15_comb$part1),]

# Part 2 ---
# Given the ingredients in your kitchen and their properties, 
# what is the total score of the highest-scoring cookie you can make with a calorie total of 500?

part2 <- day15_comb %>%
  filter(cal == 500)

part2[which.max(part2$part1),]
