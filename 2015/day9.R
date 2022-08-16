# Day 9 AoC
# All in a Single Night

library(dplyr)
library(stringr)
library(readr)
library(tidyr)
library(gtools)

# Read Day 9 Data
day9_data <- readLines("~/AoC/2015/data/day9_data")

day9_df <- data.frame(day9_data)

# Break the string to create Location 1, Location 2 and Distance columns
day9_df <- day9_df %>%
  tidyr::separate(day9_data,
                  c("loc1", "2", "loc2", "4", "dist"), 
                  sep = ' ')

day9_df <- day9_df %>%
  dplyr::select(-"2", -"4") %>%
  mutate(loc1 = trimws(loc1),
         loc2 = trimws(loc2),
         dist = trimws(dist))
  
loc_list <- c(paste0(day9_df$loc2, "_", day9_df$loc1),
              paste0(day9_df$loc1, "_", day9_df$loc2))

dist_list <- c(as.numeric(day9_df$dist),
               as.numeric(day9_df$dist))

# City Vector
cities_vec <- as.vector(names(table(rbind(day9_df$loc1, day9_df$loc2))))

# Permutation of 8 cities
perm_vec <- gtools::permutations(n=length(cities_vec), 
                                 r=length(cities_vec), 
                                 v=cities_vec)

# Create Matrix cell with two cities 
perm_dist_vec <- matrix(NA, nrow = nrow(perm_vec), ncol=length(cities_vec) - 1)

for (j in 1:(length(cities_vec) - 1)) {
  for (i in 1:nrow(perm_vec)) {
    perm_dist_vec[i, j] = paste0(perm_vec[i, j], "_", perm_vec[i, j+1])  
  }
}

# Matrix to fill distance btw two cities 
perm_distl_vec <- matrix(NA, nrow = nrow(perm_vec), ncol=length(cities_vec) - 1)

# Fill in the distance 
for (i in 1:56) {
  for (j in 1:(length(cities_vec) - 1)) {
    for (k in 1:nrow(perm_vec)) {
      if (loc_list[i] == perm_dist_vec[k,j]) {
        perm_distl_vec[k,j] = dist_list[i]
      }
    }
  }
}

# Add Sum column to matrix 
perm_dist_vec <- cbind(perm_dist_vec, rowSums(perm_distl_vec))

# Part 1 ----
# What is the shortest distance he can travel to achieve this?
cat("Shortest distance Santa can travel: ", 
    perm_dist_vec[which(perm_dist_vec[, 8] == min(perm_dist_vec[, 8]), arr.ind=TRUE), ][1, ])

# Part 2 ----
# What is the distance of the longest route?
cat("Distance of the longest route: ", 
    perm_dist_vec[which(perm_dist_vec[, 8] == max(perm_dist_vec[, 8]), arr.ind=TRUE), ][1, ])


