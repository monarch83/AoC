# AoC 2015 Day 14
# Reindeer Olympics ----

# Reindeer can only either be flying (always at their top speed) or resting (not moving at all), 
# and always spend whole seconds in either state.

library(dplyr)
library(stringr)
library(tidyr)

day14_data <- readLines("~/AoC/2015/data/day14_data")

day14_df <- data.frame(day14_data)

day14_df <- day14_df %>%
  dplyr::mutate(rend_nm = stringr::str_extract(day14_data, "\\w*"),
                speed = as.numeric(sapply(day14_data, function(x) stringr::str_extract_all(x, "[0-9]+")[[1]][1])),
                travel = as.numeric(sapply(day14_data, function(x) stringr::str_extract_all(x, "[0-9]+")[[1]][2])),
                rest = as.numeric(sapply(day14_data, function(x) stringr::str_extract_all(x, "[0-9]+")[[1]][3]))
  )

# Part 1 ----
# Given the descriptions of each reindeer (in your puzzle input), after exactly 2503 seconds, 
#  What distance has the winning reindeer traveled?

day14_df1 <- day14_df %>%
  mutate(final_time = 2503,
         min_remainder = pmin(travel, (final_time %% (travel+rest))),
         tot_dist = ((speed * travel * floor(final_time/(travel + rest))) + (speed * min_remainder))
         )
cat("What distance has the winning reinder traveled after 2503 Seconds: ", 
    paste0(day14_df1[which.max(day14_df1$tot_dist),"rend_nm"], " ", max(day14_df1$tot_dist)))

# Part 2 ----
# New scoring system
# Instead, at the end of each second, he awards one point to the reindeer currently in the lead. 
#  (If there are multiple reindeer tied for the lead, they each get one point.)
# Again given the descriptions of each reindeer (in your puzzle input), after exactly 2503 seconds, 
# how many points does the winning reindeer have?

day14_df <- day14_df %>%
  arrange(rend_nm)

day14_df2 <- as.data.frame(lapply(day14_df, rep, 2503))

day14_df2 <- day14_df2 %>%
  arrange(rend_nm) %>%
  group_by(rend_nm) %>%
  mutate(seconds = row_number())

dist_trvl_fun <- function(speed, travel, rest, time = 2503) {
  
  return(cumsum(rep(c(rep(speed, travel), rep(0, rest)), length.out = time)))
  
}

dist_trvl <- mapply(dist_trvl_fun, day14_df$speed, day14_df$travel, day14_df$rest)

day14_df2$dist_trvl <-  as.vector(dist_trvl)
  
# Make Wide Dataset
day14_wide <- day14_df2 %>%
  tidyr::pivot_wider(id_cols = "seconds",
                     names_from = "rend_nm",
                     values_from = "dist_trvl")

day14_wide <- day14_wide %>%
  mutate(Blitzen_point = ifelse(pmax(Blitzen, Comet, Cupid, Dancer, Dasher, Donner, Prancer, Rudolph, Vixen) == Blitzen, 1, 0),
         Comet_point = ifelse(pmax(Blitzen, Comet, Cupid, Dancer, Dasher, Donner, Prancer, Rudolph, Vixen) == Comet, 1, 0),
         Cupid_point = ifelse(pmax(Blitzen, Comet, Cupid, Dancer, Dasher, Donner, Prancer, Rudolph, Vixen) == Cupid, 1, 0),
         Dancer_point = ifelse(pmax(Blitzen, Comet, Cupid, Dancer, Dasher, Donner, Prancer, Rudolph, Vixen) == Dancer, 1, 0),
         Dasher_point = ifelse(pmax(Blitzen, Comet, Cupid, Dancer, Dasher, Donner, Prancer, Rudolph, Vixen) == Dasher, 1, 0),
         Donner_point = ifelse(pmax(Blitzen, Comet, Cupid, Dancer, Dasher, Donner, Prancer, Rudolph, Vixen) == Donner, 1, 0),
         Prancer_point = ifelse(pmax(Blitzen, Comet, Cupid, Dancer, Dasher, Donner, Prancer, Rudolph, Vixen) == Prancer, 1, 0),
         Rudolph_point = ifelse(pmax(Blitzen, Comet, Cupid, Dancer, Dasher, Donner, Prancer, Rudolph, Vixen) == Rudolph, 1, 0),
         Vixen_point = ifelse(pmax(Blitzen, Comet, Cupid, Dancer, Dasher, Donner, Prancer, Rudolph, Vixen) == Vixen, 1, 0)
         )

day14_summary <- day14_wide %>%
  summarise(Blitzen = sum(Blitzen_point),
            Comet = sum(Comet_point),
            Cupid = sum(Cupid_point),
            Dancer = sum(Dancer_point),
            Dasher = sum(Dasher_point),
            Donner = sum(Donner_point),
            Prancer = sum(Prancer_point),
            Rudolph = sum(Rudolph_point),
            Vixen = sum(Vixen_point)
  )

cat("after exactly 2503 seconds, how many points does the winning reindeer have? ", names(which.max(day14_summary)), "Points: ", max(day14_summary))