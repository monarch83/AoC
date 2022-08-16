# AoC 2015 Day 22
# Wizard Simulator 20XX ----

library(dplyr)
library(stringr)
library(gtools)

boss_data <- readLines("~/AoC/2015/data/day22_data")

player_hit <- 50
player_mana <- 500

spell_data <- data.frame(name = c("Magic Missile", "Drain", "Shield", "Poison", "Recharge"),
                         cost = c(53, 73, 113, 173, 229),
                         damage = c(4, 2, 0, 3, 0),
                         armor = c(0, 0, 7, 0, 0),
                         heal = c(0, 2, 0, 0, 0),
                         timer = c(0, 0, 6, 6, 5),
                         mana = c(0, 0, 0, 0, 101))

# Part 1 ----
# What is the least amount of mana you can spend and still win the fight? 
# (Do not include mana recharge effects as "spending" negative mana.)

spell_combo <- expand.grid(rep(list(spell_data$name), 15))
