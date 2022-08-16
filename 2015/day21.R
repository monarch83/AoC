# AoC 2015 Day 21
# RPG Simulator 20XX ----

library(dplyr)
library(stringr)
library(gtools)

boss_data <- readLines("~/AoC/2015/data/day21_data")

player_hit <- 100

# Player start with 100 HIT point 
weapons_data <- data.frame(name = c("Dagger", "Shortsword", "Warhammer", "Longsword", "Greataxe"),
                           cost = c(8, 10, 25, 40, 74),
                           damage = c(4, 5, 6, 7, 8),
                           armor = c(0, 0, 0, 0, 0))

armor_data <- data.frame(name = c("Leather", "Chainmail", "Splitmail", "Bandedmail", "Platemail", "No armor"),
                         cost = c(13, 31, 53, 75, 102, 0),
                         damage = c(0, 0, 0, 0, 0, 0),
                         armor = c(1, 2, 3, 4, 5, 0))

rings_data <- data.frame(name = c("Damage +1", "Damage +2", "Damage +3", "Defense +1", "Defense +2", "Defense +3", "No ring"),
                         cost = c(25, 50, 100, 20, 40, 80, 0),
                         damage = c(1, 2, 3, 0, 0, 0, 0),
                         armor = c(0, 0, 0, 1, 2, 3, 0))

eqp_combo <- expand.grid(weapons_data$name, armor_data$name, rings_data$name, rings_data$name) %>%
  # Only 1 Item available in store
  dplyr::filter(Var3 != Var4)

colnames(eqp_combo) <- c("weapons", "armor", "rings1", "rings2")

# Figure out for each combination Cost, Damage, Armor

cost_fun <- function(wpn, arm, ring1, ring2) {
  
  cost<- 
    weapons_data[which.max(weapons_data$name == wpn), "cost"] +
    armor_data[which.max(armor_data$name == arm), "cost" ] + 
    rings_data[which.max(rings_data$name == ring1), "cost" ] + 
    rings_data[which.max(rings_data$name == ring2), "cost" ] 
  
  return(cost)
  
}

dmg_fun <- function(wpn, arm, ring1, ring2) {
  
  dmg<- 
    weapons_data[which.max(weapons_data$name == wpn), "damage"] +
    armor_data[which.max(armor_data$name == arm), "damage" ] + 
    rings_data[which.max(rings_data$name == ring1), "damage" ] + 
    rings_data[which.max(rings_data$name == ring2), "damage" ] 
  
  return(dmg)
  
}

arm_fun <- function(wpn, arm, ring1, ring2) {
  
  arm<- 
    weapons_data[which.max(weapons_data$name == wpn), "armor"] +
    armor_data[which.max(armor_data$name == arm), "armor" ] + 
    rings_data[which.max(rings_data$name == ring1), "armor" ] + 
    rings_data[which.max(rings_data$name == ring2), "armor" ] 
  
  return(arm)
  
}

eqp_combo <- eqp_combo %>%
  mutate(cost = mapply(cost_fun, weapons, armor, rings1, rings2),
         damage = mapply(dmg_fun, weapons, armor, rings1, rings2),
         armorv =  mapply(arm_fun, weapons, armor, rings1, rings2),
         plyr_hit = player_hit,
         boss_hit = as.numeric(stringr::str_extract_all(boss_data, "[0-9]+")[[1]]),
         boss_dmg = as.numeric(stringr::str_extract_all(boss_data, "[0-9]+")[[2]]),
         boss_arm = as.numeric(stringr::str_extract_all(boss_data, "[0-9]+")[[3]])
         )

# Part 1 ----
# You have 100 hit points. The boss's actual stats are in your puzzle input. 
# What is the least amount of gold you can spend and still win the fight?

# Write loop or something to figure out at each level who won, find the one where Player won, and find the one with 
# lowest cost

game_win_fun <- function(bhit, phit, bdmg, pdmg, barm, parm) {
  repeat {
 
    if (bhit > 0) {
      
      if (barm > pdmg) tmp_hit_boss <- bhit - 1
      else tmp_hit_boss <- bhit - (pdmg - barm)
    }
    if (phit > 0) {
      
      if (parm > bdmg) tmp_hit_plyr <- phit - 1
      else tmp_hit_plyr <- phit - (bdmg - parm)
    
    }
    
    bhit <- tmp_hit_boss
    phit <- tmp_hit_plyr
    
    if (tmp_hit_boss <= 0 | tmp_hit_plyr <= 0) {
      return(ifelse(tmp_hit_boss <= 0, "Player", "Boss"))
      break
    }
    cat(paste0("Boss HIT: ", tmp_hit_boss, " Player HIT: ", tmp_hit_plyr, "\n"))
  }
}
  
eqp_combo <- eqp_combo %>%
  mutate(winner =  mapply(game_win_fun, boss_hit, plyr_hit, boss_dmg, damage, boss_arm, armorv))
   
eqp_combo <- eqp_combo %>%
  arrange(cost)

cat("What is the least amount of gold you can spend and still win the fight? ",
    (eqp_combo %>% filter(winner == "Player") %>% slice_min(n = 1, cost) %>% pull(cost))[1])

# Part 2 ----
# What is the most amount of gold you can spend and still lose the fight?

cat("What is the most amount of gold you can spend and still lose the fight? ",
    (eqp_combo %>% filter(winner == "Boss") %>% slice_max(n = 1, cost) %>% pull(cost))[1])
