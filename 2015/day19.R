# Day 19 AoC 2015
# Medicine for Rudolph ----

library(dplyr)
library(stringr)
library(gtools)

day19_data <- readLines("~/AoC/2015/data/day19_data")

# Example 
# med_mol <- "HOH"
# med_mol <- "HOHOHO"
# repl_left <- c("H", "H", "O")
# repl_right <- c("HO", "OH", "HH")

# Your puzzle input describes all of the possible replacements and, at the bottom, 
# the medicine molecule for which you need to calibrate the machine. 
# How many distinct molecules can be created after all the different ways you can
# do one replacement on the medicine molecule?

med_mol <- day19_data[45]

repl_mol <- day19_data[1:43]

repl_left <- sapply(repl_mol, function(x) str_extract_all(x, "[a-zA-z]+")[[1]][1])

repl_right <- sapply(repl_mol, function(x) str_extract_all(x, "[a-zA-z]+")[[1]][2])

names(repl_left) <- NULL
names(repl_right) <- NULL


# PART 1 ----
# our puzzle input describes all of the possible replacements and, at the bottom, 
# the medicine molecule for which you need to calibrate the machine. How many distinct molecules 
# can be created after all the different ways you can do one replacement on the medicine molecule?

# Replace Each Left Side replacement with the medicine molecule, Save the resulting molecule
# once completed, Get the disticnt medicine molecule
new_med_mol_list <- list()

for (i in 1:length(repl_left)) {
  
  if (length(str_extract_all(med_mol, repl_left[i])[[1]]) > 0) {
    
    for (j in 1:length(str_extract_all(med_mol, repl_left[i])[[1]])) {
      
      str_locator <- str_locate_all(med_mol, repl_left[i])[[1]]
      
      new_med_mol <- med_mol
      str_sub(new_med_mol, start = str_locator[j, "start"], end = str_locator[j, "end"]) <- repl_right[i]
      
      # print(new_med_mol)
      new_med_mol_list <- append(new_med_mol_list, new_med_mol)
    
      new_med_mol <- NA
      
    }
    
  }
  
}

cat("How many distinct molecules can be created after all the different ways you can do one replacement on the medicine molecule? ",
    length(unique(unlist(new_med_mol_list, use.names = FALSE))))

# PART 2 ----
# How long will it take to make the medicine? Given the available replacements and the medicine 
# molecule in your puzzle input, what is the fewest number of steps to go from e to the medicine molecule?

rev_keys <- split(repl_left, repl_right)

count <- 0

repeat {
  if (med_mol == "e") break
  
  r_key <- sample(rev_keys, 1)
  
  
  if (str_detect(med_mol, names(r_key))) {
    count <- count + str_count(med_mol, names(r_key))
    med_mol <- str_replace_all(med_mol, names(r_key), r_key[[1]])
  }

}
