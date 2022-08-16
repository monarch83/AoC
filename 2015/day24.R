# AoC 2015 Day 24
# It Hangs in the Balance ----

library(dplyr)
library(stringr)
library(gtools)

day24_data <- as.numeric(readLines("~/AoC/2015/data/day24_data"))

# day24_data <- c(1:5, 7:11)

# Part 1 ----

tot_wght <- sum(day24_data)/3

pckg_wght <- day24_data
pckg_loc <- seq_along(pckg_wght)

pckg_combo_list <- list()

# Create diff combo & only keep the one where weight is 1/3 of total weight, and
# smallest group
for (i in 1:length(pckg_wght)) {
  
  pckg_combo <- combn(pckg_wght, i)
  # print(pckg_combo)
  # print(colSums(pckg_combo))
  if (any(colSums(pckg_combo) == tot_wght)) {

    # Only keep when the total weight in the grp is same as the 1/3 of everything
    pckg_combo_list <- c(pckg_combo_list, list(pckg_combo[, which(colSums(pckg_combo) == tot_wght)]))
    break
  }
  
}

# Compute Quantum Entaglement 

pckg_seq_list <- list()

for (i in 1:length(pckg_combo_list)) {
  
  if (any(class(pckg_combo_list[[i]]) == "matrix")) {
    
    for (j in 1:ncol(pckg_combo_list[[i]])) {
      
      pckg_seq_list <- c(pckg_seq_list, prod(pckg_combo_list[[i]][,j]))
    }
  }
}

df <- data.frame(unlist(pckg_seq_list))

colnames(df) <- c("seq")

df <- df %>%
  arrange(seq)

cat("What is the quantum entanglement of the first group of packages in the ideal configuration, 3 group ?",
    min(df$seq))

# Part 2 ----

tot_wght <- sum(day24_data)/4

pckg_wght <- day24_data
pckg_loc <- seq_along(pckg_wght)

pckg_combo_list <- list()

# Create diff combo & only keep the one where weight is 1/3 of total weight
for (i in 1:length(pckg_wght)) {
  
  pckg_combo <- combn(pckg_wght, i)
  # print(pckg_combo)
  # print(colSums(pckg_combo))
  if (any(colSums(pckg_combo) == tot_wght)) {
    
    # Only keep when the total weight in the grp is same as the 1/3 of everything
    pckg_combo_list <- c(pckg_combo_list, list(pckg_combo[, which(colSums(pckg_combo) == tot_wght)]))
    break
  }
  
}

pckg_seq_list <- list()

for (i in 1:length(pckg_combo_list)) {
  
  if (any(class(pckg_combo_list[[i]]) == "matrix")) {
    
    for (j in 1:ncol(pckg_combo_list[[i]])) {
      
      pckg_seq_list <- c(pckg_seq_list, prod(pckg_combo_list[[i]][,j]))
    }
  }
}

df <- data.frame(unlist(pckg_seq_list))

colnames(df) <- c("seq")

df <- df %>%
  arrange(seq)

cat("What is the quantum entanglement of the first group of packages in the ideal configuration, 4 group ?",
    min(df$seq))

