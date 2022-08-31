# ----
# Day 4
# Security Through Obscurity ---

library(stringr)
library(tidyverse)

# Read Puzzle Input
day4_data <- read.table(file = "~/AoC/2016/data/day4_data", sep=",")

day4_data <- day4_data %>%
  mutate(checksum = sub(".*\\[([^][]+)].*", "\\1", V1),
         enc_name = gsub("\\[.*" , "",
                         gsub("[0-9]+", "", 
                              gsub("-", "", V1))),
         sec_id = as.numeric(str_extract(V1, "[0-9]+")))

# Part 1 ---- 
# Find the Real IDs. & Sum for it

checksum <- day4_data$checksum
enc_name <- str_extract_all(day4_data$enc_name,"[a-z]")
sec_id <- day4_data$sec_id

real_id <- rep(NA, length(checksum))

for (i in 1:length(checksum)) {
  
  real_id[i] = paste(names(sort(table(enc_name[i]), decreasing=TRUE)[1:5]), collapse = "") == checksum[i]
    
}

paste("Sum of the sector IDs of the real rooms? ", sum(sec_id*real_id))

# Part 2 ----

# rotate each letter forward through the alphabet a number of times equal to the room's sector ID
# What is the sector ID of the room where North Pole objects are stored?

# Test String
#shift_time <- as.numeric(343 %% 26)
#enc_name <- str_extract_all("qzmtzixmtkozyivhz","[a-z]") 

enc_name2 <- gsub("\\[.*" , "", gsub("[0-9]+", "", day4_data$V1))
enc_name2 <- str_extract_all(enc_name2,"[a-z\\-]")

let_num <- setNames(seq_along(letters), letters)

real_name <- rep(NA, length(enc_name2)) 

for (i in 1:length(enc_name2)) {
  
  temp_var <- rep(NA, length(enc_name2[[i]]))
  
  for (j in 1:length(enc_name2[[i]])) {
    
    temp_var[j] <- ifelse(enc_name2[[i]][j] == "-", " ", 
                          names(which(let_num == max(1,(which(letters == enc_name2[[i]][j]) + sec_id[i]) %% 26))))
    
  }
  real_name[i] = paste(temp_var, collapse = "")
  
}

print(paste("Sector ID : " , sec_id[which(grepl("north", real_name))]))

