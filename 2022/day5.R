# ----
# Day 5: Supply Stacks ---

library(stringr)
library(dplyr)

# Read Puzzle Input
day5_data <- readLines("~/AoC/2022/data/day5_data")

head(day5_data)

# Test case
# cargo <- list(c("Z", "N"),
#               c("M", "C", "D"),
#               c("P"))
# reang_proc <- data.frame(V1 = c("move 1 from 2 to 1", "move 3 from 1 to 3", "move 2 from 2 to 1", "move 1 from 1 to 2"))

# Get Info about starting stacks
cargo <- list(c("Z", "J", "N", "W", "P", "S"),
              c("G", "S", "T"),
              c("V", "Q", "R", "L", "H"),
              c("V", "S", "T", "D"),
              c("Q", "Z", "T", "D", "B", "M", "J"),
              c("M", "W", "T", "J", "D", "C", "Z", "L"),
              c("L", "P", "M", "W", "G", "T", "J"),
              c("N", "G", "M", "T", "B", "F", "Q", "H"),
              c("R", "D", "G", "C", "P", "B", "Q", "W"))

# Get Info about rearrangment procedures
reang_proc <- as.data.frame(day5_data[str_detect(day5_data, "move")])
colnames(reang_proc) <- "V1"

reang_proc <- reang_proc %>%
  mutate(move = as.numeric(str_extract_all(V1, "[0-9]+", simplify = TRUE)[,1]),
         crt_s = as.numeric(str_extract_all(V1, "[0-9]+", simplify = TRUE)[,2]),
         crt_e = as.numeric(str_extract_all(V1, "[0-9]+", simplify = TRUE)[,3])
  )

# Part 1 ----

for (i in 1:nrow(reang_proc)) {
  
  cat("Ieration :", i,":", reang_proc$V1[i], "\n")
  
  # Add crate to specific cargo at top
  cargo[[reang_proc$crt_e[[i]]]] <- append(cargo[[reang_proc$crt_e[[i]]]],
                                           cargo[[reang_proc$crt_s[[i]]]][length(cargo[[reang_proc$crt_s[[i]]]]):(length(cargo[[reang_proc$crt_s[[i]]]])-reang_proc$move[[i]]+1)])
  
  # Remove crate once its been added
  cargo[[reang_proc$crt_s[[i]]]] <- cargo[[reang_proc$crt_s[[i]]]][-(length(cargo[[reang_proc$crt_s[[i]]]]):(length(cargo[[reang_proc$crt_s[[i]]]])-reang_proc$move[[i]]+1))]
  
  # print(cargo)

}

crt_top <- paste0(cargo[[1]][length(cargo[[1]])],
                 cargo[[2]][length(cargo[[2]])],
                 cargo[[3]][length(cargo[[3]])],
                 cargo[[4]][length(cargo[[4]])],
                 cargo[[5]][length(cargo[[5]])],
                 cargo[[6]][length(cargo[[6]])],
                 cargo[[7]][length(cargo[[7]])],
                 cargo[[8]][length(cargo[[8]])],
                 cargo[[9]][length(cargo[[9]])])

cat("After the rearrangement procedure completes, what crate ends up on top of each stack?", crt_top)

# Part 2 ----
# Get Info about starting stacks
cargo <- list(c("Z", "J", "N", "W", "P", "S"),
              c("G", "S", "T"),
              c("V", "Q", "R", "L", "H"),
              c("V", "S", "T", "D"),
              c("Q", "Z", "T", "D", "B", "M", "J"),
              c("M", "W", "T", "J", "D", "C", "Z", "L"),
              c("L", "P", "M", "W", "G", "T", "J"),
              c("N", "G", "M", "T", "B", "F", "Q", "H"),
              c("R", "D", "G", "C", "P", "B", "Q", "W"))

for (i in 1:nrow(reang_proc)) {
  
  cat("Ieration :", i,":", reang_proc$V1[i], "\n")
  
  # Add crate to specific cargo at top
  cargo[[reang_proc$crt_e[[i]]]] <- append(cargo[[reang_proc$crt_e[[i]]]],
                                           cargo[[reang_proc$crt_s[[i]]]][(length(cargo[[reang_proc$crt_s[[i]]]])-reang_proc$move[[i]]+1):length(cargo[[reang_proc$crt_s[[i]]]])])
  
  # Remove crate once its been added
  cargo[[reang_proc$crt_s[[i]]]] <- cargo[[reang_proc$crt_s[[i]]]][-(length(cargo[[reang_proc$crt_s[[i]]]]):(length(cargo[[reang_proc$crt_s[[i]]]])-reang_proc$move[[i]]+1))]
  
  # print(cargo)
  
}

crt_top2<- paste0(cargo[[1]][length(cargo[[1]])],
                 cargo[[2]][length(cargo[[2]])],
                 cargo[[3]][length(cargo[[3]])],
                 cargo[[4]][length(cargo[[4]])],
                 cargo[[5]][length(cargo[[5]])],
                 cargo[[6]][length(cargo[[6]])],
                 cargo[[7]][length(cargo[[7]])],
                 cargo[[8]][length(cargo[[8]])],
                 cargo[[9]][length(cargo[[9]])])

cat("After the rearrangement procedure completes, what crate ends up on top of each stack?", crt_top2)
