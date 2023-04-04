# ----
# Day 1  Report Repair ---

library(stringr)
library(dplyr)

# Read Puzzle Input
day1_data <- read.table(file = "AoC/2020/data/day1_data")

day1_data <- day1_data$V1

for (i in 1:length(day1_data)) {

  for (j in min((i+1),length(day1_data)):length(day1_data)) {

   if (day1_data[i] + day1_data[j] == 2020) {
     
     cat(paste0("The two entries that sum to 2020 are ",
                day1_data[i], " and ", day1_data[j], ". Multiplying them together produces ",
                day1_data[i] * day1_data[j]))
     
    }
    
  }
  
}


# Part 2 ----
for (i in 1:length(day1_data)) {
  
  for (j in min((i+1),length(day1_data)):length(day1_data)) {
    
    for (k in min((j+1), length(day1_data)):length(day1_data)) {
      
      if (day1_data[i] + day1_data[j] + day1_data[k] == 2020) {
        
        cat(paste0("The three entries that sum to 2020 are ",
                   day1_data[i], day1_data[j], " and ", day1_data[k], ". Multiplying them together produces ",
                   day1_data[i] * day1_data[j] * day1_data[k]))
        
      }
    
    }
    
  }
  
}

# Solution 2 ----

day1_2 <- expand.grid(day1_data, day1_data)
day1_2 <- day1_2 %>%
  mutate(fl_2020 = ifelse(Var1 + Var2 == 2020, 1, 0)) %>%
  filter(fl_2020 == 1)

cat(paste0("The two entries that sum to 2020 are ",
           day1_2$Var1[1], " and ", day1_2$Var2[1], ". Multiplying them together produces ",
           day1_2$Var1[1] * day1_2$Var2[1]))

day1_21 <- expand.grid(day1_data, day1_data, day1_data)

day1_21 <- day1_21 %>%
  mutate(fl_2020 = ifelse(Var1 + Var2 + Var3 == 2020, 1, 0)) %>%
  filter(fl_2020 == 1)

cat(paste0("The three entries that sum to 2020 are ",
           day1_21$Var1[1], day1_21$Var2[1], " and ", day1_21$Var3[1], ". Multiplying them together produces ",
           day1_21$Var1[1] * day1_21$Var2[1] * day1_21$Var3[1]))
