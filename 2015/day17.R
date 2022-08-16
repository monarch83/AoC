# Day 17 AoC 2015
# No Such Thing as Too Much ----

library(dplyr)
library(stringr)
library(gtools)

day17_data <- sort(as.numeric(readLines("~/AoC/2015/data/day17_data")))

# Change Repeated Numers to something else
day17_data[2] <- 101 #1
day17_data[4] <- 201 #3
day17_data[5] <- 202 #3
day17_data[8] <- 301 #11
day17_data[16]<- 401 #32
day17_data[18]<- 501 #36


combination_list <- list()

# Create a combinations of containters from 2 to the length of the day17 Data
for (i in 2:length(day17_data)) {
  combination_list[[i]] <- as.data.frame(gtools::combinations(length(day17_data), i, v=day17_data, set=TRUE, repeats.allowed=FALSE))
} 
  # Change values back
combination_df <- bind_rows(combination_list[[1]], combination_list[[2]],combination_list[[3]], combination_list[[4]],
                            combination_list[[5]],combination_list[[6]], combination_list[[7]],
                            combination_list[[8]],combination_list[[9]], combination_list[[10]],
                            combination_list[[11]],combination_list[[12]], combination_list[[13]],
                            combination_list[[14]],combination_list[[15]], combination_list[[16]],
                            combination_list[[17]],combination_list[[18]], combination_list[[19]], combination_list[[20]])

reset.var <- function(var) {
  
  combination_df[[var]] <- 
    ifelse(combination_df[[var]] == 101, 1, 
           ifelse(combination_df[[var]] == 201, 3,
                  ifelse(combination_df[[var]] == 202, 3,
                         ifelse(combination_df[[var]] == 301, 11,
                                ifelse(combination_df[[var]] == 401, 32,
                                       ifelse(combination_df[[var]] == 501, 36, combination_df[[var]]))))))
  
  return(combination_df)
  
}

combination_df <- reset.var("V1")
combination_df <- reset.var("V2")
combination_df <- reset.var("V3")
combination_df <- reset.var("V4")
combination_df <- reset.var("V5")
combination_df <- reset.var("V6")
combination_df <- reset.var("V7")
combination_df <- reset.var("V8")
combination_df <- reset.var("V9")
combination_df <- reset.var("V10")
combination_df <- reset.var("V11")
combination_df <- reset.var("V12")
combination_df <- reset.var("V13")
combination_df <- reset.var("V14")
combination_df <- reset.var("V15")
combination_df <- reset.var("V16")
combination_df <- reset.var("V17")
combination_df <- reset.var("V18")
combination_df <- reset.var("V19")
combination_df <- reset.var("V20")


combination_df <- combination_df %>% 
  mutate(sum= rowSums(combination_df[ , c(1:20)], na.rm = T), flag=ifelse(sum == 150, 1,0)) %>%
  filter(sum == 150) %>%
  select(-13:-20)

# Part 1 ----
# Filling all containers entirely, how many different combinations of containers can exactly fit all 150 liters of eggnog?

cat("Filling all containers entierely, How maby different combinations of containers can exactly fit all 150 liters of eggnog? ", 
    table(combination_df$flag))

# Part 2
# Find the minimum number of containers that can exactly fit all 150 liters of eggnog. How many different ways can you 
# fill that number of containers and still hold exactly 150 litres?

combination_df2 <- combination_df %>% mutate(cont.num = rowSums(!is.na(combination_df[, c(1:12)])))  

cat("Find the minimum # of containers that can exactly fit all 150 liters of eggnog: ", min(combination_df2$cont.num))

# Simpler way

# Use Seq Along to get position of each of the 20 containers, as some containers have same size limit
# use expand.grid to create all combinations of the supplied vectors

day17_data <- sort(as.numeric(readLines("~/shahm21_play/AoC/2015/data/day17_data")))

day17_df <- expand.grid(lapply(seq_along(day17_data), function(x) c(F,T)))

# Function to change each values back to the original data
reset_val_fun <- function(data) {
  
  for (i in 1:ncol(data)) {
    data[[paste0("Var",i)]] <- data[[paste0("Var",i)]] * day17_data[i]
  }
  
  return(data)
}

day17_df <- reset_val_fun(data = day17_df)

day17_df <- day17_df %>%
  mutate(row_sum = rowSums(day17_df))

# Part 1 ----

cat("Filling all containers entierely, How maby different combinations of containers can exactly fit all 150 liters of eggnog? ", 
    sum(day17_df$row_sum == 150))

# Part 2 ----

# Find out Total # of containers with 0s and substract from total possible containers to get the min # of container needed 
# to fill up 150 eggnogs
day17_df2 <- day17_df %>%
  mutate(row_sum_zero = ncol(day17_df[1:20]) - rowSums(day17_df[1:20] == 0)) %>%
  filter(row_sum == 150) 

cat("Find the minimum # of containers that can exactly fit all 150 liters of eggnog: ", min(day17_df2$row_sum_zero))
