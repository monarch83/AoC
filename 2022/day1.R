# ----
# Day 1: Calorie Counting ---

# Part 1 ----
# Find the Elf carrying the most Calories. How many total Calories is that Elf carrying?

library(stringr)

# Read Puzzle Input
day1_data <- read.table(file = "~/AoC/2022/data/day1_data", blank.lines.skip=F)

# Test case
# day1_data <- data.frame(c(NA, 1000, 2000, 3000, NA, 4000, NA, 5000, 6000, NA, 7000, 8000, 9000, NA, 10000))
# colnames(day1_data) <- "V1"

# Add blank line as first line
day1_data <- rbind(NA, day1_data)

# Each Blank row starts new Elf, Start from 1st and iterate over to find the elf with most total calories

day1_dlist <- split(day1_data$V1, cumsum(is.na(day1_data$V1)))

day1_totcal <- sapply(day1_dlist, sum, na.rm=TRUE) 

cat("Find the Elf carrying the most Calories: ", as.numeric(names(which(day1_totcal == max(day1_totcal)))), "\n",
    "How many total Calories is that Elf Carring: ", as.numeric(max(day1_totcal)))

# Part 2 ----
# Find the top three Elves carrying the most Calories. How many Calories are those Elves carrying in total?

# Use the day1_totcal vector, sort it and find the 1st 3
day1_totcal_sort <- sort(day1_totcal, decreasing = TRUE)

cat("Find the top three Elves carrying the most Calories: ", names(day1_totcal_sort[1:3]), "\n",
    "How many total Calories top 3 elfes are Carring: ", as.numeric(sum(day1_totcal_sort[1:3])))
