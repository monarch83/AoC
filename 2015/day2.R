rm(list=ls())

# Day 2 
#
# Part 1 ----
# Find the surface area of each box (2*l*w + 2*w*h + 2*h*l), and
# little extra paper for each present: Area of the smallest side 
# For example:
#  A present with dimensions 2x3x4 requires 2*6 + 2*12 + 2*8 = 52 square feet of wrapping paper 
#  plus 6 square feet of slack, for a total of 58 square feet.
#  A present with dimensions 1x1x10 requires 2*1 + 2*10 + 2*10 = 42 square feet of wrapping paper 
#  plus 1 square foot of slack, for a total of 43 square feet.
#
# How many total square feet of wrapping paper should they order?

library(stringr)
library(tidyr)

# Read Puzzle Input
day2_data <- read.table(file = "~/AoC/2015/data/day2_data")

colnames(day2_data) <- "Box_Size"

# head(day2_data)

# Split string into Height, Length, Width 
day2_data <- stringr::str_split(day2_data$Box_Size, "x")

# Convert into numeric 
day2_data <- lapply(day2_data, type.convert, as.is = TRUE)

# Test Data
# day2_data <- list(c(2,3,4), c(1,1,10))

suf_area <- rep(NA, length(day2_data))
sml_area <- rep(NA, length(day2_data))
tot_area <- rep(NA, length(day2_data))

for (i in 1:length(day2_data)) {
  
  suf_area[i] <- 
    (2*day2_data[[i]][1] * day2_data[[i]][2]) +
    (2*day2_data[[i]][2] * day2_data[[i]][3]) +
    (2*day2_data[[i]][1] * day2_data[[i]][3])
  
  sml_area[i] <- 
    min(
        (day2_data[[i]][1] * day2_data[[i]][2]),
        (day2_data[[i]][2] * day2_data[[i]][3]),
        (day2_data[[i]][1] * day2_data[[i]][3])
        )
  
  tot_area[i] <-
    suf_area[i] + sml_area[i]
  
}

cat("How many total square feet of wrapping paper should they order? ", sum(tot_area))                        


# PART 2 ----
# The elves are also running low on ribbon. Ribbon is all the same width, so they only have to worry about 
# the length they need to order, which they would again like to be exact.

# The ribbon required to wrap a present is the shortest distance around its sides, or the smallest perimeter 
# of any one face. Each present also requires a bow made out of ribbon as well; the feet of ribbon required 
# for the perfect bow is equal to the cubic feet of volume of the present. Don't ask how they tie the bow, 
# though; they'll never tell.

# For example
# 
# A present with dimensions 2x3x4 requires 2+2+3+3 = 10 feet of ribbon to wrap the present plus 2*3*4 = 24 feet 
# of ribbon for the bow, for a total of 34 feet.
# A present with dimensions 1x1x10 requires 1+1+1+1 = 4 feet of ribbon to wrap the present plus 1*1*10 = 10 
# feet of ribbon for the bow, for a total of 14 feet.
# 
# How many total feet of ribbon should they order?

# Compute Smallest Perimeter, Cubic Feet & Total Feet of Ribbon needed. 

sml_perm <- rep(NA, length(day2_data))
vol_cubfet <- rep(NA, length(day2_data))
tot_feet <- rep(NA, length(day2_data))

for (i in 1:length(day2_data)) {
  
  sml_perm[i] <- 
    (sort(day2_data[[i]])[1] + sort(day2_data[[i]])[2])*2
  
  vol_cubfet[i] <- 
    day2_data[[i]][1] * day2_data[[i]][2] * day2_data[[i]][3] 
  
  tot_feet[i] <-
    sml_perm[i] + vol_cubfet[i]
  
}

cat("How many total feet of ribbon should they order? ", sum(tot_feet))                        
