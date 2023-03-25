# ----
# Day 1: Calorie Counting ---

# Part 1 ----
# Find the Elf carrying the most Calories. How many total Calories is that Elf carrying?

library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(gganimate)

# Read Puzzle Input
day1_data <- read.table(file = "AoC/2022/data/day1_data", blank.lines.skip=F)

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

# Viz

df <- data.frame()
for (i in 1:length(day1_dlist)) {
  
  df <- bind_rows(df, data.frame(t(sapply(day1_dlist[[i]],c))))
  
}

df <- df %>%
  select(-X1) %>%
  mutate(elf = row_number()) 
  
df$sum <- rowSums(df[, 1:15], na.rm = TRUE)

df <- df %>% 
  unite(., col = "New.Col", X2:X16, na.rm=TRUE, sep = ",")

df <- df %>%
  mutate(colcat = 1,
         sumcat = 1.4)

df <- df %>%
  mutate(maxcal = ifelse(sum == max(df$sum), max(df$sum), NA))

g <- ggplot(df, aes(colcat, elf, label = New.Col)) +
  geom_text(size = 1, hjust = 0) +
  scale_y_continuous(breaks = seq(1, 256, by=1), limits = c(1,256)) +
  xlim(1, 1.5) 
  
g <- g +
  geom_text(aes(sumcat, elf, label = sum), size = 1, hjust = 0, color = "blue") +
  geom_text(aes(sumcat, elf, label = maxcal), size = 1, hjust = 0, color = "red") +
  theme(axis.text.y = element_text(size=3),
        axis.text.x = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()
        ) 

g

pdf(file="AoC/2022/gifs_viz/day1.pdf",
    width = 8,
    height=22,
    bg = "white",
    paper = "legal")
plot(g)
dev.off()

g <- g +
  transition_reveal(elf) 
