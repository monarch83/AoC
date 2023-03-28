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

# Make dataset LONG
df_long <- df %>%
  pivot_longer(cols = c(X2:X16, "sum"),
               names_prefix = "Elf",
               values_to = "calories",
               values_drop_na = TRUE)

df_long <- df_long %>%
  mutate(x_cat = case_when(
    elf %in% c(1:16) ~ 1,
    elf %in% c(17:32) ~ 2,
    elf %in% c(33:48) ~ 3,
    elf %in% c(49:64) ~ 4,
    elf %in% c(65:80) ~ 5,
    elf %in% c(81:96) ~ 6,
    elf %in% c(97:112) ~ 7,
    elf %in% c(113:128) ~ 8,
    elf %in% c(129:144) ~ 9,
    elf %in% c(145:160) ~ 10,
    elf %in% c(161:176) ~ 11,
    elf %in% c(177:192) ~ 12,
    elf %in% c(193:208) ~ 13,
    elf %in% c(209:224) ~ 14,
    elf %in% c(225:240) ~ 15,
    elf %in% c(241:256) ~ 16))

dummy <- data.frame(elf = c(1:256), y_cat = rep(1:16, 16))

df_long <- df_long %>%
  left_join(dummy, by = c("elf"))

df_long <- df_long %>%
  mutate(sumcal = ifelse(name == "sum", calories, NA),
         category = as.numeric(str_extract(name, "[0-9]+")))

df_long <- df_long %>%
  mutate(category = ifelse(is.na(category), 16, category),
         color_grp = as.factor(ifelse(category != 16, 1, 2)))

df_long <- df_long %>%
  group_by(elf) %>%
  mutate(id = (y_cat + row_number()/10))


g <- ggplot(df_long, aes(x_cat, id, color = color_grp, label = as.character(calories))) +
  geom_text(size = 2, hjust = 0) +
  scale_y_reverse(breaks = seq(1, 16, by=1), limits = c(16,1)) +
  scale_x_continuous(breaks = seq(1, 16, by=1), limits = c(1,16)) +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none"
        )  +
  scale_color_manual(values = c("black", "red")) 

g

# pdf(file="AoC/2022/gifs_viz/day1.pdf",
#     width = 8,
#     height=22,
#     bg = "white",
#     paper = "legal")
# plot(g)
# dev.off()

g <- g +
  transition_time(id) +
  shadow_mark()

animate(g, end_pause = 15, width=12, height=30, units="in", res = 300)

anim_save("AoC/2022/gifs_viz/day1.gif", animation = last_animation())

