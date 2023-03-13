# ----
# Day 5: Hydrothermal Venture ---

# Part 1 ----
# At how many points do at least two lines overlap?

library(stringr)
library(dplyr)

# Read Puzzle Input
day5_data <- read.table(file = "~/AoC/2021/data/day5_data", blank.lines.skip=F, colClasses=c("character"))
day5_data <- day5_data %>%
  mutate(x1 = as.numeric(str_extract_all(day5_data$V1, "[0-9]+", simplify = TRUE)[,1]),
         y1 = as.numeric(str_extract_all(day5_data$V1, "[0-9]+", simplify = TRUE)[,2]),
         x2 = as.numeric(str_extract_all(day5_data$V3, "[0-9]+", simplify = TRUE)[,1]),
         y2 = as.numeric(str_extract_all(day5_data$V3, "[0-9]+", simplify = TRUE)[,2]),
         only_horz_lines = ifelse(x1 == x2 | y1 == y2, "Y", "N"))

day5_data1 <- day5_data %>%
  filter(only_horz_lines == "Y")

matrix_diagram <- matrix(0, nrow = 1000, ncol = 1000)
rownames(matrix_diagram) <- c(0:999)
colnames(matrix_diagram) <- c(0:999)

# x1, x2 = Horizontal, y1, y2 = vertical
for (i in 1:nrow(day5_data1)) {
  
  if (day5_data1$x1[i] == day5_data1$x2[i]) {
    
    # Go Vertical
    cover_line <- day5_data1$y1[i] - day5_data1$y2[i]
    
    # Negative Go down
    if (cover_line < 0) {
      matrix_diagram[(day5_data1$y1[i]+1):(day5_data1$y1[i] + abs(cover_line)+1), day5_data1$x1[i] + 1] <- matrix_diagram[(day5_data1$y1[i]+1):(day5_data1$y1[i] + abs(cover_line)+1), day5_data1$x1[i] + 1] + 1
    }
    
    # Positive Go Up
    else if (cover_line > 0){
      matrix_diagram[(day5_data1$y2[i]+1):(day5_data1$y2[i] + abs(cover_line)+1), day5_data1$x1[i] + 1] <- matrix_diagram[(day5_data1$y2[i]+1):(day5_data1$y2[i] + abs(cover_line)+1), day5_data1$x1[i] + 1] + 1
    }
    
  }
  else if (day5_data1$y1[i] == day5_data1$y2[i]) {
    
    # Go horizontal
    cover_line <- day5_data1$x1[i] - day5_data1$x2[i]
    
    # Negative Go Right
    if (cover_line < 0) {
      matrix_diagram[day5_data1$y1[i] + 1, (day5_data1$x1[i]+1):(day5_data1$x1[i] + abs(cover_line)+1)] <- matrix_diagram[day5_data1$y1[i] + 1, (day5_data1$x1[i]+1):(day5_data1$x1[i] + abs(cover_line)+1)] + 1
    }
    
    # Positive Go Left
    else if (cover_line > 0){
      matrix_diagram[day5_data1$y1[i]+1, (day5_data1$x2[i]+1):(day5_data1$x2[i] + abs(cover_line)+1)] <- matrix_diagram[day5_data1$y1[i]+1, (day5_data1$x2[i]+1):(day5_data1$x2[i] + abs(cover_line)+1)] + 1
    }
    
  }
  print(nrow(which(matrix_diagram >= 2, arr.ind = TRUE)))
}

cat(paste0("At how many points do at least two lines overlap? ", nrow(which(matrix_diagram >= 2, arr.ind = TRUE))))


# Part 2 ----
tibble(input = readLines("~/AoC/2021/data/day5_data")) %>%
  separate(input, into = c("x1", "y1", "x2", "y2"), convert = TRUE) %>%
  group_nest(row_number()) %>%
  mutate(crosses = map(data, ~tibble(x = .x$x1:.x$x2, y = .x$y1:.x$y2))) %>%
  unnest(crosses) %>%
  count(x, y) %>%
  filter(n > 1) %>%
  nrow()
