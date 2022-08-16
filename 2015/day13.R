# AoC 2015 Day 13
# Knights of the Dinner Table ----
# You're going to find the optimal seating arrangement and avoid all those awkward conversations.

library(dplyr)
library(stringr)

day13_data <- readLines("~/AoC/2015/data/day13_data")

day13_df <- data.frame(day13_data)

attend_names <- c("Alice|Bob|Carol|David|Eric|Frank|George|Mallory")

day13_df <- day13_df %>%
  mutate(person1 = sapply(day13_data, function(x) stringr::str_extract_all(x, attend_names)[[1]][1]),
         person2 = sapply(day13_data, function(x) stringr::str_extract_all(x, attend_names)[[1]][2]),
         happiness = stringr::str_extract(day13_data, "lose|gain"),
         amount = as.numeric(stringr::str_extract(day13_data, "[0-9\\-]+")),
         person12 = purrr::pmap_chr(list(person1,person2), ~paste(sort(c(...)), collapse = "_")),
         per1_per2 = paste0(person1, "_", person2))

day13_df <- day13_df %>%
  mutate(amount = ifelse(happiness == "lose", -amount, amount)) %>%
  arrange(person12)

# Part 1 ----
# What is the total change in happiness for the optimal seating arrangement of the actual guest list?

# Total Happiness
tothap <- rep(NA, 28)

for (j in seq(1,56,2)) {
  tothap[j] = day13_df$amount[[j]] + day13_df$amount[[j+1]]
}

# Remove NAs
tothap <- tothap[!is.na(tothap)]

# Repeate values twice
tothap <- rep(tothap, each=2)

# Get List of Person1 & Person2 combo lists
person_list <- dplyr::pull(day13_df,per1_per2)

# Get Names of everyone
person_names <- as.vector(names(table(day13_df$person1)))

perm_vec <- permutations(n = length(person_names), r = length(person_names), v = person_names)

perm_dist_vec <- matrix(NA, nrow = 40320, ncol = length(person_names))

# Create matrix with Person1 Person2 combined to make each row a round table
for (j in 1:8) {
  for (i in 1:nrow(perm_vec)) {
    if (j != 8) perm_dist_vec[i, j] = paste0(perm_vec[i, j], "_", perm_vec[i, j+1])
    else if (j == 8) perm_dist_vec[i, j] = paste0(perm_vec[i, j], "_", perm_vec[i, 1])
  }
}

# Fill in below vector with values
perm_dist_vec2 <- matrix(NA, nrow = 40320, ncol = length(person_names))

for (i in 1:56) {
  for (j in 1:8) {
    for (k in 1:nrow(perm_vec)) {
      if (person_list[i] == perm_dist_vec[k,j]) {
        perm_dist_vec2[k,j] = tothap[i]
      }
    }
  }
}

# Add Sum column to matrix 
perm_dist_vec <- cbind(perm_dist_vec, rowSums(perm_dist_vec2, na.rm = TRUE))

cat("What is the total change in happiness for the optimal seating arrangement of the actual guest list? ", 
    perm_dist_vec[which(perm_dist_vec[, 9] == max(as.numeric(perm_dist_vec[, 9]), arr.ind=TRUE)), ][1, ])

# Part 2 ----
# What is the total change in happiness for the optimal seating arrangement that actually includes yourself?

add_monarch <- data.frame(day13_data = rep("Bla Bla Bla",16),
                          person1 = c(person_names, rep("Monarch", 8)),
                          person2 = c(rep("Monarch", 8), person_names),
                          hapiness = as.character(rep("gain", 16)),
                          amount = rep(0, 16))

add_monarch <- add_monarch %>% 
  mutate(person1 = as.character(person1),
         person2 = as.character(person2),
         hapiness = as.character(hapiness),
         person12 = purrr::pmap_chr(list(person1,person2), ~paste(sort(c(...)), collapse = "_")),
         per1_per2 = paste0(person1, "_", person2)
         )
colnames(add_monarch) <- colnames(day13_df)

day13_df2 <- rbind(day13_df, add_monarch)

day13_df2 <- day13_df2 %>%
  arrange(person12)

# Total Happiness
tothap <- rep(NA, 36)

for (j in seq(1,72,2)) {
  tothap[j] = day13_df2$amount[[j]] + day13_df2$amount[[j+1]]
}

# Remove NAs
tothap <- tothap[!is.na(tothap)]

# Repeate values twice
tothap <- rep(tothap, each=2)

# Get List of Person1 & Person2 combo lists
person_list <- dplyr::pull(day13_df2,per1_per2)

# Get Names of everyone
person_names <- as.vector(names(table(day13_df2$person1)))

perm_vec <- permutations(n = length(person_names), r = length(person_names), v = person_names)

perm_dist_vec <- matrix(NA, nrow = 362880, ncol = length(person_names))

# Create matrix with Person1 Person2 combined to make each row a round table
for (j in 1:9) {
  for (i in 1:nrow(perm_vec)) {
    if (j != 9) perm_dist_vec[i, j] = paste0(perm_vec[i, j], "_", perm_vec[i, j+1])
    else if (j == 9) perm_dist_vec[i, j] = paste0(perm_vec[i, j], "_", perm_vec[i, 1])
  }
}

# Fill in below vector with values
perm_dist_vec2 <- matrix(NA, nrow = 362880, ncol = length(person_names))

for (i in 1:72) {
  for (j in 1:9) {
    for (k in 1:nrow(perm_vec)) {
      if (person_list[i] == perm_dist_vec[k,j]) {
        perm_dist_vec2[k,j] = tothap[i]
      }
    }
  }
}

# Add Sum column to matrix 
perm_dist_vec <- cbind(perm_dist_vec, rowSums(perm_dist_vec2, na.rm = TRUE))

cat("What is the total change in happiness for the optimal seating arrangement of the actual guest list and adding yourself? ", 
    perm_dist_vec[which(perm_dist_vec[, 10] == max(as.numeric(perm_dist_vec[, 10]), arr.ind=TRUE)), ][1, ])



# Better cleaner way?
# library(dplyr)
# library(stringr)
# 
# from <- str_extract(input, "\\w*")
# to <- str_extract(input, "\\w*\\.$")
# to <- str_remove(to, "\\.")
# happiness <- as.numeric(str_extract(input, "[0-9]+"))
# negative <- str_detect(input, "lose")
# 
# input_df <- data.frame(
#   from,
#   to,
#   happiness = happiness * ifelse(negative, -1, 1)
# )
# 
# all_perms <- combinat::permn(unique(from), m = length(unique(from)))
# 
# table_happiness <- function(x) {
#   seq_x <- seq_along(x)
#   len_x <- length(x)
#   
#   data.frame(
#     from = c(x, x),
#     to = c(x[c(seq_x[-1], 1)], x[c(len_x, seq_x[-len_x])])
#   ) %>%
#     left_join(input_df, by = c("from", "to")) %>%
#     summarize(sum = sum(happiness, na.rm = TRUE)) %>%
#     pull(sum)
#   print(x)
# }
# 
# perm_values <- vapply(all_perms, table_happiness, numeric(1))
# max(perm_values)
