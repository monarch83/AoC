# Day 5 AoC

# A nice string is one with all of the following properties:

# It contains at least three vowels (aeiou only), like aei, xazegov, or aeiouaeiouaeiou.
# It contains at least one letter that appears twice in a row, like xx, abcdde (dd), 
#  or aabbccdd (aa, bb, cc, or dd).
# It does not contain the strings ab, cd, pq, or xy, even if they are part of one of the other requirements.

# For example:

# ugknbfddgicrmopn is nice because it has at least three vowels (u...i...o...), 
# a double letter (...dd...), and none of the disallowed substrings.
# aaa is nice because it has at least three vowels and a double letter, even though the 
# letters used by different rules overlap.
# jchzalrnumimnmhp is naughty because it has no double letter.
# haegwjzuvuyypxyu is naughty because it contains the string xy.
# dvszwmarrgswjxmb is naughty because it contains only one vowel.

# Part 1 ----
# How many strings are nice?

# Test Data
# day5_data <- data.frame(rbind("ugknbfddgicrmopn", "aaa", "jchzalrnumimnmhp", "haegwjzuvuyypxyu", "dvszwmarrgswjxmb"))

library(dplyr)
library(stringr)

day5_data <- read.table(file = "~/AoC/2015/data/day5_data")
colnames(day5_data) <- "string"

# Rule 1
day5_data$rule1 <- ifelse(stringr::str_count(day5_data$string, "[aeiou]") >= 3, TRUE, FALSE)

# Rule 2
day5_data$rule2 <- ifelse(stringr::str_detect(day5_data$string, "([a-z])\\1+"), TRUE, FALSE)

# Rule 3
day5_data$rule3 <- ifelse(!stringr::str_detect(day5_data$string, "ab|cd|pq|xy"), TRUE, FALSE)

# Naught or Nice String
day5_data$nn_str <- ifelse(rowSums(day5_data[,c("rule1", "rule2", "rule3")]) == 3, "Nice", "Naughty")

cat("How many strings are nice? ", table(day5_data[day5_data$nn_str == "Nice", ]$nn_str))

# Part 2 ----
# Now, a nice string is one with all of the following properties:

# It contains a pair of any two letters that appears at least twice in the string without overlapping, 
# like xyxy (xy) or aabcdefgaa (aa), but not like aaa (aa, but it overlaps).
# It contains at least one letter which repeats with exactly one letter between them, like xyx, abcdefeghi (efe), or even aaa.

# For example:
# qjhvhtzxzqqjkmpb is nice because is has a pair that appears twice (qj) and a letter that repeats 
# with exactly one letter between them (zxz).
# xxyxx is nice because it has a pair that appears twice and a letter that repeats with one between, 
# even though the letters used by each rule overlap.
# uurcxstgmygtbstg is naughty because it has a pair (tg) but no repeat with a single letter between them.
# ieodomkazucvgmuy is naughty because it has a repeating letter with one between (odo), but no pair that appears twice.

# How many strings are nice under these new rules?

# Test Data
# day5_data <- data.frame(rbind("qjhvhtzxzqqjkmpb", "xxyxx", "uurcxstgmygtbstg", "ieodomkazucvgmuy"))

day5_data <- read.table(file = "~/AoC/2015/data/day5_data")
colnames(day5_data) <- "string"

# Rule 1
day5_data$rule1 <- ifelse(stringr::str_detect(day5_data$string, regex("([a-z]{2}).*?\\1")), TRUE, FALSE)

# Rule 2
day5_data$rule2 <- ifelse(stringr::str_detect(day5_data$string, regex("([a-z])[a-z]\\1.?")), TRUE, FALSE)

# Naught or Nice String
day5_data$nn_str <- ifelse(rowSums(day5_data[,c("rule1", "rule2")]) == 2, "Nice", "Naughty")

cat("How many strings are nice? ", table(day5_data[day5_data$nn_str == "Nice", ]$nn_str))
