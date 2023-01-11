# ----
# Day 7: No Space Left On Device ---

library(stringr)
library(dplyr)

# Read Puzzle Input
input <- readLines("~/AoC/2022/data/day7_data")

# Test Data
# input <- c("$ cd /", "$ ls", "dir a" ,"14848514 b.txt" ,"8504156 c.dat" ,"dir d"
#               ,"$ cd a" ,"$ ls" ,"dir e" ,"29116 f" ,"2557 g" ,"62596 h.lst" ,"$ cd e"
#               ,"$ ls" ,"584 i" ,"$ cd .." ,"$ cd .." ,"$ cd d" ,"$ ls" ,"4060174 j"
#               ,"8033020 d.log" ,"5626152 d.ext" ,"7214296 k")

done <- ongoing <- numeric()

for (line in input) {
  if (line == "$ ls") {
    ongoing <- c(ongoing, 0)
    cat(line, ": ", ongoing, "\n")
  }
  else if (grepl("^\\d", line)) {
    ongoing <- ongoing + as.numeric(gsub("\\D", "", line))
    cat(line, ": ", ongoing, "\n")
  }
  else if (line == "$ cd ..") {
    done <- c(done, tail(ongoing, 1))
    ongoing <- head(ongoing, -1)
    cat(line, ": ", ongoing, "\n")
  }
}

done <- c(ongoing, done)
part1 <- sum(done[done < 100000])

# Part 2
free <- 70000000 - done[1]
needed <- 30000000 - free
part2 <- min(done[done > needed])


