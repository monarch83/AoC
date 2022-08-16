# AoC 2015 Day 12
# JSAbacusFramework.io ----

library(dplyr)
library(stringr)
library(gtools)

# Read Day 9 Data
day12_data <- as.vector(read.csv(text = readLines("~/AoC/2015/data/day12_data"), sep = "\n", header = FALSE))
class(day12_data)

# Part 1 ----
# What is the sum of all numbers in the document?

input.1 <- as.numeric(unlist(str_extract_all(day12_data$V1,"[0-9\\-]+")))

cat(paste0("Sum of All Numbers in the document: ", sum(input.1)))

# Part 2 ----
# Uh oh - the Accounting-Elves have realized that they double-counted everything red.
# Ignore any object (and all of its children) which has any property with the value "red". 
#  Do this only for objects ({...}), not arrays ([...]).
# Inspiration from ---> https://emilhvitfeldt.github.io/rstats-adventofcode/2015.html?panelset=day-12

example1 <- "[1,2,3]"
example2 <- bquote('[1,{"c":"red","b":2},3]')
example3 <- bquote('{"d":"red","e":[1,2,3,4],"f":5}')
example4 <- bquote('[1,"red",5]')

input <- readLines("~/shahm21_play/AoC/2015/data/day12_data")

flag_red <- function(x) {
  
  if (length(x) > 1) return(FALSE)
  
  if (class(x) == "list") return(FALSE)
  
  x == "red"
  
}


remove_red_sum <- function(inpstr) {
  
  # Allows recursive check?
  repeat {
    
    # Find the location of End of } 
    end <- stringr::str_locate(inpstr, "\\}")[1, 1]
    
    if (all(is.na(end))) break
    
    starts <- stringr::str_locate_all(inpstr, "\\{")[[1]][, 1]
    start <- max(starts[starts < end])
    
    # Extract string btw { and }
    extraction <- stringr::str_sub(inpstr, start, end)
    
    # Convert to JSON
    json <- jsonlite::parse_json(extraction)
    
    # If no red, then sum, if red set sum to 0
    if (!any(sapply(json, flag_red))) {
      res <- stringr::str_extract_all(extraction, "[0-9\\-]+")[[1]]
      res <- sum(as.numeric(res), na.rm = TRUE)
    } else {
      # if red sum is 0
      res <- 0
    }
    
    # replace string btw { and } with the result 
    str_sub(inpstr, start, end) <- res
  }
  
  # Sum everything after sorting out red
  res <- str_extract_all(inpstr, "[0-9\\-]+")[[1]]
  res <- sum(as.numeric(res), na.rm = TRUE)
  res
  
}

remove_red_sum(example1)
remove_red_sum(example2)
remove_red_sum(example3)
remove_red_sum(example4)

cat(paste0("Sum of All Numbers in the document after removing double counted red: ", remove_red_sum(input)))


# Use of Repeat function
# Repeat loop untill sum becomes 10
# sum <- 0
# repeat{
#   sum <- sum + 1
#   print(sum)
#   
#   if (sum == 10){
#     break
#   }
# }
