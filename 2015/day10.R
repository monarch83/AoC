# Day 10 AoC Elves Look, Elves Say ----
#
# Look-and-say sequences are generated iteratively, using the previous value as input for the next step. 
# For each step, take the previous value, and replace each run of digits (like 111) with the number of digits (3) 
# followed by the digit itself (1).

# Puzzle Input 

input <- 3113322113

library(stringr)

look_and_say <- function(x, return.an.int=FALSE){
  
  # convert number to character vector
  xstr <- unlist(strsplit(as.character(x), ""))
  
  # get run length encoding   
  rlex <- rle(xstr)
  
  # New String for Lengths 
  odds <- as.character(rlex$lengths)
  
  # New String for Values 
  evens <- rlex$values
  
  # Combine Length & Values to form next look and say string 
  newstr <- as.vector(rbind(odds, evens))
  
  # collapse to scalar
  newstr <- paste(newstr, collapse="")
  #convert to number, if desired
  if(return.an.int) as.integer(newstr) else newstr
}

# Part 1 ----
# Starting with the digits in your puzzle input, apply this process 40 times. What is the length of the result?
x <- input
for(i in 1:40) {
  x <- look_and_say(x)
  #print(x)
}

cat("What is the lenght of the result, apply look and say 40 times: ", nchar(x))

# Part 1 ----
# Starting with the digits in your puzzle input, apply this process 50 times. What is the length of the result?
x <- input

for(i in 1:50) {
  x <- look_and_say(x)
  #print(x)
}

cat("What is the lenght of the result, apply look and say 50 times: ", nchar(x))


# Old slow way ----

i1 <- "3113322113"
final.vec.list <- list()
new.vec = vector()

for (j in 1:50) {
  
  input.list <- vector()
  input.list <-  as.numeric(unlist(strsplit(i1, split=character(0))))
  
  diff.vec.list <- vector()
  diff.vec.list <- c(diff(input.list), 999)
  
  count = 1
  final.vec = vector()
  
  for (i in 1:length(diff.vec.list)) {
    if (diff.vec.list[i] == 0) {
      if (i == length(diff.vec.list)) {
        count = count + 1
        new.vec = c(as.numeric(count), as.numeric(input.list[i]))
      }
      else if (i != length(diff.vec.list)) {
        count = count + 1
      }
    }
    else if (diff.vec.list[i] != 0) {
      new.vec = c(as.numeric(count), as.numeric(input.list[i]))
      count = 1
    }
    
    final.vec = append(final.vec, new.vec)
    new.vec = vector()
    #print(final.vec)
  }
  
  final.vec.list <- append(final.vec.list, list(final.vec))
  i1 = paste(final.vec, collapse="")
  #print(i1)
  
}

look_and_say_5 <- nchar(noquote(paste(final.vec.list[[5]], collapse="")))
print(look_and_say_5)

# look_and_say_40 <- nchar(noquote(paste(final.vec.list[[40]], collapse="")))
# print(look_and_say_40)
# 
# look_and_say_50 <- nchar(noquote(paste(final.vec.list[[50]], collapse="")))
# print(look_and_say_50)
