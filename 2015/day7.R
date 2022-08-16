# Day 7 AoC 

library(dplyr)
library(stringr)
library(readr)
library(tidyr)

day7_data <- readLines("~/AoC/2015/data/day7_data")

day7_df <- data.frame(day7_data)

##Sort ur Data Frame
day7_df <- day7_df %>% separate(day7_data ,c("Input","Output"), sep = ' -> ')

day7_df <- day7_df %>% mutate(outLength = nchar(Output))
day7_df <- arrange(day7_df,outLength, Output)
day7_df[ , 'Input'] <- str_replace(day7_df$Input, "NOT", " NOT")
day7_df[ , 'Input'] <- str_replace(day7_df$Input, "lx", "lx  ")
day7_df[ , 'Input'] <- str_replace(day7_df$Input, "19138", "19138  ")
day7_df[ , 'Input'] <- str_replace(day7_df$Input, "0", "0  ")

day7_df <- day7_df %>% separate(Input ,c("left","Op", "right"), sep = ' ')

day7_df[2, 'OutputN'] <- as.numeric(day7_df[2, 'left'])
day7_df[3, 'OutputN'] <- as.numeric(day7_df[3, 'left'])

# Set up Alpha Vector 
alphal.vector <- as.vector(day7_df[['Output']])

for (i in 1:length(alphal.vector)) {
  
  if (day7_df[i,'Output'] == alphal.vector[i] & !is.na(day7_df[i,'OutputN'])) {
    
    # Go through each letters in the Left & Right Column, if numeric result is possible replace with numeric result
    day7_df$left  = str_replace(day7_df$left,  paste0("^", noquote(alphal.vector[i]), "$"), as.character(day7_df[i , 'OutputN']))
    day7_df$right = str_replace(day7_df$right, paste0("^", noquote(alphal.vector[i]), "$"), as.character(day7_df[i , 'OutputN']))
    
  }
  else if (day7_df[i,'Output'] == alphal.vector[i] & is.na(day7_df[i,'OutputN'])) {
    
    # Operation is missing then already result is available 
    if (is.na(day7_df[i, 'Op']) == TRUE) {
      day7_df[i, 'OutputN'] = day7_df[i, 'OutputN']
    }
    
    # Perform Operation
    
    else if (str_detect(day7_df[i , 'Op'], "RSHIFT") == TRUE) {
      day7_df[i,'OutputN'] = bitwShiftR(as.numeric(day7_df[i, 'left']),as.numeric(day7_df[i, 'right']))
    }
    
    else if (str_detect(day7_df[i, 'Op'], "LSHIFT") == TRUE) {
      day7_df[i,'OutputN'] = bitwShiftL(as.numeric(day7_df[i,'left']),as.numeric(day7_df[i,'right']))
    }
    
    else if (str_detect(day7_df[i, 'Op'], "AND") == TRUE) {
      day7_df[i,'OutputN'] = bitwAnd(as.numeric(day7_df[i,'left']),as.numeric(day7_df[i,'right']))
    }
    
    else if (str_detect(day7_df[i, 'Op'], "OR") == TRUE) {
      day7_df[i,'OutputN'] = bitwOr(as.numeric(day7_df[i,'left']),as.numeric(day7_df[i,'right']))
    }
    
    else if (str_detect(day7_df[i, 'Op'], "NOT") == TRUE) {
      day7_df[i,'OutputN'] = (2**16 - bitwNot(as.numeric(day7_df[i,'right'])))
    }
    
    day7_df$left  = str_replace(day7_df$left,  paste0("^", noquote(alphal.vector[i]), "$"), as.character(day7_df[i , 'OutputN']))
    day7_df$right = str_replace(day7_df$right, paste0("^", noquote(alphal.vector[i]), "$"), as.character(day7_df[i , 'OutputN']))
    
  }
  
}
cat("What signal is ultimately provided to wire a ", day7_df[1 , "left"])


# part 2 ----
# Override Signal a - 16076 to signal b

day7_data <- readLines("~/shahm21_play/AoC/2015/data/day7_data")

day7_df <- data.frame(day7_data)

##Sort ur Data Frame
day7_df <- day7_df %>% separate(day7_data ,c("Input","Output"), sep = ' -> ')

day7_df <- day7_df %>% mutate(outLength = nchar(Output))
day7_df <- arrange(day7_df,outLength, Output)
day7_df[ , 'Input'] <- str_replace(day7_df$Input, "NOT", " NOT")
day7_df[ , 'Input'] <- str_replace(day7_df$Input, "lx", "lx  ")
day7_df[ , 'Input'] <- str_replace(day7_df$Input, "19138", "19138  ")
day7_df[ , 'Input'] <- str_replace(day7_df$Input, "0", "0  ")

day7_df <- day7_df %>% separate(Input ,c("left","Op", "right"), sep = ' ')
day7_df[2, 'left'] <- "16076"

day7_df[2, 'OutputN'] <- as.numeric(day7_df[2, 'left'])
day7_df[3, 'OutputN'] <- as.numeric(day7_df[3, 'left'])

alphal.vector <- as.vector(day7_df[['Output']])

for (i in 1:length(alphal.vector)) {
  
  if (day7_df[i,'Output'] == alphal.vector[i] & !is.na(day7_df[i,'OutputN'])) {
    day7_df$left  = str_replace(day7_df$left,  paste0("^", noquote(alphal.vector[i]), "$"), as.character(day7_df[i , 'OutputN']))
    day7_df$right = str_replace(day7_df$right, paste0("^", noquote(alphal.vector[i]), "$"), as.character(day7_df[i , 'OutputN']))
    
  }
  else if (day7_df[i,'Output'] == alphal.vector[i] & is.na(day7_df[i,'OutputN'])) {
    
    if (is.na(day7_df[i, 'Op']) == TRUE) {
      day7_df[i, 'OutputN'] = day7_df[i, 'OutputN']
    }
    else if (str_detect(day7_df[i , 'Op'], "RSHIFT") == TRUE) {
      day7_df[i,'OutputN'] = bitwShiftR(as.numeric(day7_df[i, 'left']),as.numeric(day7_df[i, 'right']))
    }
    else if (str_detect(day7_df[i, 'Op'], "LSHIFT") == TRUE) {
      day7_df[i,'OutputN'] = bitwShiftL(as.numeric(day7_df[i,'left']),as.numeric(day7_df[i,'right']))
    }
    
    else if (str_detect(day7_df[i, 'Op'], "AND") == TRUE) {
      day7_df[i,'OutputN'] = bitwAnd(as.numeric(day7_df[i,'left']),as.numeric(day7_df[i,'right']))
    }
    else if (str_detect(day7_df[i, 'Op'], "OR") == TRUE) {
      day7_df[i,'OutputN'] = bitwOr(as.numeric(day7_df[i,'left']),as.numeric(day7_df[i,'right']))
    }
    else if (str_detect(day7_df[i, 'Op'], "NOT") == TRUE) {
      day7_df[i,'OutputN'] = (2**16 - bitwNot(as.numeric(day7_df[i,'right'])))
    }
    
    day7_df$left  = str_replace(day7_df$left,  paste0("^", noquote(alphal.vector[i]), "$"), as.character(day7_df[i , 'OutputN']))
    day7_df$right = str_replace(day7_df$right, paste0("^", noquote(alphal.vector[i]), "$"), as.character(day7_df[i , 'OutputN']))
    
  }
  
}
cat("What signal is ultimately provided to wire a ", day7_df[1 , "left"])
