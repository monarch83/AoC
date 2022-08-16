# Day 3: Perfectly Spherical Houses in a Vacuum ---- 

rm(list=ls())

library(stringr)
library(dplyr)

# north (^), south (v), east (>), or west (<).
# Part 1 - How many houses receive at least one present? ----

day3_data <- read.table(file = "~/AoC/2015/data/day3_data")

day3_data_vec <- unlist(strsplit(as.character(day3_data$V1), split=character(0)))

# Test Cases
# day3_data.vec <- c(">")
# day3_data.vec <- c("^", ">", "v", "<")
# day3_data.vec <- c("^", "v", "^", "v", "^", "v", "^", "v", "^", "v")

pos.x <- c(0)
pos.y <- c(0)

cordinates_x <- rep(NA, length(day3_data_vec))
cordinates_y <- rep(NA, length(day3_data_vec))

for (i in 1:length(day3_data_vec)) {
  
  if(day3_data_vec[i]=="^") {
    pos.y = pos.y + 1
  }
  else if(day3_data_vec[i]=="v") {
    pos.y = pos.y - 1
  }
  else if(day3_data_vec[i]==">") {
    pos.x = pos.x + 1
  }
  else if (day3_data_vec[i]=="<") {
    pos.x = pos.x - 1
  }
  cordinates_x[i] <- pos.x
  cordinates_y[i] <- pos.y
}

day3_df <- data.frame(day3_data_vec, cordinates_x, cordinates_y)
base_df <- data.frame(day3_data_vec = "Start", cordinates_x = 0, cordinates_y = 0)

day3_df <- rbind(base_df, day3_df)

day3_df <- day3_df %>% dplyr::mutate(ncord = paste0("(", cordinates_x, " ,", cordinates_y, ")"))

cat(paste0("How many houses receive at least one present? ", day3_df %>% summarise(n_distinct(ncord))))


# part 2 ----
# Santa and Robo-Santa start at the same location (delivering two presents to the same starting house), 
# then take turns moving based on instructions from the elf, who is eggnoggedly reading from the same script as the previous year.
# day3_data_vec <- c("^", "v")
# day3_data_vec <- c("^", ">", "v", "<")
# day3_data_vec <- c("^", "v", "^", "v", "^", "v", "^", "v", "^", "v")

pos.sx <- c(0)
pos.sy <- c(0)

pos.rsx <- c(0)
pos.rsy <- c(0)

cordinates_sx <- rep(NA, length(day3_data_vec))
cordinates_sy <- rep(NA, length(day3_data_vec))
cordinates_rsx <- rep(NA, length(day3_data_vec))
cordinates_rsy <- rep(NA, length(day3_data_vec))

for (i in 1:length(day3_data_vec)) {

  # Santa goes First
  if (seq_along(day3_data_vec)[i] %% 2 != 0) {
    if(day3_data_vec[i]=="^") {
      pos.sy = pos.sy + 1
    }
    else if(day3_data_vec[i]=="v") {
      pos.sy = pos.sy - 1
    }
    else if(day3_data_vec[i]==">") {
      pos.sx = pos.sx + 1
    }
    else if (day3_data_vec[i]=="<") {
      pos.sx = pos.sx - 1
    }
  }
  
  # Robo Santa goes next
  else if (seq_along(day3_data_vec)[i] %% 2 == 0) {
    if(day3_data_vec[i]=="^") {
      pos.rsy = pos.rsy + 1
    }
    else if(day3_data_vec[i]=="v") {
      pos.rsy = pos.rsy - 1
    }
    else if(day3_data_vec[i]==">") {
      pos.rsx = pos.rsx + 1
    }
    else if (day3_data_vec[i]=="<") {
      pos.rsx = pos.rsx - 1    
    }
  } 

  cordinates_sx[i] <- pos.sx
  cordinates_sy[i] <- pos.sy
  cordinates_rsx[i] <- pos.rsx
  cordinates_rsy[i] <- pos.rsy
  
}

day3_df_b <- data.frame(day3_data_vec, cordinates_sx, cordinates_sy, cordinates_rsx, cordinates_rsy)
base_df_b <- data.frame(day3_data_vec = "Start", cordinates_sx = 0, cordinates_sy = 0, cordinates_rsx = 0, cordinates_rsy = 0)

day3_df_b <- rbind(base_df_b, day3_df_b)

day3_df_b <- day3_df_b %>% dplyr::mutate(snt_cord = paste0("(", cordinates_sx, " ,", cordinates_sy, ")"),
                                     rsnt_cord = paste0("(", cordinates_rsx, " ,", cordinates_rsy, ")"))

cat(paste0("How many houses receive at least one present? ", length(table(cbind(day3_df_b$snt_cord, day3_df_b$rsnt_cord)))))
