# Day 12: Hill Climbing Algorithm ---

library(stringr)
library(dplyr)

# Read Puzzle Input
day12_data <- readLines("~/AoC/2022/data/day12_data")

# Test Case
day12_data <- c("Sabqponm",
                "abcryxxl",
                "accszExk",
                "acctuvwj",
                "abdefghi")

day12_mat <- matrix(NA, length(day12_data), nchar(day12_data[1]))

for (i in 1:length(day12_data)) {
  
  day12_mat[i, ] <- unlist(strsplit(day12_data[i],""))
  
}

road1 <- c("S", "a", "a")
road2 <- c("S", "a", )

library(igraph)
g4 <- graph( c("John", "Jim", "Jim", "Jack", "Jim", "Jack", "John", "John"), 
             
             isolates=c("Jesse", "Janis", "Jennifer", "Justin") )  

# In named graphs we can specify isolates by providing a list of their names.






map <- do.call(rbind,strsplit(readLines("~/AoC/2022/data/day12_data"),""))
start <- which(map=="S",arr.ind=T)
end <- which(map=="E",arr.ind=T)
map[map=="S"]<-"a"
map[map=="E"]<-"z"
all_coords <- expand.grid(1:nrow(map),1:ncol(map))
nb <- function(x,y){
  coords <- rbind(c(x-1,y),
                  c(x+1,y),
                  c(x,y-1),
                  c(x,y+1))
  coords <- coords[!coords[,1]%in%c(0,nrow(map)+1)&!coords[,2]%in%c(0,ncol(map)+1),]
  w <- which(letters==map[x,y])
  l <- apply(coords,1,function(z)which(letters==map[z[1],z[2]]))
  coords <- coords[l-w<=1,,drop=FALSE]
  if(nrow(coords)){
    ids <- apply(coords,1,function(z)which(all_coords[,1]==z[1]&all_coords[,2]==z[2]))
    id <- which(all_coords[,1]==x&all_coords[,2]==y)
    return(data.frame("from"=id,"to"=ids))
  }else{return(NULL)}
}

library(igraph)
edg <- do.call(rbind,apply(all_coords,1,function(x)nb(x[1],x[2])))
g <- graph_from_edgelist(as.matrix(edg)) #Make a directed graph with list of possible paths
distances(g,
          which(all_coords[,1]==start[1]&all_coords[,2]==start[2]),
          which(all_coords[,1]==end[1]&all_coords[,2]==end[2]),mode="out")

plot(g)
