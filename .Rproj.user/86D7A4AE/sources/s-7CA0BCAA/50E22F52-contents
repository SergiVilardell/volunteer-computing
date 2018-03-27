library(tidyverse)
source("graph-functions.R")
source("network-functions.R")


path = "/home/bill/Doc/traces"
file_list <- list.files(path, pattern = ".*\\.R", full.names=TRUE)
#print.plots(file_list)
failed.nodes <- compute_failed_nodes(file_list)
active.nodes <- compute_active_nodes(file_list)
network.timeline <- compute_network_timeline(failed.nodes, active.nodes)
node.active.time <- sapply(network.timeline, function(x) sum(x, na.rm = TRUE)/735 )
plot(node.active.time)
consecutive.active.time <- sapply(network.timeline, function(x) rle(x)$lengths)
consecutive.value <- sapply(network.timeline, function(x) rle(x)$value)
adjacency_matrices <- compute_adjacency_matrices(file_list)


a <- unlist(consecutive.active.time)
b <- unlist(consecutive.value)

c <- vector()

for(i in 1:length(a)){
  if(!is.na(b[i]) && b[i]==0){
    c[i] <- i
  }
}

c <- c[!is.na(c)]
active.consecutives <- a[-c]


get.edgelist(g)

adj <- as_adjacency_matrix(g, attr = "bw", sparse = T)
l <- list()
l[1] <- adj
z <- as.numeric(table(active.consecutives))
l <- as.numeric(names(table(active.consecutives)))

plot(l, z, log= "xy")

z <- hist(log(active.consecutives))
z <- hist(active.consecutives, breaks = exp(z$breaks))
plot(z$mids, z$counts, log = "xy")

hist(active.consecutives)
barplot(table(active.consecutives), log= "y")

nodes <- vector()
edges.bw <- list()
for(i in 1:735){
  source(file_list[i])
  edges.bw[i] <- vcount(g)
  
}
