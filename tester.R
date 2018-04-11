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
network.connectivity <- compute_network_connectivity(file_list)

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

nodes <- vector()
edges.bw <- list()
for(i in 1:735){
  source(file_list[i])
  edges.bw[i] <- vcount(g)
  
}

 
mean.matrix <- matrix(nrow = 74, ncol = 74)

sd.matrix <- matrix(nrow = 74, ncol = 74)

for(j in 1:74){
  for(k in 1:74){
    b <- vector()
    for( i in 1:735){
      if(dim(adjacency_matrices[[i]])[1] < k | dim(adjacency_matrices[[i]])[1] < j)
      {
        break()
      }
      else{
      b[i] <- adjacency_matrices[[i]][j,k]
      sd.matrix[j,k] <- sd(b[b != 0]) 
      mean.matrix[j,k] <- mean(b[b != 0])
      }
    }
  }
}


plot(network.connectivity[,25])

r <- c()


for(i in 1:10){
  source(file_list[15])
  total.bw <- one_server_total_bandwidth(g)
  d <- degree(g)
  #r[i] <- summary(lm(t~ d))$r.squared
}

total.bw <- total.bw[!is.na(total.bw)]
d <- d[d!=0]
plot(total.p, total.bw)

model <- lm(d ~ 1/total.p)


rs <- data.frame(d, total.p)
t[[1]]


#Best worse bandwidth of NN

total.bw <- one_server_total_bandwidth(g)

min.bw <- c()
for(i in 1:length(total.bw)){
a <- g[i, as.vector(neighbors(g, i, mode = "out")), attr = "bw"]
a <- a[!is.na(a)]
if(length(a)!=0){
min.bw[i] <- min(a)
}
else{
  min.bw[i] <- NA
}
}

plot(min.bw, total.bw)

linear <- lm(total.bw ~ max.bw)
summary(linear)
