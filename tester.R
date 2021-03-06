library(tidyverse)
library(viridis)
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

tbw <- list()
for(i in 1:200){
  source(file_list[i])
  tbw[[i]] <- one_server_total_bandwidth(g)
}

total.bw <- total.bw[!is.na(total.bw)]
d <- d[d!=0]
plot(total.p, total.bw)

model <- lm(d ~ 1/total.p)

rs <- data.frame(d, total.p)
t[[1]]

#Plot the values of min, max and total bw
ggplot(record.bw, aes(x = as.factor(min.bw), y = as.factor(max.bw)))+
  geom_tile(aes(fill = total.bw))+
  scale_fill_viridis(discrete = F)

plot(min.bw, total.bw)

plot(max.bw, total.bw)
        
res <- top_nodes(file_list[1:20])  




