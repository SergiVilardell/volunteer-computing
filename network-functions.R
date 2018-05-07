source("graph-functions.R")


compute_network_timeline <- function(failed.nodes, active.nodes){
  
  network.timeline <- as.data.frame(matrix(nrow = 735, ncol = 74))
  
  for(i in 1:735){
    for( j in 1:74){
      if(j %in% failed.nodes[[i]]){
        network.timeline[i, j] <- 0
      }
      else if(j %in% active.nodes[[i]]){
        network.timeline[i, j] <- 1
      }
      else{
        network.timeline[i, j] <- NA
      }
    }
  }
  
  return(network.timeline)
}

compute_network_connectivity <- function(file_list){
  
  network.timeline.connectivity <- as.data.frame(matrix(nrow = 735, ncol = 74))
  for(i in 1:735){
    source(file_list[i])
    a <- degree(g)
    for(j in 1:74)
      if(length(a) < j){
        network.timeline.connectivity[i,j] <- NA
      }
      else{
        network.timeline.connectivity[i,j] <- a[j]
      }
    
    
  }
  return(network.timeline.connectivity)
}



#Best worse bandwidth of NN

top_nodes <- function(file_list){
  
  is.optimal <- c()
  res <- c()
  sampled.points <- c()
  for( j in 1:200){
    source(file_list[j])
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
    
    
    max.bw <- c()
    for(i in 1:length(total.bw)){
      a <- g[i, as.vector(neighbors(g, i, mode = "out")), attr = "bw"]
      a <- a[!is.na(a)]
      if(length(a)!= 0){
        max.bw[i] <- max(a)
      }
      else{
        max.bw[i] <- NA
      }
    }
    
    
    record.bw <- data.frame(min.bw, max.bw, total.bw)
    
    #Check if the top half pairs of min and max bw are the most promising solutions overall.
    
    best.min.bw <- record.bw %>% 
      arrange(desc(min.bw))
    
    cut <- floor(dim(best.min.bw)[1]/4)
    best.min.bw <- best.min.bw[1:cut,] 
    
    
    best.max.bw <- record.bw %>% 
      arrange(desc(max.bw))
    
    optimal <- sort(best.max.bw$total.bw, decreasing = T)[1]
    
    cut <- floor(dim(best.max.bw)[1]/4)
    best.max.bw <- best.max.bw[1:cut,]
    
    common <- inner_join(best.max.bw, best.min.bw)
    
    sorted.total.bw <- sort(total.bw, decreasing = T)
    
    sampled.points[j] <- dim(common)[1]/dim(record.bw)[1]
    is.optimal[j] <- optimal %in% common$total.bw 
    cut <- floor(length(sorted.total.bw)/10)
    cutted.total.bw <- sorted.total.bw[1:cut] 
    
    top <- intersect(common$total.bw, cutted.total.bw)
    
    res[j] <- length(top)/cut
  }
  return(res)
}



top_min_nodes <- function(file_list){
  
  is.optimal <- c()
  res <- c()
  sampled.points <- c()
  for( j in 1:600){
    source(file_list[j])
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
    
    
    
    record.bw <- data.frame(min.bw, total.bw)
    
    #Check if the top half pairs of min and max bw are the most promising solutions overall.
    
    best.min.bw <- record.bw %>% 
      arrange(desc(min.bw))
    
    cut <- floor(dim(best.min.bw)[1]/10)
    best.min.bw <- best.min.bw[1:cut,] 
    
    optimal <- sort(best.min.bw$total.bw, decreasing = T)[1]
    
    sorted.total.bw <- sort(total.bw, decreasing = T)
    
    is.optimal[j] <- optimal %in% best.min.bw$total.bw 
    cut <- floor(length(sorted.total.bw)/10)
    cutted.total.bw <- sorted.total.bw[1:cut] 
    
    top <- intersect(best.min.bw$total.bw, cutted.total.bw)
    
    res[j] <- length(top)/cut
  }
  return(res)
}

for(i in 1:20){
ts.plot(df[, i], gpars= list(col=rainbow(20) ))
}

