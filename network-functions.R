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




