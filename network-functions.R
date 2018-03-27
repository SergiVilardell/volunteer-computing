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