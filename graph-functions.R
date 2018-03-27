library(igraph)

print.plots <- function(file_list){
  
  #Codification to easily make a video out of the plots  
  file_num <- 1:length(file_list)
  file_num <- sprintf("%03d", file_num)
  
  #Generate the plots
  for(i in 1:length(a)){
    
    set.seed(137) 
    
    # .R file wih the graph  
    source(a[i], chdir = T)
    filename <- paste("plots/",x[i], sep = "")
    png(paste(filename,".png", sep = ""), width = 720, height = 720)
    
    #Fix the layout of the nodes for every plot
    layout <- layout.norm(layout, -1, 1, -1, 1)
    
    #Plot the graph "g"
    plot(g, layout=layout, vertex.label = NA,vertex.size=6,  edge.arrow.size=.1)
    dev.off()
  }
}

get_isolated_nodes <- function(g){
  
    return( list(which(degree(g)==0)) )
}

get_non_isolated_nodes <- function(g){
  
  return( list(which(degree(g)>0)) )
}

compute_failed_nodes <- function(file_list){
  
  isolated.nodes <- list()
  
  for( i in 1:length(file_list)){
    source(file_list[i])
    isolated.nodes[i] <- get_isolated_nodes(g)
    
  }
  
  return(isolated.nodes)
  
}

compute_active_nodes <- function(file_list){
  
  non.isolated.nodes <- list()
  
  for( i in 1:length(file_list)){
    source(file_list[i])
    non.isolated.nodes[i] <- get_non_isolated_nodes(g)
    
  }
  
  return(non.isolated.nodes)
  
}


compute_adjacency_matrices <- function(file_list){
  adjacency_matrices <- list()
  
  for( i in 1:length(file_list)){
    source(file_list[i])
    adjacency_matrices[[i]] <- as_adjacency_matrix(g, attr = "bw")
  }
  return(adjacency_matrices)
}