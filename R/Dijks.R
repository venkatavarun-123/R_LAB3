#'Dijkstra algorithm for finding shortest paths between source node and all other nodes in a given graph.
#'@param graph A graph represented in the form of data frame with 3 columns namely v1,v2,w which represents starting node values,ending node values,weight of the path.
#'@param init_node A source node in the form of scalar from which shortest distance to every node is calculated.
#'@description The algorithm checks for indices of minimum distance and updates the distance value of its neighbours only if there exists a path from source to current node such that path distance is less than current distance value.
#'@return A vector containing shortest distance from source node to all other nodes.
#'@source https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm.
#'@export
dijkstra<-function(graph,init_node)
{
  if(is.numeric(init_node)== F || is.data.frame(graph) ==F){
    stop("Please enter valid inputs")
  }
  if(ncol(graph)!= 3||!(all(colnames(graph) == c("v1", "v2" , "w")))){
    stop("Please enter the graph  with correct column names")
  }
  if(!(init_node %in% unique(c(graph$v1,graph$v2)))){
    stop("The initial node given does not exist in the graph")
  }

  j=1
  k=1
  my_matrix<-matrix(NA,nrow=length(unique(graph$v1)),ncol=length(unique(graph$v1)))
  for(i in graph$v1)
  {
    my_matrix[i,graph$v2[j]]=graph$w[k]
    j=j+1
    k=k+1
  }
  my_matrix[is.na(my_matrix)]=0
  dist = rep(Inf, nrow(my_matrix))
  visited = rep(FALSE, nrow(my_matrix))
  dist[init_node] = 0
  repeat{
    minimum_distance = Inf
    minimum_index = -1
    for(i in seq_along(dist)) {
      if(dist[i] < minimum_distance && !visited[i]){
        minimum_distance = dist[i]
        minimum_index = i
      }
    }
    if(minimum_index == -1){
      return (dist)
    }
    for(i in seq_along(my_matrix[minimum_index,])) {
      if(my_matrix[minimum_index,i] != 0 && dist[i] > dist[minimum_index] + my_matrix[minimum_index,i]){
        dist[i] = dist[minimum_index] + my_matrix[minimum_index,i]
      }
      visited[minimum_index] = TRUE
    }
  }
}

