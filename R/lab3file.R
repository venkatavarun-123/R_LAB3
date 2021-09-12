#'Euclidean algorithm to find greatest common divisor
#'The function takes two numbers for which GCD will be calculated as arguments
#'The algorithm replaces larger of the two numbers by the remainder when divided by smaller of two numbers and stops when remainder equals zero
#'@return The greatest common divisor of two numbers passed in as arguments
#' https://en.wikipedia.org/wiki/Euclidean_algorithm

euclidean<-function(x,y)
{
  if(is.numeric(x)==TRUE && is.numeric(y)==TRUE && length(x)==1L && length(y)==1L)
  {
    if(x==0)
      return(y)
  return(euclidean(y%%x,x))
  }
  else
    stop()
}

#'Dijkstra algorithm for finding shortest paths between source node and all other nodes in a given graph
#'The function takes two arguments 1:graph in the form of data frame which has three variables v1,v2,w representing starting node,ending node,weight for the path
#'The algorithm checks for indices of minimum distance and updates the distance value of its neighbours only if there exists a path from source to current node such that path distance is less than current distance value
#'@return A vector containig shortest distance from source node to all other nodes
#'https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm 

dijkstra<-function(graph,init_node)
{
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

