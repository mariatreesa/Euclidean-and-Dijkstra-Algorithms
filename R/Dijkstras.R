#' Function to get the shortest distance to all the nodes from an initial node
#' Authors: Maria Treesa Sebastian(marse306), Brian Masinde(brima748), Omkar(omkbh878)
#' @param graph as dataframe
#' @param init_node as numeric
#'
#' @return Shortest distance to all vertex from init_node . The wikipage link to algorithm : <https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm>
#'
#' @export
#'
#' @examples dijkstra(graph = data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9)),init_node = 1)
#'
#'
dijkstra <- function(graph, init_node){


  dist <- c() # initializing an empty distance vector
  vertex = unique(c(graph$v1,graph$v2))  # getting the list of vertex from the input graph
  dist[init_node] <-0  # setting distance as 0 to the starting node to itself


  for(v in vertex){     # in a for loop we set the distance of all nodes from inital node to infinity
    if(v != init_node){  # this is done for comparison
      dist[v] <- Inf
    }
  }
  Qset <- data.frame(vertex,dist)  # creating a Qset for keeping track of all unvisited nodes
  result <- data.frame(vertex,dist) # the result data frame for storing final results

  while(length(Qset$vertex) > 0){   # looping until uvisited set is empty
    mindistance <- min(Qset$dist)  # getting minimum distance in unvisited set
    mindistindex <- which(Qset$dist == mindistance)  # getting index of the minimum distance inorder to find the vertex
    mindistvertex <- Qset$vertex[mindistindex]   # getting the vertex to which we have the minimum distance
    Qset <- Qset[-c(mindistindex),]  # deleting the entry from qset which has minimum distance

    neighbours <- graph[graph$v1==mindistvertex,]$v2 # getting the neighbours for min distance entry that we got in the step above

    # in the loop below we are going through all the neighbour vertex and finding the
    # distance for each node from the previous node ( which is mindistvertex)
    # then checks if the distance of the node which is already populated is less than the
    #distance that we calculated now and if so populate the new distance(which is the shortest)
    for (n in neighbours){
      g <- graph[graph$v1==mindistvertex,]
      g1 <- g[g$v2==n,] # filter the graph to have entries which corresponds to the mindistvertex and the current neighbour
      new_distance <- mindistance + g1$w  # calculate the distance of neighbour from mindisvertex
      if(new_distance < result[result$vertex==n,]$dist){  # populate the distance if it is less that the distance already calculated for that vertex.
        result[result$vertex==n,]$dist <- new_distance
        Qset[Qset$vertex==n,]$dist <- new_distance
      }
     }
  }
 # r <- as.vector(result$dist)
  return(as.vector(result$dist))
}

#examples to run dijkstra
wiki_graph <-
  data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
             v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
             w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))

dijkstra(wiki_graph, 1)
#[1] 0 7 9 20 20 11
dijkstra(wiki_graph, 3)
#[1] 9 10 0 11 11 2
