#' Dijkstra's algorithm, finds shortest distance to other nodes
#' 
#' @param graph Data.frame object with three variables (v1, v2 and w) that contains the edges of the graph (from v1 to v2) with the weight of the edge (w)
#' @param init_node initial node (must be a scalar number).
#' @return  vector of distances
#' @description  
#' The algorithm takes a graph and an initial node and calculates the shortest path from the initial node to every other node in the graph. Find Wikipedia docs \href{https://cutt.ly/SfE5Er1}{Here}.
#' @examples
#' wiki_graph <-
#'  data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
#'           v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
#'           w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
#' dijkstra(wiki_graph, 1)
#' dijkstra(wiki_graph, 3)
#' @export


dijkstra <- function(graph, init_node){
  
  #assertions
  stopifnot(is.data.frame(graph) && ncol(graph) == 3)
  stopifnot(colnames(graph) == c("v1", "v2", "w"))
  stopifnot(is.numeric(graph[[1]]) && is.numeric(graph[[2]]))
  stopifnot(is.numeric(init_node) && length(init_node) == 1 && is.element(init_node, graph[[1]]))
  
  # to make it universal, changing graph nodes to string
  graph[ , 1] <- ifelse(is.character(graph[ ,1]), graph[ ,1], lapply(graph[1], as.character))
  graph[ , 2] <- ifelse(is.character(graph[ ,2]), graph[ ,2], lapply(graph[2], as.character))
  initNode <- toString(init_node)
  
  # Vector of nodes to check 
  checkNodeVector <- unique(graph[ ,1])
  
  # Vector which holds distances to initial node
  distanceVector<-  rep(Inf, length(checkNodeVector))
  names(distanceVector) <- checkNodeVector
  
  
  distanceVector[initNode] <- 0
  while(length(checkNodeVector) > 0){
    #find node with lowest distance from node to check
    nodeDistanceToCheck <- distanceVector[names(distanceVector) %in% checkNodeVector]
    node <- nodeDistanceToCheck[which.min(nodeDistanceToCheck)]
    
    # extracting edges to be checked
    nodeEdges <- graph[which(graph[ ,1] == names(node)), ]
    
    # apply distances to distance vector
    for (i in 1:nrow(nodeEdges)) {
      if(distanceVector[nodeEdges[i, 2]] > node + nodeEdges[i, 3]){
        distanceVector[nodeEdges[i, 2]] <- node + nodeEdges[i, 3]
      }
    }
    
    #removing current node from list of nodes to be checked
    checkNodeVector <- checkNodeVector[ -which(checkNodeVector == names(node))]
  }
  return(unname(distanceVector))
}