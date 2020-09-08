#' Dijkstra's algorithm, finds shortest distance to other nodes
#' 
#' @param graph graph with 3 rows of nodes and edges.
#' @param init_node initial node (scalar).
#' @return  vector of distances
#' @examples
#' wiki_graph <-
#'  data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
#'           v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
#'           w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
#' dijkstra(wiki_graph, 1)
#' dijkstra(wiki_graph, 3)
#' 
#' @description ... Euclidean function, finds the greatest common divisor of two numbers. Find Wikipedia docs \href{http://en.wikipedia.org/wiki/Dijkstra%27s_algorithm}{Here}.
#' @export


dijkstra <- function(graph, init_node){
  
  #assertions
  stopifnot(is.data.frame(graph) && ncol(graph) == 3)
  stopifnot(colnames(graph) == c("v1","v2","w"))
  stopifnot(is.numeric(graph[[1]]) && is.numeric(graph[[2]]))
  stopifnot(is.numeric(init_node) && length(init_node) == 1 && is.element(init_node, graph[[1]]))
  
  # to make it universal, changing graph nodes to string
  graphCopy <- graph
  graphCopy[ ,1] <- ifelse(is.character(graphCopy[ ,1]), graphCopy[ ,1], lapply(graphCopy[1], as.character))
  graphCopy[ ,2] <- ifelse(is.character(graphCopy[ ,2]), graphCopy[ ,2], lapply(graphCopy[2], as.character))
  initNode <- toString(init_node)
  
  # Vector of nodes to check 
  checkNodeVector <- unique(graphCopy[ ,1])
  
  # Vector which holds distances to initial node
  distanceVector<-  rep(Inf, length(checkNodeVector))
  names(distanceVector) <- checkNodeVector
  
  
  distanceVector[initNode] <- 0
  while(length(checkNodeVector) > 0){
    #find node with lowest distance from node to check
    nodeDistanceToCheck <-distanceVector[names(distanceVector) %in% checkNodeVector]
    node <- nodeDistanceToCheck[which.min(nodeDistanceToCheck)]
    
    # extracting edges to be checked
    nodeEdges <- graphCopy[which(graphCopy[ ,1] == names(node)), ]
    
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