euclidean <- function(value1, value2){
  
  stopifnot(is.numeric(value1) && length(value1) == 1 && value1 > 0)
  stopifnot(is.numeric(value2) && length(value2) == 1 && value2 > 0)
  
  while(value1 != value2){
    if(value1 > value2){
      value1 <- value1 - value2
    } else {
      value2 <- value2 - value1
    }
  }
  return(value1)
}