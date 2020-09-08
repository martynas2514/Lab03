#' Euclidean function, finds the greatest common divisor of two numbers
#' 
#' @param value1 An integer number must be introduced
#' @param value2 An integer number must be introduced
#' @return It will return the greatest common divisor between value 1 and value 2. An integer will be returned.
#' @examples
#' euclidean(123612, 13892347912)
#' euclidean(100, 1000)
#' 
#' @description 
#' Euclidean function, finds the greatest common divisor of two numbers. Find Wikipedia docs \href{https://en.wikipedia.org/wiki/Euclidean}{Here}.
#' @export

euclidean <- function(value1, value2){
  
  stopifnot(is.numeric(value1) && length(value1) == 1) 
  stopifnot(is.numeric(value2) && length(value2) == 1)
  value1 <- abs(value1)
  value2 <- abs(value2)
  if (value1==0 && value2==0){
    value1 <- value1
  }
  else if (value1 == 0 && value2 != 0){
    value1 <- value2
  }
  else if (value1 != 0 && value2 == 0){
    value1 <- value1
  }
  
  else {while(value1 != value2){
    if(value1 > value2){
      value1 <- value1 - value2
    } 
    else {
      value2 <- value2 - value1
    }
    
  }
  }
  return(value1)
}
