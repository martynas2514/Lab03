#' Finds the greatest common divisor of two numbers
#'
#'\code{euclidean}
#' @param value1 A number.
#' @param value2 A number.
#' @return  Greatest common divisor.
#' @examples
#' euclidean(123612, 13892347912)
#' euclidean(100, 1000)
#' @export

euclidean <- function(value1, value2){
  
  stopifnot(is.numeric(value1) && length(value1) == 1 && value1 >= 0)
  stopifnot(is.numeric(value2) && length(value2) == 1 && value2 >= 0)
  
  while(value1 != value2){
    if(value1 > value2){
      value1 <- value1 - value2
    } else {
      value2 <- value2 - value1
    }
  }
  return(value1)
}
