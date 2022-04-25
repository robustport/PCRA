#' this is a test
#'
#' @param test 
#' @param x 
#' @param y 
#'
#' @return
#' @export
#'
#' @examples
testFun <- function(test,x,y){
  print(test)
  z1 = x+y
  z2 = x-y
  return(list(z1,z2))
}



