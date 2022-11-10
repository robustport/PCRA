#' SKest
#' 
#' Sample estimate of skewness
#' This function will eventually have a robust estimation option
#'
#' @param x A numeric vector
#'
#' @return Value of sample skewness
#' 
#' @examples
#' args(SKest)
#' @export
SKest =  function(x)
{
  n = length(x)
  k = 1/n
  SK <- k*sum(((x-mean(x))/(sd(x)*sqrt((n-1)/n)))^3)
  return(SK)
}