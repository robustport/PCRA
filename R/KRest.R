#' KRest
#' 
#' Sample estimate of excess kurtosis, with option for kurtosis
#' This function will eventually have a robust estimation option
#'
#' @param x A numeric vector
#' @param excess A logical variable with TRUE resulting in computation
#' of excess kurtosis, and FALSE resulting in ordinary kurtosis
#'
#' @return The value of excess kurtosis or ordinary kurtosis
#'
#' @examples
#' args(KRest)
#' @export
KRest =  function(x,excess = TRUE)
{
  n = length(x)
  k = 1/n
  KR <- k*sum(((x-mean(x))/(sd(x)*sqrt((n-1)/n)))^4)
  if(excess){
    KR <- KR - 3} else
      KR
  if(excess){
    names(KR) <- "Excess Kurtosis"} else
      names(KR) <- "Kurtosis"
  return(KR)
}