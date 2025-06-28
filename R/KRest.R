#' @title Kurtosis Estimator
#' 
#' @description Sample estimate of excess kurtosis, with option for ordinary
#' kurtosis. This function will eventually have a robust estimator option.

#' @param x A numeric vector
#' @param excess A logical variable with default TRUE, which results in the
#' computation of excess kurtosis, and FALSE results ordinary kurtosis.
#'
#' @return numeric value of excess kurtosis or ordinary kurtosis
#'
#' @examples
#' args(KRest)
#' @export
KRest <- function(x, excess = TRUE)
{
  n <- length(x)
  # this is a github learning comments
  k <- 1/n
  KR <- k*sum(((x-mean(x))/(sd(x)*sqrt((n-1)/n)))^4)
  if(excess){
    KR <- KR - 3} else
    KR
  return(KR)
}