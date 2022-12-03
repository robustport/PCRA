#' @title Long Short Portfolio Leverage
#'
#' @description This function computes a time series of portfolio leverages,
#' defined  as the sum of the absolute portfolio weights divided by the sum of
#' the long position weights
#'
#' @param wts Multivariate xts portfolio weights object
#'
#' @return Univariate xts time series of portfolio leverages
#'  
#' @rdname levgLongShort
#' @author Doug Martin
#'
#' @examples
#' args(levgLongShort)
#' 
#' @export
levgLongShort <- function(wts)
{
  # Input w must be a multivariate xts object (change code to allow a matrix)
  wMat <- coredata(wts)
  n <- dim(wMat)[1]
  levg <- rep(0,n)
  for(i in 1:n){
    wtVec <- wMat[i,]
    wtNonNeg <- wtVec[wtVec >= 0]
    wtNeg <- wtVec[wtVec < 0]
    levg[i] <- (sum(wtNonNeg) - sum(wtNeg))/sum(wtNonNeg)
  }
  as.xts(levg, order.by = index(wts))
}