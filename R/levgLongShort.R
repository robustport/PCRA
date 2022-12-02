#' @title Long Short Portfolio Leverage
#'
#' @description This function computes a time series of portfolio leverages,
#' defined  as the sum of the absolute portfolio weights divided by the sum of
#' the long position weights
#'
#' @param w a multivariate xts portfolio weights object
#'
#' @return levg   a univariate xts time series of portfolio leverages
#'  
#' @rdname levgLongShort
#' @author Doug Martin
#'
#' @examples
#' args(levgLongShort)
#' 
#' @export
levgLongShort <- function(w)
{
  # Input w must be a multivariate xts object (change code to allow a matrix)
  wMat <- coredata(w)
  n <- dim(wMat)[1]
  levg <- rep(0,n)
  for(i in 1:n){
    wt <- wMat[i,]
    wtNonNeg <- wt[wt >= 0]
    wtNeg <- wt[wt < 0]
    levg[i] <- (sum(wtNonNeg)-sum(wtNeg))/sum(wtNonNeg)
  }
  ind <- index(w)
  as.xts(levg,order.by = index(w))
}