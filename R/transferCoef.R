#' @title Transfer Coefficent
#'
#' @description Computes the transfer coefficient (TF), which measures
#' the reduction in mean excess return of an MV portfolio with weights
#' constraint relative to the mean excess return of an unconstrained
#' MV portfolio.  This description also holds with portfolios mean
#' returns replaced by Sharpe ratios.
#'
#' @param returns An xts multivariate returns object that contains
#' the returns of the risk-free T-Bill in the last column 
#' @param wtVec The weight vector of a constrained MV portfolio 
#'
#' @return Numeric value of the TC
#' @export
#'
#' @examples
#' args(transferCoef)
transferCoef <- function(returns, wtVec)
{
  n <- length(names(returns))
  n1 <- n - 1
  mueVec <- apply(returns[,1:n1], 2, mean) - mean(returns[ , n])
  covmat <- cov(returns[ , 1:n1])
  covmatInv <- solve(covmat)
  a0 <- wtVec%*%mueVec
  a1 <- t(wtVec)%*%covmat%*%wtVec
  a2 <- t(mueVec)%*%covmatInv%*%mueVec
  TC <- a0/sqrt(a1*a2)
  return(TC)
}