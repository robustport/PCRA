#' @title Global Minimum Variance Portfolios From Mu and Cov
#' 
#' @description Compute the weights, mean return and volatility of a GMV portfolio based on
#' user specified mean vector and covariance matrix
#' 
#' @param muRet Mean vector
#' @param volRet Volatility vector
#' @param corrRet matrix of correlations
#' @param digits Integer value number of decimal places, default 3
#' @return a list contains weights, mean return and volatility of a GMV portfolio
#' @examples
#' args(mathGmvMuCov)
#' @export
mathGmvMuCov <- function(muRet,volRet,corrRet, digits = 3) 
{
  covRet = diag(volRet)%*%corrRet%*%diag(volRet)
  names(muRet) = c("Stock 1","Stock 2","Stock 3")
  mu = muRet
  V = covRet
  one = rep(1, nrow(V))
  z1 = solve(V, one)  # Vinv*one
  a = as.numeric(t(mu) %*% z1) # a = mu*Vinv*one
  cc = as.numeric(t(one) %*% z1) # c = one*Vinv*one
  z2 = solve(V, mu) # Vinv*mu
  b = as.numeric(t(mu) %*% z2) # b = mu*Vinv*mu
  d = b * cc - a^2
  wtsGmv = z1/cc
  names(wtsGmv) = c("Stock 1","Stock 2","Stock 3")
  muGmv = a/cc
  varGmv = 1/cc
  volGmv = sqrt(varGmv)
  out = list(volGmv = volGmv, muGmv = muGmv, wtsGmv = wtsGmv)
  if (!is.null(digits)) {out = lapply(out,round, digits = 3)}
  out
}