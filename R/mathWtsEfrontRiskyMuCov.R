#' Portfolio Weights on an Efficient Frontier
#' 
#' Same as function \code{"mathWtsEfrontRisky"} except that instead a user specified
#' time series of portfolio return, it is based on user specified returns 
#' mean vector and covariance matrix
#'
#' @param muRet Vector of asset mean returns
#' @param volRet Vector of asset volatilies
#' @param corrRet Asset correlation matrix
#' @param mu.efront A vector of specified efficient frontier mean returns
#' @param digits If null no rounding of weights, else number of significant
#' digits for rounding
#'
#' @return
#' A matrix whose first row contains the volatilities along the efficient
#' at specified mean returns contained in the second row, and the remaining
#' n rows contain contain the n weight vectors at each of the efficient
#' frontier mean return values
#'
#' @examples
#' args(mathWtsEfrontRiskyMuCov)
#' @export
mathWtsEfrontRiskyMuCov <- function (muRet, volRet, corrRet, mu.efront,
                                     digits = NULL) 
{
  covRet <- diag(volRet) %*% corrRet %*% diag(volRet)
  mu <- muRet
  V <- covRet
  n.assets <- length(mu)
  one <- rep(1, n.assets)
  z1 <- solve(V, one)
  a <- as.numeric(t(mu) %*% z1)
  cc <- as.numeric(t(one) %*% z1)
  z2 <- solve(V, mu)
  b <- as.numeric(t(mu) %*% z2)
  d <- b * cc - a^2
  g1 <- matrix((b * z1 - a * z2)/d, ncol = 1)
  g2 <- matrix((cc * z2 - a * z1)/d, ncol = 1)
  n <- length(mu.efront)
  a1 <- matrix(rep(g1, n), ncol = n)
  mu.efrontMat <- matrix(mu.efront, nrow = 1)
  a2 <- g2 %*% mu.efrontMat
  wts.efront <- a1 + a2
  wts.efront <- as.data.frame(wts.efront)
  vol.efront <- (1/cc + (cc/d) * (mu.efront - a/cc)^2)^0.5
  out <- rbind(vol.efront, mu.efront, wts.efront)
  rowNames <- c("MU", "VOL", paste("W", sep = "", 1:n.assets))
  row.names(out) <- rowNames
  names(out) <- paste("P", sep = "", 1:n)
  if (is.null(digits)) {
    out
  }
  else {
    out <- round(out, digits)
    out
  }
}