#' Math Weights on Efficient Frontier
#' @description Computes mean, volatility and weights vector along the efficient frontier
#' computed based a set of returns
#' @param returns 
#' @param mu.efront 
#' @param digits 
#'
#' @return A matrix with the mean, volatility and weights vector at each of a
#' set of points along the efficient frontier.
#' @export
#'
#' @examples
#' args(mathWtsEfrontRisky)
mathWtsEfrontRisky <- function (returns, mu.efront, digits = NULL) 
{
  V = var(returns)
  mu = apply(returns, 2, mean)
  n.assets <- length(mu)
  one <- rep(1, n.assets)
  z1 = solve(V, one)
  a = as.numeric(t(mu) %*% z1)
  cc = as.numeric(t(one) %*% z1)
  z2 = solve(V, mu)
  b = as.numeric(t(mu) %*% z2)
  d = b * cc - a^2
  g1 = as.matrix((b * z1 - a * z2)/d, ncol = 1)
  g2 = as.matrix((cc * z2 - a * z1)/d, ncol = 1)
  n = length(mu.efront)
  a1 = matrix(rep(g1, n), ncol = n)
  a2 = g2 %*% mu.efront
  wts.efront = a1 + a2
  wts.efront <- as.data.frame(wts.efront)
  vol.efront <- (1/cc + (cc/d) * (mu.efront - a/cc)^2)^0.5
  out <- rbind(vol.efront, mu.efront, wts.efront)
  rowNames <- c("Vol", "Mean", paste("W-", sep = "", names(returns)))
  row.names(out) <- rowNames
  names(out) <- paste("P", sep = "", 1:n)
  if (is.null(digits)) {
    out
  }
  else {
    out <- sapply(out, FUN = round, digits = digits)
    row.names(out) = rowNames
    data.frame(out)
  }
}
