#' Efficient Frontier
#' @description Efficient Frontier based on user specified mean vector and
#' covariance matrix
#'
#' @param muRet 
#' @param volRet 
#' @param corrRet 
#' @param npoints 
#' @param display 
#' @param efront.only 
#' @param print 
#' @param digits 
#'
#' @return Values of efficient frontier and optional plot
#' @export
#'
#' @examples
#' args(mathEfrontRiskyMuCov)
mathEfrontRiskyMuCov <- function(muRet,volRet,corrRet, npoints = 100,
                display = T, efront.only = T, print = F,  digits = NULL) 
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
  muGmv = a/cc
  varGmv = 1/cc
  sigmaGmv = sqrt(varGmv)
  sigma.stocks = sqrt(diag(V))
  mu.max = 1.2 * max(mu)
  sigma.max = (varGmv + 1/(d*varGmv) * (mu.max - muGmv)^2)^0.5
  #sigma.max = 1.2 * sigma.max
  sigma = seq(sigmaGmv + .000001, sigma.max, length = npoints)
  mu.efront = muGmv + (d*varGmv*(sigma^2 - varGmv))^0.5
  if (!efront.only) {
    mu.front = muGmv - (d*varGmv*(sigma^2 - varGmv))^0.5
  }
  xlim = c(0, max(sigma))
  if (efront.only) {
    ylim = range(mu.efront, mu, 0)
  } else
  {ylim = range(mu.efront, mu.front)}
  if (display) {
    plot(sigma, mu.efront, type = "l", lwd = 1.5, xlim = xlim, 
         ylim = ylim, xlab = "VOLATILITY", ylab = "MEAN RETURN")
    if (!efront.only) {
      lines(sigma, mu.front)
    }
    points(sigmaGmv, muGmv, pch = 19, cex = 1)
    text(sigmaGmv, muGmv, "GMV", cex = 1.2, pos = 2)
    points(sigma.stocks, mu, pch = 20, cex = 1.5)
    text(sigma.stocks, mu, names(mu), cex = 1.2, pos = 4)
    text(0.07, 0.095, "EFFICIENT FRONTIER", cex = 1.2)
    arrows(0.07, 0.09, sigma[15], mu.efront[15], length = 0.1, lwd= 1.5)
  }
  if (is.null(digits)) {
    out = list(mu.efront = mu.efront, vol.efront = sigma)
  } else {
    vol.efront = sigma
    out = rbind(mu.efront, vol.efront)
    out = round(out, digits = digits)
  }
  out
}