#' @title Efficient Frontier of Risky Stocks
#' 
#' @description Computes and plots the efficient frontier of risky assets
#' only, using a multivariate time series of returns to compute the mean
#' vector and covariance matrix
#' 
#' @param returns Multivariate xts object of portfolio returns
#' @param npoints Integer number of efficient frontier points, with default 100
#' @param efront.only Logical variable with default TRUE
#' @param display If TRUE the efficient frontier is plotted
#' @param values Logical variable with default TRUE
#' @param digits Integer variable number of significant digits, default NULL
#' @param cexGmv A size parameter for the text "GMV"
#' @param pchPoints A parameter of the type of points
#' @param cexPoints A size parameter of points
#' @param cexText A size parameter of text
#' 
#' @details When efront.only = TRUE only the efficient frontier is computed,
#' and if FALSE the entire frontier is computed.  When value = TRUE the
#' efficient frontier mean and volatility values are returned, and when
#' value = FALSE these values are not returned.
#'
#' @return A plot of a risky assets only efficient frontier, or optionally
#' a plot of the entier frontier
#' @export
#'
#' @examples
#' args(mathEfrontRisky)
mathEfrontRisky <-
  function(returns, npoints = 100, efront.only = TRUE, display = TRUE,
           cexGmv = 0.9, pchPoints = 20, cexPoints = 1.0, cexText = 0.7,
           values = FALSE, digits = NULL)
  {
    V <- var(returns)
    mu <- apply(returns, 2, mean)
    one <- rep(1, nrow(V))
    z <- solve(V, one)               # z = Vinv * 1
    a <- as.numeric(t(mu) %*% z)     # a = mutp * Vinv * 1
    cc <- as.numeric(t(one) %*% z)   # cc = 1tp * Vinv * 1
    z <- solve(V, mu)                # z = Vinv * mu
    b <- as.numeric(t(mu) %*% z)     # b = mutp * Vinv* mu
    d <- b * cc - a^2
    gmv <- mathGmv(returns)
    sigma.gmv <- gmv$vol
    mu.stocks <- apply(returns, 2, mean)
    sigma.stocks <- apply(returns, 2, var)^0.5
    mu.max <- 1.1*max(mu.stocks)
    sigma.max <- (1/cc + (cc/d) * (mu.max - a/cc)^2)^0.5
    sigma.max <- 1.1*sigma.max
    sigma <- seq(sigma.gmv, sigma.max, length = npoints)
    mu.efront <- a/cc + ((d * sigma^2)/cc - d/cc^2)^0.5
    if(!efront.only) {mu.front <- a/cc - ((d * sigma^2)/cc - d/cc^2)^0.5}
    mu.efront[1] <- a/cc					# Replace mu[1] NA
    xlim <- c(0.6*sigma.gmv, 1.2*max(c(sigma,sigma.stocks)))
    if(efront.only) {ylim <- range(mu.efront,mu.stocks)} else
    {ylim <- range(mu.efront, mu.front)}
    if(display)
    {plot(sigma, mu.efront, type = "l", lwd = 1.3, xlim = xlim, ylim = ylim,
          xlab = "StdDev",ylab = "Mean Return")
      if(!efront.only) {lines(sigma, mu.front)}	
      points(gmv$vol, gmv$mu, pch = 19)
      text(gmv$vol, gmv$mu,"GMV", cex = cexGmv, pos = 4)
      points(sigma.stocks, mu.stocks, pch = pchPoints, cex = cexPoints)
      text(sigma.stocks, mu.stocks, names(returns), cex = cexText, pos = 4)
    }
    if(values == TRUE) {
      if(is.null(digits))
      {out <- list(mu.efront = mu.efront, vol.efront = sigma)} else
      {vol.efront <- sigma; out = rbind(mu.efront, vol.efront)
      out <- round(out, digits = digits)}
      out
    }
  }
