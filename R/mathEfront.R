#' mathEfront
#'
#' @param returns 
#' @param mu.max 
#' @param sigma.max 
#' @param rf 
#' @param rf.line 
#' @param stocks 
#' @param stock.names 
#' @param values 
#' @param identify.stock.names 
#' @param npoints 
#' @param digits 
#'
#' @return
#' @export
#'
#' @examples
mathEfront = function(returns, mu.max=NULL, sigma.max=NULL, rf=0.005, rf.line=T, stocks=T,
      stock.names = T, values = T, identify.stock.names = F, npoints = 100, digits = NULL)
{
	C <- var(returns)
	mu <- apply(returns, 2, mean)
	one <- rep(1, nrow(C))
	z <- solve(C, one)					# z = C.inv * 1
	a <- as.numeric(t(mu) %*% z)		# a = mu.tp * C.inv * 1
	cc <- as.numeric(t(one) %*% z)	    # cc = 1.tp * C.inv * 1
	z <- solve(C, mu)					# z = C.inv * mu
	b <- as.numeric(t(mu) %*% z)		# b = mu.tp * C.inv* mu
	d <- b * cc - a^2
	mvp <- mathGmv(returns)
	tanp <- mathTport(returns, rf)
	sharpe <- round((tanp$muTan - rf)/(tanp$volTan), 3)
	# Compute tangent portfolio
	mu.stocks <- apply(returns, 2, mean)
	sigma.stocks <- apply(returns, 2, var)^0.5
	if(missing(mu.max))
        mu.max <- 2*max(mu.stocks)
    if(missing(sigma.max))
	   sigma.max <- (1/cc + (cc/d) * (mu.max - a/cc)^2)^0.5
	sigma <- seq(1/(cc^0.5), sigma.max, length = npoints)
	mu <- a/cc + ((d * sigma^2)/cc - d/cc^2)^0.5
	mu[1] <- a/cc					# Replace mu[1] NA due to 0^.5 with a/cc
	xlim <- range(0, sigma)
	ylim <- range(0, mu.max)
	x <- xlim[1] + 0.05 * (xlim[2] - xlim[1])
	y <- ylim[2] - 0.05 * (ylim[2] - ylim[1])
	plot(sigma, mu, xlim = xlim, ylim = ylim, type = "l", xlab = "VOL",
         ylab = "MEAN RETURN",xaxs = "i",cex = .9)
	title(main = "EFFICIENT FRONTIER")
	lines(sigma, mu, lwd = 2)
	points(mvp$vol,mvp$mu,pch = 16)
    text(mvp$vol,mvp$mu,"GMV",pos = 2,cex = .9)
	if(rf.line)
        {points(tanp$vol, tanp$mu, pch = 16)
        text(tanp$vol, tanp$mu, "T", pos = 2)
		abline(rf, (tanp$mu - rf)/tanp$vol, lty = 4)
        if(values)
            {text(x, y, paste("SHARPE RATIO = ", sharpe), adj = 0, cex = .6)
            y <- ylim[2] - 0.1 * (ylim[2] - ylim[1])
            text(x, y, paste("RISK-FREE = ", rf), adj = 0, cex = .6)
            points(0,rf,pch = 3,cex = 1.5)}
        }
	if(stocks) {
		points(sigma.stocks, mu.stocks,pch = 19,cex = .7)
		if(stock.names)
			text(sigma.stocks + 0.02 * xlim[2], mu.stocks, names(returns), cex = 0.7,
				adj = 0)
		else if(identify.stock.names)
			identify(sigma.stocks, mu.stocks, labels = names(along = sigma.stocks))
	}
	cat("\n", "SHARPE RATIO =", sharpe, sep = " ", "\n\n")
	print(mvp)
	print(tanp)
	invisible()
}
