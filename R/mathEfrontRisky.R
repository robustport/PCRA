#' mathEfrontRisky
#' 
#' Compute the efficient frontier of a fully invested portfolio
#' based on a data set of returns.  Uses the mathematical formula based on
#' the estimated mean return vector and covariance matrix.  Can have short
#' positions.
#' 
#' @param returns A multivariate xts object of returns
#' @param npoints The number of points computed on the efficient frontier
#' @param efront.only If true only the efficient frontier is computed, and 
#' otherwise the entire frontier is computed
#' @param display If TRUE the efficient frontier is plotted
#' @param digits If NULL no rounding is done, else specifies the number of 
#' significant digits
#'
#' @return Values portfolio mean and volatility along efficient frontier
#' @export
#'
#' @examples
#' args(mathEfrontRisky)
mathEfrontRisky <-
		function(returns,npoints = 100,efront.only = TRUE,display = T,digits = NULL)
{
	V = var(returns)
	mu = apply(returns, 2, mean)
	one = rep(1, nrow(V))
	z = solve(V, one)               # z = Vinv * 1
	a = as.numeric(t(mu) %*% z)     # a = mutp * Vinv * 1
	cc = as.numeric(t(one) %*% z)   # cc = 1tp * Vinv * 1
	z = solve(V, mu)                # z = Vinv * mu
	b = as.numeric(t(mu) %*% z)     # b = mutp * Vinv* mu
	d = b * cc - a^2
	gmv = mathGmv(returns)
	sigma.gmv = gmv$vol
	mu.stocks = apply(returns, 2, mean)
	sigma.stocks = apply(returns, 2, var)^0.5
	mu.max = 2*max(mu.stocks)
	sigma.max = (1/cc + (cc/d) * (mu.max - a/cc)^2)^0.5
	sigma.max = 1.2*sigma.max
	sigma = seq(sigma.gmv, sigma.max, length = npoints)
	mu.efront = a/cc + ((d * sigma^2)/cc - d/cc^2)^0.5
	if(!efront.only) {mu.front = a/cc - ((d * sigma^2)/cc - d/cc^2)^0.5}
	mu[1] = a/cc					# Replace mu[1] NA
	xlim = c(0, max(sigma))
	if(efront.only) {ylim = range(mu.efront,mu.stocks)}
	else
	{ylim = range(mu.efront, mu.front)}
	if(display)
	{plot(sigma, mu.efront, type = "l", lwd = 3, xlim = xlim, ylim = ylim,
				xlab = "VOL",ylab = "MEAN RETURN", main = "PORTFOLIO FRONTIER")
		if(!efront.only) {lines(sigma,mu.front)}	
		points(gmv$vol, gmv$mu, pch = 19)
		text(gmv$vol, gmv$mu,"GMV",cex = 1.2, pos = 2)
		points(sigma.stocks, mu.stocks, pch = 20)
		text(sigma.stocks, mu.stocks, names(returns), cex = 0.5,pos = 4)
		text(.07,.06,"EFFICIENT FRONTIER")
		arrows(.07,.056,.09,.038,length = .1)
	}
	if(is.null(digits))
	{out = list(mu.efront = mu.efront,vol.efront = sigma)}
	else
	{vol.efront = sigma; out = rbind(mu.efront, vol.efront)
		out = round(out,digits=digits)}
	out
}
