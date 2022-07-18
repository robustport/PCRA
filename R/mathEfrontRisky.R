#' Mathematical Efficient Frontier
#' @description Mathematical efficient frontier based on a data set of returns
#' @param returns 
#' @param npoints 
#' @param efront.only 
#' @param display 
#' @param digits 
#'
#' @return Values of efficient frontier
#' @export
#'
#' @examples
#' args(mathEfrontRisky)
mathEfrontRisky <-
		function(returns,npoints = 100,efront.only = T,display = T,digits = NULL)
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
