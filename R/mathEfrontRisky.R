#' mathEfrontRisky
#'
#' @param returns 
#' @param npoints 
#' @param efront.only 
#' @param display 
#' @param digits 
#'
#' @return
#' @export
#'
#' @examples
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


#' mathEfrontRiskyMuCov
#'
#' @param muRet 
#' @param volRet 
#' @param corrRet 
#' @param npoints 
#' @param display 
#' @param efront.only 
#' @param digits 
#'
#' @return
#' @export
#'
#' @examples
mathEfrontRiskyMuCov <- function(muRet,volRet,corrRet, npoints = 100,display = T,
		efront.only = T, digits = NULL) 
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
		plot(sigma, mu.efront, type = "l", lwd = 3, xlim = xlim, 
				ylim = ylim, xlab = "VOLATILITY", ylab = "MEAN RETURN", 
				main = "PORTFOLIO FRONTIER")
		if (!efront.only) {
			lines(sigma, mu.front)
		}
		points(sigmaGmv, muGmv, pch = 19, cex = 1)
		text(sigmaGmv, muGmv, "GMV", cex = 1.2, pos = 2)
		points(sigma.stocks, mu, pch = 20, cex = 1.5)
		text(sigma.stocks, mu, names(mu), cex = 1.2, pos = 4)
		text(0.07, 0.095, "EFFICIENT FRONTIER", cex = 1.2)
		arrows(0.07, 0.09, sigma[15], mu.efront[15], length = 0.1, lwd= 1.8)
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
