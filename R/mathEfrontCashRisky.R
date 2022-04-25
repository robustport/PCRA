#' mathEfrontCashRisky
#'
#' @param returns 
#' @param rf 
#' @param scalex 
#' @param scaley 
#' @param display.stocks 
#' @param stock.names 
#' @param risk.tol 
#' @param npoints 
#' @param plot.efront 
#' @param wts.plot 
#' @param equal.wts 
#' @param bar.ylim 
#' @param digits 
#'
#' @return
#' @export
#'
#' @examples
mathEfrontCashRisky <-
		function(returns,rf = 0.005,scalex = 1.2,scaley = 2,display.stocks = T,
				stock.names = T, risk.tol = F, npoints = 10, plot.efront = T, wts.plot = T,
				equal.wts = T,bar.ylim = c(0,1), digits = NULL)
{
	V = var(returns)
	mu.stocks = apply(returns, 2, mean)
	sigma.stocks = apply(returns, 2, sd)
	mue = mu.stocks - rf
	a = solve(V, mue)                  # a = Vinv * mue
	b = as.numeric(t(mue) %*% a)       # b = muetp * Vinv* mue
	sr.opt = sqrt(as.numeric(b))
	bsr = sqrt(b)
	mu.max = scaley * max(mu.stocks)   # Important for leverage
	muvals = seq(0,mu.max, length.out = npoints)
	sigmavals = c(muvals/bsr)
	muvalse = seq(0,mu.max, length.out = npoints)
	muvals = rf + muvalse
	inv.lambdavals = (muvals-rf)/b
	if(plot.efront)
	{
		if(wts.plot) {par(mfrow = c(1,2))}
		if(risk.tol) {
			x = inv.lambdavals; y = muvals; xlab = "Risk Tolerance"; par(mfrow=c(1,1))}
		else {
			x = sigmavals; y = muvals; xlab = "Portfolio Standard Deviation"}
		xlim = c(0,scalex*max(sigma.stocks,x))
		ylim = c(min(mu.stocks),max(muvals))
		plot(x, y, type = "l", xaxs = "i", lwd = 2, xlim = xlim, ylim = ylim,
				xlab = xlab, ylab = "Portfolio Mean Return")
		if(risk.tol)
		{display.stocks = F; stock.names = F}
		if(display.stocks)
		{points(sigma.stocks, mu.stocks, pch = 20)
			if(stock.names)
			{text(sigma.stocks + 0.02*xlim[2], mu.stocks, names(returns),
						cex = 0.5, adj = 0)}
		}
		x <- xlim[1] + .05 * (xlim[2] - xlim[1])
		y <- ylim[2] - .02 * (ylim[2] - ylim[1])
		text(x,y,paste("Rf = ",round(rf,3),sep = ""),pos = 4)
		y = y - .05*(ylim[2] - ylim[1])
		text(x,y,paste("SR = ",round(sr.opt,2),sep = ""),pos = 4)
		# Compute and plot equal weighted portfolio
		if(equal.wts)
		{n = length(mu.stocks)
			wts = rep(1/n,n)
			mu.eq = mean(mu.stocks)
			sigma.eq =as.numeric((t(wts)%*%V%*%wts)^.5)
			sr.eq = mu.eq/sigma.eq
			points(sigma.eq,mu.eq,pch=15,cex = 1.2)
			text(sigma.eq,mu.eq,"EQ.WTS",pos = 4, cex = .7)
		}
	}
# Compute cash and risky assets weights and barplot
	wts.risky = (sum(a)/b)*muvalse
	wts.cash = 1 - wts.risky
	wts.efront = rbind(muvals,sigmavals,wts.cash,wts.risky)
	row.names(wts.efront) = c("MU","VOL","Cash","Risky Assets")
	if(wts.plot)
	{barplotWts(wts.efront,legend.text = T,col = topo.colors(2),ylab = "Weights",
				xlab = "VOL",bar.ylim = bar.ylim);par(mfrow=c(1,1))}
	if(is.null(digits))
	{if(!risk.tol) {wts.efront}}
	else
	{if(equal.wts)
		{out = list(MU.EQ.WT = mu.eq, STDEV.EQ.WT = sigma.eq, SR.EQ.WT = sr.eq,
					SR.EFRONT = sr.opt)
			out = lapply(out,round,digits = digits)}
		else
		{out = list(SR.EFRONT = sr.opt)
			out = lapply(out,round,digits = digits)}
		out
	}
}
