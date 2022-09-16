#' mathEfrontCashRisky
#' 
#' This function computes and plots a linear efficient frontier that is
#' a mix of a risk-free asset ("cash") and risky stocks (or other assets).
#'
#' @param returns Risky asset returns in the form of an xts object 
#' @param rf A risk-free rate
#' @param scalex A scale parameter for scaling the horizontal axis
#' @param scaley A scale parameter for scaling the vertical axis
#' @param display.stocks Logical parameter which if TRUE results in stocks
#' being displayed as points in the figures
#' @param stock.names Logical variable which if TRUE results in stock names 
#' being displayed 
#' @param risk.tol A parameter equal to the reciprocal of risk aversion
#' @param npoints Number of points at which to compute efront values and
#' weights
#' @param plot.efront Logical variable which if TRUE results in a plot of
#' the linear efficient frontier
#' @param wts.plot Logical variable which if TRUE results in a plot of the 
#' efficient frontier weights at npoints
#' @param equal.wts Logical variable which if TRUE results in computing and
#' displaying the mean and vol location of an equal weighted portfolio
#' @param bar.ylim Numerical character vector with upper and lower vertical
#' axis limits of a weight barplot display
#' @param values
#' @param digits Number of digits for printing efront values
#'
#' @return  A plot of the linear efficient frontier or the weights along the
#' efficient frontier.  Optionally output the mean and vol values
#' of the linear efficient frontier or its weights, or both.
#' @export
#'
#' @examples
#' args(mathEfrontCashRisky)
mathEfrontCashRisky <-
		function(returns,rf = 0.005,scalex = 1.1, scaley = 1.1 ,display.stocks = TRUE,
				stock.names = TRUE, npoints = 10, plot.efront = TRUE,
				wts.plot = TRUE, equal.wts = TRUE, bar.ylim = c(0,1),
				cexPoints = 0.8, cexText = 0.8,
				values = FALSE, digits = NULL)
{
	V = var(returns)
	mu.stocks = apply(returns, 2, mean)
	sigma.stocks = apply(returns, 2, sd)
	mue = mu.stocks - rf
	a = solve(V, mue)                  # a = Vinv * mue
	b = as.numeric(t(mue) %*% a)       # b = muetp * Vinv* mue
	sr.opt = sqrt(as.numeric(b))
	bsr = sqrt(b)
	mu.max = scaley * max(mu.stocks)
	muvals = seq(0,mu.max, length.out = npoints)
	sigmavals = c(muvals/bsr)
	muvalse = seq(0,mu.max, length.out = npoints)
	muvals = rf + muvalse
	inv.lambdavals = (muvals-rf)/b
	if(plot.efront)
	{
		if(wts.plot) {par(mfrow = c(1,2))}
		x = sigmavals; y = muvals
		xlim = c(0,scalex*max(sigma.stocks,x))
		ylim = c(min(mu.stocks),max(muvals))
		plot(x, y, type = "l", xaxs = "i", lwd = 2, xlim = xlim, ylim = ylim,
				xlab = "Volatility", ylab = "Mean Return")
		if(display.stocks){
		points(sigma.stocks, mu.stocks, pch = 20, cex = cexPoints)
		if(stock.names){
		  text(sigma.stocks + 0.02*xlim[2], mu.stocks, names(returns),
						cex = cexText, adj = 0)
		  }
		}
		x <- xlim[1] + .05 * (xlim[2] - xlim[1])
		y <- ylim[2] - .02 * (ylim[2] - ylim[1])
		text(x,y,paste("SR = ",round(sr.opt,2),sep = ""),pos = 4)
		y = y - .07*(ylim[2] - ylim[1])
		text(x,y,paste("RF= ",round(rf,3),sep = ""),pos = 4)

# Compute cash and risky assets weights and barplot
	wts.risky = (sum(a)/b)*muvalse
	wts.cash = 1 - wts.risky
	wts.efront = rbind(muvals,sigmavals,wts.cash,wts.risky)
	row.names(wts.efront) = c("MU","VOL","Cash","Risky Assets")
	if(wts.plot){
	  barplotWts(wts.efront,legend.text = T,col = topo.colors(2),ylab = "Weights",
	             xlab = "VOL",bar.ylim = bar.ylim);par(mfrow=c(1,1))
  	}

	if(values){
	  if(is.null(digits)){
	    out <- wts.efront
	  } else{
	    out <- lapply(wts.efront,round,digits = digits)}
	  }
	    out
	}
}
