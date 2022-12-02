#' @title Math Efficient Frontier: Cash and Risky Assets
#' 
#' @description This function computes and plots a linear efficient frontier that is
#' a mix of a risk-free asset ("cash") and risky stocks (or other assets).
#' It optionally returns the weights along the linear efficient frontier.
#'
#' @param returns Risky asset returns multivariate xts object 
#' @param rf A risk-free rate with default 0.003
#' @param scalex Horizontal axis scale parameter with default 1.1
#' @param scaley Vertical axis scale parameter with default 1.1
#' @param stock.names Logical variable with default TRUE
#' @param npoints Number of efficient frontier points with default 10
#' @param plot.efront Logical variable which if TRUE results in a plot of
#' @param values Logical variable for returning efront values with default FALSE
#' @param cexPoints Numerical size parameter for points with default 0.8
#' @param cexText Numerical size parameter for text with default 0.8
#' @return  A plot of the linear efficient frontier or the weights along the
#' efficient frontier.  Optionally output the mean and volatility values
#' of the linear efficient frontier or its weights, or both.
#' @export
#'
#' @examples
#' args(mathEfrontCashRisky)
mathEfrontCashRisky <-
		function(returns, npoints = 10, rf = 0.003, plot.efront = TRUE,
		    stock.names = TRUE, values = FALSE, scalex = 1.1, scaley = 1.1,
		    cexPoints = 0.8, cexText = 0.8)
{
		  V <- var(returns)
		  mu.stocks <- apply(returns, 2, mean)
		  sigma.stocks <- apply(returns, 2, sd)
		  mue <- mu.stocks - rf
		  a <- solve(V, mue)                  # a = Vinv * mue
		  b <- as.numeric(t(mue) %*% a)       # b = muetp * Vinv* mue
		  sr.opt <- sqrt(as.numeric(b))
		  bsr <- sqrt(b)
		  mu.max <- scaley * max(mu.stocks)
		  muvals <- seq(0,mu.max, length.out = npoints)
		  sigmavals <- c(muvals/bsr)
		  muvalse <- seq(0,mu.max, length.out = npoints)
		  muvals <- rf + muvalse
		  if(plot.efront)
		  {
		    x <- sigmavals; y = muvals
		    xlim <- c(0,scalex*max(sigma.stocks, x))
		    ylim <- c(min(mu.stocks),max(muvals))
		    plot(x, y, type = "l", xaxs = "i", lwd = 2, xlim = xlim, ylim = ylim,
		         xlab = "Volatility", ylab = "Mean Return")
		    points(sigma.stocks, mu.stocks, pch = 20, cex = cexPoints)
		    if(stock.names){
		      text(sigma.stocks + 0.02*xlim[2], mu.stocks, names(returns),
		           cex = cexText, adj = 0)
		    }
		    x <- xlim[1] + .05 * (xlim[2] - xlim[1])
		    y <- ylim[2] - .02 * (ylim[2] - ylim[1])
		    text(x,y,paste("SR = ", round(sr.opt,2), sep = ""), pos = 4)
		    y <- y - .07*(ylim[2] - ylim[1])
		    text(x,y,paste("RF = ",round(rf,3),sep = ""), pos = 4)
		  }
		  
		  # Compute cash and risky assets weights
		  wts.risky <- (sum(a)/b)*muvalse
		  wts.cash <- 1 - wts.risky
		  wts.efront <- rbind(muvals, sigmavals, wts.cash, wts.risky)
		  row.names(wts.efront) <- c("MU", "VOL", "Cash" ,"Risky Assets")
		  
		  if(values){
		    return(wts.efront)
		  }
}
