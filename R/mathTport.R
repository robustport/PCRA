#' @title Tangency Portfolio Weights
#' 
#' @description Computes the portfolio weights of the tangency portfolio, and
#' its mean return and volatility.  The tangency portfolio is defined by the
#' line connecting the zero volatility risk-free rate to its tangency point on
#' the efficient frontier.
#'
#' @param returns A vector or xts object 
#' @param rf The risk-free rate, default 0.005
#' @param digits Number of significant digits default NULL
#'
#' @return Tangency portfoliow weights, mean and volatility
#' @export
#'
#' @examples
#' args(mathTport)
mathTport = function(returns, rf = 0.005, digits = NULL)
{
	mu <- apply(returns, 2, mean)
	C <- var(returns)
	one <- rep(1, nrow(C))
	mu.e <- mu - rf * one		#  Compute excess returns
	z <- solve(C, mu.e)			#  z = C.inv * mu.e
	cc <- t(one) %*% z			# cc = 1.transpose * C.inv. * mu.e
	cc <- as.numeric(cc)		# Convert 1-by-1 matrix to a scalar
	wtsTan <- z/cc
	muTan <- as.numeric(t(mu) %*% wtsTan)
	volTan <- (t(mu.e) %*% z)^0.5/abs(cc)
    if(is.null(digits))
	   {out <- list(wts = wtsTan, mu = muTan, vol = volTan)}
        else
        {out <- list(WTS.TAN= wtsTan, MU.TAN = muTan, VOL.TAN = volTan)
         out <- lapply(out,round, digits=digits)}
    out
}
