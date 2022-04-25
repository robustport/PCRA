#' mathTport
#'
#' @param returns 
#' @param rf 
#' @param digits 
#'
#' @return
#' @export
#'
#' @examples
mathTport = function(returns, rf = 0.01,digits = NULL)
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
	   {out = list(wts = wtsTan, mu = muTan, vol = volTan)}
        else
        {out = list(WTS.TAN= wtsTan, MU.TAN = muTan, VOL.GMV = volTan)
         out = lapply(out,round,digits=digits)}
    out
}
