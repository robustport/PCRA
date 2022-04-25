#' Global Minimum Variance (GMV) Portfolio
#' 
#' Compute the weights, mean return and volatility of a GMV portfolio.
#' 
#' 
#' @param returns matrix of asset returns
#' @param digits integer indicating the number of decimal places
#' @examples
#' data(midcap.ts)
#' returns = midcap.ts[, 1:10]
#' mathGmv(returns)
#' @export
mathGmv <-
		function(returns,digits = NULL)
{
	V <- var(returns)
	one <- rep(1, nrow(V))
	z <- solve(V, one)		# Compute z = V.inv * 1
	cc <- t(one) %*% z		# Compute cc = 1.transpose * V.inv * 1
	cc <- as.numeric(cc)	# Convert 1-by-1 matrix to a scalar
	wtsGmv <- z/cc
	mu <- apply(returns, 2, mean)
	a <- t(mu) %*% z
	muGmv <- as.numeric(a/cc)
	volGmv <- 1/cc^0.5
	if(is.null(digits))
	{out = list(wts = wtsGmv,mu = muGmv, vol = volGmv)}
	else
	{out = list(WTS.GMV = wtsGmv, MU.GMV = muGmv, VOL.GMV = volGmv)
		out = lapply(out,round,digits=digits)}
	out
}

#' Global Minimum Variance (GMV) Portfolio
#' 
#' Compute the weights, mean return and volatility of a GMV portfolio.
#' 
#' 
#' @param returns matrix of asset returns
#' @param digits integer indicating the number of decimal places
#' @examples
#' data(midcap.ts)
#' returns = midcap.ts[, 1:10]
#' mathGmv(returns)
#' @export
mathGmvMuCov <- function(muRet,volRet,corrRet, digits = 3) 
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
	wtsGmv = z1/cc
	names(wtsGmv) = c("Stock 1","Stock 2","Stock 3")
	muGmv = a/cc
	varGmv = 1/cc
	volGmv = sqrt(varGmv)
	out = list(volGmv = volGmv, muGmv = muGmv, wtsGmv = wtsGmv)
	if (!is.null(digits)) {out = lapply(out,round, digits = 3)}
	out
}