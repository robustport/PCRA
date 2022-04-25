
#' Print metrics from R-forge PerformanceAnalytics that compatible with table.Performance
#' @param NULL
#' @details use \code{table.Performance.pool} to check available metrics. recoded SharpeRatio 
#' @author Kirk Li  \email{kirkli@@stat.washington.edu} 
#' @seealso \code{\link{table.Performance}}, \code{\link{table.Performance.pool.cran}}
#' @keywords table metrics performance measure
#' @examples
#' table.Performance.pool()
#' @export
table.Performance.pool <- function(...){
	c(
			"AdjustedSharpeRatio", 
			"AverageDrawdown", 
			"AverageRecovery", 
			"BernardoLedoitRatio", 
			"BurkeRatio", 
			"CalmarRatio", 
			"CVaR", 
			"DownsideDeviation", 
			"DownsideFrequency", 
			"DownsidePotential", 
			"DRatio", 
			"DrawdownDeviation", 
			"ES", 
			"ETL",
			"etl",
			"Frequency", 
			"HurstIndex", 
#			"KellyRatio", something is wrong with this metric, will fix later
			"kurtosis", 
			"lpm", 
			"MartinRatio", 
			"maxDrawdown", 
			"mean.geometric", 
			"mean.LCL", 
			"mean.stderr", 
			"mean.UCL", 
			"MeanAbsoluteDeviation", 
			"Omega", 
			"OmegaSharpeRatio", 
			"PainIndex", 
			"PainRatio", 
			"Return.annualized", 
			"Return.cumulative", 
			"sd.annualized", 
			"sd.multiperiod", 
			"SemiDeviation", 
			"SemiVariance", 
			"SFM.beta",
#			"SharpeRatio",
			"sharpeRatio",
			"SharpeRatio.annualized", 
			"skewness", 
			"SkewnessKurtosisRatio", 
			"SmoothingIndex", 
			"SortinoRatio", 
			"StdDev", 
			"StdDev.annualized", 
			"SterlingRatio", 
			"starrRatio",
			"UlcerIndex", 
			"UpsideFrequency", 
			"UpsidePotentialRatio", 
			"UpsideRisk", 
			"VaR", 
			"VolatilitySkewness")
}

.beta <- function (xRa, xRb, subset) 
{
	if (missing(subset)) 
		subset <- TRUE
	if (NCOL(xRa) != 1L || NCOL(xRb) != 1L || NCOL(subset) != 
			1L) 
		stop("all arguments must have only one column")
	merged <- as.data.frame(na.omit(cbind(xRa, xRb, subset)))
	if (NROW(merged) == 0) 
		return(NA)
	colnames(merged) <- c("xRa", "xRb", "subset")
	merged$subset <- as.logical(merged$subset)
	model.lm = lm(xRa ~ xRb, data = merged, subset = merged$subset)
	beta = coef(model.lm)[[2]]
	beta
}

SFM.beta <- function (R, Rb.index = 1, Rf = 0){
	Ra = checkData(R)
	Rb = R[,Rb.index]
	Rb = checkData(Rb)
	if (!is.null(dim(Rf))) 
		Rf = checkData(Rf)
	Ra.ncols = NCOL(Ra)
	Rb.ncols = NCOL(Rb)
	xRa = Return.excess(Ra, Rf)
	xRb = Return.excess(Rb, Rf)
	pairs = expand.grid(1:Ra.ncols, 1:Rb.ncols)
	result = apply(pairs, 1, FUN = function(n, xRa, xRb) .beta(xRa[, 
								n[1]], xRb[, n[2]]), xRa = xRa, xRb = xRb)
	if (length(result) == 1) 
		return(result)
	else {
		dim(result) = c(Ra.ncols, Rb.ncols)
		colnames(result) = paste("Beta:", colnames(Rb))
		rownames(result) = colnames(Ra)
		return(t(result))
	}
}


