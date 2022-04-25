#' Plot a QQ chart that adapts to different distribution types
#' modified from \code{\link{chart.QQPlot}} 
#' 
#' A Quantile-Quantile (QQ) plot is a scatter plot designed to compare the data
#' to the theoretical distributions to visually determine if the observations
#' are likely to have come from a known population. The empirical quantiles are
#' plotted to the y-axis, and the x-axis contains the values of the theorical
#' model.  A 45-degree reference line is also plotted. If the empirical data
#' come from the population with the choosen distribution, the points should
#' fall approximately along this reference line. The larger the departure from
#' the reference line, the greater the evidence that the data set have come
#' from a population with a different distribution.
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param distribution root name of comparison distribution - e.g., 'norm' for
#' the normal distribution; 't' for the t-distribution. See examples for other
#' ideas.
#' @param xlab set the x-axis label, as in \code{\link{plot}}
#' @param ylab set the y-axis label, as in \code{\link{plot}}
#' @param xaxis if true, draws the x axis
#' @param yaxis if true, draws the y axis
#' @param ylim set the y-axis limits, same as in \code{\link{plot}}
#' @param main set the chart title, same as in \code{plot}
#' @param las set the direction of axis labels, same as in \code{plot}
#' @param envelope confidence level for point-wise confidence envelope, or
#' FALSE for no envelope.
#' @param labels vector of point labels for interactive point identification,
#' or FALSE for no labels.
#' @param col color for points and lines; the default is the \emph{second}
#' entry in the current color palette (see 'palette' and 'par').
#' @param lwd set the line width, as in \code{\link{plot}}
#' @param pch symbols to use, see also \code{\link{plot}}
#' @param cex symbols to use, see also \code{\link{plot}}
#' @param line 'quartiles' to pass a line through the quartile-pairs, or
#' 'robust' for a robust-regression line; the latter uses the 'rlm' function
#' in the 'MASS' package. Specifying 'line = "none"' suppresses the line.
#' @param element.color provides the color for drawing chart elements, such as
#' the box lines, axis lines, etc. Default is "darkgray"
#' @param cex.legend The magnification to be used for sizing the legend
#' relative to the current setting of 'cex'
#' @param cex.axis The magnification to be used for axis annotation relative to
#' the current setting of 'cex'
#' @param cex.lab The magnification to be used for x- and y-axis labels
#' relative to the current setting of 'cex'
#' @param cex.main The magnification to be used for the main title relative to
#' the current setting of 'cex'.
#' @param \dots any other passthru parameters to the distribution function
#' @author updated by Kirk Li \email{kirkli@@stat.washington.edu}
#' @seealso 
#' \code{\link[stats]{qqplot}} \cr 
#' \code{\link[car]{qq.plot}} \cr
#' \code{\link{plot}} \cr
#' CRAN package \code{\link[nor1mix]{norMixFit}} for mixture normal distribution 
#' @references main code forked/borrowed/ported from the excellent: \cr Fox,
#' John (2007) \emph{car: Companion to Applied Regression} \cr
#' \url{http://www.r-project.org},
#' \url{http://socserv.socsci.mcmaster.ca/jfox/}
#' @references \code{\link{nor1mix}}
#' @keywords normal mixture model, QQplot
#' @examples
#' 
#' library(MASS) 
#' library(PerformanceAnalytics)
#' data(managers)
#' x = checkData(managers[,2, drop = FALSE], na.rm = TRUE, method = "vector")
#' 
#' # Panel 1: Normal distribution
#' chart.QQPlot2(x, main = "Normal Distribution",
#' 		line=c("quartiles"), distribution = 'norm',  
#' 		envelope=0.95)
#' 
#' 
#' 
#' # Panel 2: Mixture Normal distribution
#' library(nor1mix)
#' obj = norMixEM(x,m=2)
#' chart.QQPlot2(x, main = "Normal Mixture Distribution",
#' 		line=c("quartiles"), distribution = 'norMix',  distributionParameter='obj',
#' 		envelope=0.95)
#' 
#' 
#' retLW = largecapW['2006-01-31/',12:20]
#' ret = retLW[,"MO"]
#' ret = as.numeric(coredata(ret))
#' n = length(ret)
#' 
#' 
#' # Panel 3: Symmetric t distribution
#' fit.tSN = st.mple(as.matrix(rep(1,n)),ret,symmetr = TRUE)
#' names(fit.tSN$dp) = c("location","scale","dof")
#' round(fit.tSN$dp,3)
#' chart.QQPlot2(ret, main = "MO Symmetric t-Distribution QQPlot",
#' 		xlab = "quantilesSymmetricTdistEst",line = c("quartiles"),
#' 		envelope = .95, distribution = 't', distributionParameter='df=fit.tSN$dp[3]',pch = 20)
#' 
#' 
#' # Panel 4: Skewed t distribution
#' fit.st = st.mple(as.matrix(rep(1,n)),ret)
#' # fit.st = st.mple(y=ret)  Produces same result as line above
#' names(fit.st$dp) = c("location","scale","skew","dof")
#' round(fit.st$dp,3)
#' chart.QQPlot2(ret, main = "MO Returns Skewed t-Distribution QQPlot",
#' 		xlab = "quantilesSkewedTdistEst",line = c("quartiles"),
#' 		envelope = .95, distribution = 'st',distributionParameter = 'xi = fit.st$dp[1],
#' 				omega = fit.st$dp[2],alpha = fit.st$dp[3],
#' 				nu=fit.st$dp[4]',pch = 20)
#' 
#' 
#' @export 

chart.QQPlot2 <- 
function (R, distribution = "norm", ylab = NULL, xlab = paste(distribution, 
				"Quantiles"), main = NULL, las = par("las"), envelope = FALSE, 
		labels = FALSE, col = c(1, 4), lwd = 2, pch = 1, cex = 1, 
		line = c("quartiles", "robust", "none"), element.color = "darkgray", 
		cex.axis = 0.8, cex.legend = 0.8, cex.lab = 1, cex.main = 1, 
		xaxis = TRUE, yaxis = TRUE, ylim = NULL, distributionParameter=NULL,...) 
{
	x = checkData(R, method = "vector", na.rm = TRUE)
	if (is.null(main)) {
		if (!is.null(colnames(R)[1])) 
			main = colnames(R)[1]
		else main = "QQ Plot"
	}
	if (is.null(ylab)) 
		ylab = "Empirical Quantiles"
	result <- NULL
	line <- match.arg(line)
	good <- !is.na(x)
	ord <- order(x[good])
	ord.x <- x[good][ord]
	
	q.function <- eval(parse(text = paste("q", distribution, 
							sep = "")))
	d.function <- eval(parse(text = paste("d", distribution, 
							sep = "")))
	n <- length(ord.x)
	P <- ppoints(n)
	
	eval(parse(text=paste("	z <- q.function(P,", distributionParameter,",...)")))
  
	plot(z, ord.x, xlab = xlab, ylab = ylab, main = main, las = las, 
			col = col[1], pch = pch, cex = cex, cex.main = cex.main, 
			cex.lab = cex.lab, axes = FALSE, ylim = ylim, ...)
	if (line == "quartiles") {
		Q.x <- quantile(ord.x, c(0.25, 0.75))
		
		eval(parse(text=paste("	Q.z <- q.function(c(0.25, 0.75),", distributionParameter,",...)")))
#		Q.z <- q.function(c(0.25, 0.75), ...)
		b <- (Q.x[2] - Q.x[1])/(Q.z[2] - Q.z[1])
		a <- Q.x[1] - b * Q.z[1]
		abline(a, b, col = col[2], lwd = lwd)
	}
	if (line == "robust") {
		stopifnot(requireNamespace("MASS", quietly = TRUE))
		coef <- coefficients(MASS::rlm(ord.x ~ z))
		a <- coef[1]
		b <- coef[2]
		abline(a, b, col = col[2])
	}
	if (line != "none" & envelope != FALSE) {
		zz <- qnorm(1 - (1 - envelope)/2)
		eval(parse(text=paste("	SE <- (b/d.function(z,", distributionParameter,",...))* sqrt(P * (1 - P)/n)")))
		
#		SE <- (b/d.function(z, ...)) * sqrt(P * (1 - P)/n)
		fit.value <- a + b * z
		upper <- fit.value + zz * SE
		lower <- fit.value - zz * SE
		lines(z, upper, lty = 2, lwd = lwd/2, col = col[2])
		lines(z, lower, lty = 2, lwd = lwd/2, col = col[2])
	}
	if (labels[1] == TRUE & length(labels) == 1) 
		labels <- seq(along = z)
	if (labels[1] != FALSE) {
		selected <- identify(z, ord.x, labels[good][ord])
		result <- seq(along = x)[good][ord][selected]
	}
	if (is.null(result)) 
		invisible(result)
	else sort(result)
	if (xaxis) 
		axis(1, cex.axis = cex.axis, col = element.color)
	if (yaxis) 
		axis(2, cex.axis = cex.axis, col = element.color)
	box(col = element.color)
}

#chart.QQPlot.norMix <-
#		function(R, distribution="norm", ylab=NULL,
#				xlab=paste(distribution, "Quantiles"), main=NULL, las=par("las"),
#				envelope=FALSE, labels=FALSE, col=c(1,4), lwd=2, pch=1, cex=1,
#				line=c("quartiles", "robust", "none"), element.color = "darkgray", 
#				cex.axis = 0.8, cex.legend = 0.8, cex.lab = 1, cex.main = 1, xaxis=TRUE, yaxis=TRUE, ylim=NULL, ...)
#{ 
#	
#	x = checkData(R, method = "vector", na.rm = TRUE)
#	#     n = length(x)
#	
#	if(is.null(main)){ 
#		if(!is.null(colnames(R)[1])) 
#			main=colnames(R)[1]
#		else
#			main = "QQ Plot"
#	}
#	if(is.null(ylab)) ylab = "Empirical Quantiles"
#	# the core of this function is taken from John Fox's qq.plot, which is part of the car package
#	result <- NULL
#	line <- match.arg(line)
#	good <- !is.na(x)
#	ord <- order(x[good])
#	ord.x <- x[good][ord]
#	n <- length(ord.x)
#	P <- ppoints(n)
#	
#	if(distribution=="mixnormal")
#	{
#		qmixnormal <- function(q, ...){
#			# norMix distribution
#			para=list(...)$para
#			
#			if(!is.list(para))stop(" 'para' must be a 'list' object")
#			
#			if(is.null(para$m)|is.na(para$m)) 
#				stop("The number of component must be specified in 'para$m'")
#			
#			require(nor1mix)
#			out = norMixEM(x, para$m, trace=0)
#			
#			if (length(q)!=2){
#				# only print once
#				print("fitted model:")
#				print(out[1:para$m,],digits=3)
#			}
#			if(is.null(para$mu) | is.null(para$sig2)) 
#			# using fitted distribution
#			{
#				if (length(q)!=2)
#					print("using fitted model as theoretical distribution")
#				obj <- out
#			}	else{
#				# using specified distribution
#				if(length(para$mu)!=para$m | length(para$sig2)!=para$m)
#					stop("the number of components mismatch with parameter inputs")
#				
#				obj <- norMix(mu = para$mu, sig2 = para$sig2, w = para$w) 
#			}
#			qnorMix(q,obj)
#		}
#		
#		dmixnormal<- function(p, ...){
#			# norMix distribution
#			para=list(...)$para
#			if(!is.list(para))stop(" 'para' must be a 'list' object")
#			if(is.null(para$m)|is.na(para$m)) 
#				stop("The number of component must be specified in 'para$m'")
#			
#			require(nor1mix)
#			
#			out = norMixEM(x, para$m, trace=0)
#			
#			if(is.null(para$mu) | is.null(para$sig2)) 
#			# using fitted distribution
#			{
#				obj <- out
#			}	else{
#				# using specified distribution
#				if(length(para$mu) != para$m | length(para$sig2) != para$m)
#					stop("the number of components mismatch with parameter inputs")
#				
#				obj <- norMix(mu = para$mu, sig2 = para$sig2, w = para$w) 
#			}
#			dnorMix(p,obj)
#		}
#		
#	}  
#	
#	
#	q.function <- eval(parse(text=paste("q",distribution, sep="")))
#	d.function <- eval(parse(text=paste("d",distribution, sep="")))
#	
#	z <- q.function(P,...)
#	
#	plot(z, ord.x, xlab=xlab, ylab=ylab, main=main, las=las, col=col[1], pch=pch,
#			cex=cex, cex.main = cex.main, cex.lab = cex.lab, axes=FALSE, ylim=ylim)
#	
#	if (line=="quartiles"){
#		Q.x<-quantile(ord.x, c(.25,.75))
#		Q.z<-q.function(c(.25,.75), ...)
#		b<-(Q.x[2]-Q.x[1])/(Q.z[2]-Q.z[1])
#		a<-Q.x[1]-b*Q.z[1]
#		abline(a, b, col=col[2], lwd=lwd)
#	}
#	if (line=="robust"){
#		stopifnot("package:MASS" %in% search() || require("MASS",quietly=TRUE))
#		coef<-coefficients(rlm(ord.x~z))
#		a<-coef[1]
#		b<-coef[2]
#		abline(a,b, col=col[2])
#	}
#	if (line != 'none' & envelope != FALSE) {
#		zz<-qnorm(1-(1-envelope)/2)
#		SE<-(b/d.function(z,...))*sqrt(P*(1-P)/n)
#		fit.value<-a+b*z
#		upper<-fit.value+zz*SE
#		lower<-fit.value-zz*SE
#		lines(z, upper, lty=2, lwd=lwd/2, col=col[2])
#		lines(z, lower, lty=2, lwd=lwd/2, col=col[2])
#	}
#	if (labels[1]==TRUE & length(labels)==1) labels<-seq(along=z)
#	if (labels[1] != FALSE) {
#		selected<-identify(z, ord.x, labels[good][ord])
#		result <- seq(along=x)[good][ord][selected]
#	}
#	if (is.null(result)) invisible(result) else sort(result)
#	
#	if(xaxis)
#		axis(1, cex.axis = cex.axis, col = element.color)
#	if(yaxis)
#		axis(2, cex.axis = cex.axis, col = element.color)
#	
#	box(col=element.color)
#	
#}
