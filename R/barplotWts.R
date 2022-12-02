#' Barplot Weights
#' 
#' Uses the R barplot() function to make a barplot of efficient frontier weights
#' See the manual page for barplot()
#'
#' @param wts.efront  Matrix of weights along the efficient frontier
#' @param legend.text  Vector of text for the legend 
#' @param col  Vector of colors for the bars
#' @param ylab  A label for the y axis
#' @param xlab  A label for the x axis
#' @param bar.ylim  Limits of the y axis for barplot
#' @param ...  additional parameters from barplot 
#' @return  A barplot of efficient frontier weights
#' @export
#'
#' @examples
#' args(barplotWts)
barplotWts <- function(wts.efront, legend.text = NULL,col = NULL,ylab = NULL ,
                       xlab = c("MU","VOL"),bar.ylim = NULL,...)
{
	xlab.choose <- match.arg(xlab)
#   cat(xlab.choose,"\n")
	xlab <- wts.efront[xlab.choose,]
	xlab <- round(xlab,4)
	xlab <- sprintf("%.4f", xlab)
	x <- wts.efront[-c(1,2),]
	n = ncol(x); p = nrow(x)
	xpos = (abs(x)+x)/2
	xneg = (x-abs(x))/2
	if(is.null(bar.ylim))
	{ymax <- max(colSums(xpos,na.rm=T))
		ymin <- min(colSums(xneg,na.rm=T))
		ylim = c(ymin*1.2,ymax*1.2)}   else {ylim = bar.ylim}
	colnames(xpos) <- xlab

	barplot(xpos,legend.text = legend.text,col = col,ylab = ylab,xlab = xlab.choose,    xlim=c(0, ncol(xpos)*1.5),
			ylim = ylim,las=2, cex.names=0.8, bty="n",args.legend=list(
			  x=ncol(xpos) *1.5,
			  y=max(colSums(xpos)),
			  bty = "n"
			),...)
	
	barplot(xneg,add = T,col = col,axisnames=FALSE,axes=FALSE)
	abline(h=0)
}




