#' barplotWts
#'
#' @param wts.efront 
#' @param legend.text 
#' @param col 
#' @param ylab 
#' @param xlab 
#' @param bar.ylim 
#'
#' @return
#' @export
#'
#' @examples
barplotWts <- function(wts.efront, legend.text = NULL,col = NULL,ylab = NULL ,xlab = c("MU","VOL"),bar.ylim = NULL,...)
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

#	layout_mat = cbind(matrix(1,nrow(xpos),ncol(xpos)),matrix(2,nrow(xpos),2))
#	layout(layout_mat)
	
	barplot(xpos,legend.text = legend.text,col = col,ylab = ylab,xlab = xlab.choose,
			ylim = ylim,las=2, cex.names=0.8, bty="n",args.legend=list(
					x='topright',
					xjust =1,
					y=max(apply(xpos,2,max)),
					bty="n"), ...)
	
	barplot(xneg,add = T,col = col,axisnames=FALSE,axes=FALSE)
	abline(h=0)
	
}


