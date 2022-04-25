# TODO: Add comment
# 
# Author: kirkli
###############################################################################
#' efrontplot.shiny
#'
#' @param files 
#' @param group 
#' @param list.cset 
#' @param list.arg 
#'
#' @return
#' @export
#'
#' @examples
efrontplot.shiny <- function(files, group,list.cset, list.arg){
#	load("crsp.short.Rdata")
	stock.use = names(files)
	# TODO: checking function
	returns.ts = files
	returns = coredata(returns.ts)
#   print(returns)
	cat("number of stocks: ",ncol(returns),"\n")  
	
	constraint.names.list <- c(  "sum",
			"lo",
			"box",
			"groups",
			"turnover",
			"propcost")
	constraint.use <- constraint.names.list[unlist(list.cset)]
	cat("use constraints: ", constraint.use,"\n")
	if(is.null(group)){
	group = rep(1,length(stock.use))
	list.arg$group <- group
	} else {list.arg$group <- group}
	list.arg$w.initial <- rep(1/length(stock.use),length(stock.use))
	cset <-combine.cset(clist=constraint.use,returns=returns,list.arg)
	efrontPlot(returns, cset, list.arg=list.arg, rf = .003, npoints = 50,wts.plot = T,bar.ylim = c(-1,4))
	mtext(paste(constraint.use,collapse="_"),side=1,line=3)
#    return(list(cset=cset,returns=returns))
}
