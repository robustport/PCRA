#' Compute Efficient Frontier of Mean Variance Optimization
#' @param returns
#' @param cset
#' @param list.arg
#' @param npoints
#' @author Kirk Li  \email{kirkli@@stat.washington.edu} 
#' @seealso \code{\link{efrontPlot}}
#' @keywords efficient frontier
#' @examples
#' @export

efrontMV = function(returns,cset = NULL, list.arg= NULL, npoints = 10)
{
	
	require(quadprog)
	require(Rglpk)
	require(xts)
	
    if(is.null(cset))
    {
    mu = apply(returns,2,mean)
    mu.min = minmu(returns)
    mu.max = 1.2*max(mu)}     else
    {
      if (any(c("turnover.hobbs","turnover","propcost") %in% cset$clist.names)){ # find max mu without turnover/propcost constraint, since Rglpk doesn't work with turnover/propcost
        if(length(cset$clist.names)>1){
	
      index.mu <- which (cset$clist.names %in% c("turnover.hobbs","turnover","propcost"))
      clist.mu <- c(cset$clist.names[-index.mu],"sum")  
      cset.mu <- combine.cset(clist=clist.mu,returns=returns,list.arg,verbose=F)
      mu.min = minmu(returns,cset.mu)
                   
mu.max = .999*maxmu(returns,cset.mu)
      } else{mu = apply(returns,2,mean)
             mu.min = minmu(returns)
             mu.max = 1.2*max(mu)}
        } 
      else {
      mu.min = minmu(returns)
      mu.max = .999*maxmu(returns,cset)
      }
    }
    p = ncol(returns)
    efront = matrix(rep(0,npoints*(p+2)),ncol = p+2)
    muvals = seq(mu.min,mu.max,length.out = npoints)
    for(i in 1:npoints){
    if(is.null(cset))
    {efront[i,] = mvo(returns,muvals[i],wts.only = F)}     else
    {
      efront[i,] = tryCatch(mvo(returns,muvals[i],cset,wts.only = F),error=function(e)return(NA))
      efront[i,] = as.numeric( efront[i,] )
    }}
    dimnames(efront)[[2]] = c("MU","VOL",dimnames(returns)[[2]])
    if(any(is.na(efront))){
    print("turnover/propcost constraints reduced the max mean return in efficient frontier plot")
    # violate
    efront<-efront[-which(apply(efront,1,function(x)any(is.na(x)))),]}
    efront
}



#' Plot Efficient Frontier of Mean Variance Optimization
#' @param returns
#' @param cset
#' @param mu.min
#' @param mu.max
#' @param rf
#' @param npoints
#' @param wts.xlab
#' @param printout
#' @param bar.ylim
#' @param list.arg
#' @param is.fancy
#' @author Kirk Li  \email{kirkli@@stat.washington.edu} 
#' @seealso \code{\link{efrontPlot}}
#' @keywords efficient frontier
#' @examples
#' @export
efrontPlot = function(returns,cset = NULL,mu.min = NULL, mu.max = NULL, rf = NULL,
                         npoints = 10,wts.plot = T, wts.xlab="VOL",printout = F,bar.ylim = c(0,2),
                      list.arg=NULL, is.fancy=TRUE)
{
    if(is.null(cset))
    {efront = efrontMV(returns,cset = NULL, list.arg=list.arg, npoints)}     else
    {efront = efrontMV(returns,cset,list.arg=list.arg, npoints)}
	if(nrow(efront)<=1) stop("no solution, consider relaxing constraints")
    if(printout) print(efront)
    # Set smart x and y plotting region limits
    sigma = apply(returns,2,sd);mu = apply(returns,2,mean)
    xlim = c(0,max(sigma));ylim = range(mu);ylim[1] = ylim[1]-.05*diff(ylim)
    ylim[1] = min(min(efront[,1]),ylim[1])
    ylim[2] = max(max(efront[,1]),ylim[2])
    if(wts.plot) {par(mfrow = c(1,2), mgp= c(4, 2, 0), mar=c(8,6,4,2)+0.1)}
    plot(efront[,2],efront[,1],type = "l",col = 4,lwd = 2, xlab = "VOL",ylab = "MU", xlim = xlim,
            ylim = ylim, main = "MV Efficient Frontier")
    points(sigma,mu,pch = 20)
    text(sigma,mu, labels = dimnames(returns)[[2]],pos = 1, cex = .7)
    # If risk-free rate is supplied, get Sharpe ratio and plot tangent line
    if(!is.null(rf))
    {sr = (efront[,1]-rf)/efront[,2]
    i.srmax = which.max(sr);srmax = sr[i.srmax]
    abline(rf,srmax,lty = 2)
    # Plot points risk-free rate and tangency portfolio
    points(0,rf,pch = 16)
    points(efront[i.srmax,2],efront[i.srmax,1],pch = 16)
    # Plot point at minimum variance portfolio
    i.minvol = which.min(efront[,2])
    points(efront[i.minvol,2],efront[i.minvol,1],pch = 16)
     
    if(is.fancy){ #display parameter values in ef plot
     para.to.dis <- unique(unlist(sapply(cset$clist.names,function(x){
              clist.names.i <- paste("cset.",x,sep="")
              names(formals(clist.names.i))})))
     para.to.dis <- para.to.dis[para.to.dis %in% c("toc","ptc")] # only display these two parameters
     to.dis <- list.arg[para.to.dis]
     temp1 <- gsub("\\)","",gsub("\\(","",gsub(" c"," ",paste(names(to.dis),to.dis))))
     temp1 <- sub("[[:space:]]"," = ",temp1)
     temp1 <- c(temp1, paste("SRmax =",round(srmax,3),sep = ""),
                  paste("rf =",round(rf,3),sep = ""))
     temp1 <- temp1[unlist(lapply(temp1, function(x){nchar(x)<=50}))]
     legend("topleft",legend=paste(temp1),bty = "n",y.intersp=0.8, xjust=0, x.intersp=-0.5)
     } else{        
    legend("topleft",inset = c(0,.05),paste("SRmax =",round(srmax,3),sep = ""),bty = "n")
    legend("topleft",inset = c(0,.1),paste("rf =",round(rf,3),sep = ""),bty = "n")}
    }
    wts.efront = t(efront); p = nrow(wts.efront)-2
#     MU <- efront[,"MU"]
#     SD <- efront[,"SD"]
    if(wts.plot)
    {
      barplotWts(wts.efront,legend.text = T,col = topo.colors(p),ylab = "WEIGHTS",xlab = wts.xlab, bar.ylim = bar.ylim)
      }
    par(mfrow = c(1,1))
}

