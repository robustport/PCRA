#source("constraint.sets.r")
#source("mvo.constrained.r")
#source("efront.constrained.r")
#source("barplot.wts.r")
#library(xts)
#library(corpcor)
#load("crsp.short.Rdata")
## number of stocks
#n.stocks <- 5
#names(midcap.ts)
#names(smallcap.ts)
#names(largecap.ts)
#returns.ts = midcap.ts[,1:n.stocks]
#returns = coredata(midcap.ts[,1:n.stocks])
#sum=1
#
#
#
#
#mu.target=NULL 
#w.initial=rep(1/n.stocks,n.stocks) 
#toc=0.3
#upper=rep(0.5,n.stocks)
#lower=rep(-0.5,n.stocks)
#group=c(sample(1:2,n.stocks,replace=T))
#upper.group=c(0.5,0.5)
#lower.group=c(-0.5,-0.5)
#ptc=0.01
#digits=4
#wts.only=T
#mu.min = NULL 
#mu.max = NULL 
#rf = .003
#npoints = 20
#wts.plot = T 
#printout = F
#bar.ylim = c(-1,4)
#
## constraint list
#"sum" 
#"lo"
#"box"
#"groups"
#"mu.target"
#"turnover"
#"turnover"
#"propcost"
#
#
####################################################################################################
## scenario 10
#clist <- c("box","propcost")
#list.arg <- list(upper=upper,
#                 lower=lower,
#                 ptc=ptc,
#                 w.initial=w.initial)
#print(list.arg)
#cset <- NULL
#cset <-combine.cset(clist=clist,returns=returns,list.arg)
#gmv(returns, cset=cset, wts.only=T,digits=4)
##wt plot using MU on horizontal 
#efrontPlot(returns, cset, rf = .003, npoints = 20,wts.plot = T,bar.ylim = c(-1,4),list.arg=list.arg, wts.xlab="MU")
##wt plot using VOL on horizontal 
#efrontPlot(returns, cset, rf = .003, npoints = 20,wts.plot = T,bar.ylim = c(-1,4),list.arg=list.arg, wts.xlab="VOL")
#mtext(paste(clist,collapse="_"),side=1,line=3)
#
## sum constraint are not combinable with propcost constraint
#clist <- c("sum","box","propcost")
#list.arg <- list( sum=sum,
#                  upper=upper,
#                  lower=lower,
#                  ptc=ptc,
#                  w.initial=w.initial)
#print(list.arg)
#cset <- NULL
#cset <-combine.cset(clist=clist,returns=returns,list.arg)
## Expected error msg: Error in propcost.modify(cset.i) : sum constraint are not combinable with propcost constraint
#
## scenario 11
#clist <- c("sum","box","turnover")
#list.arg <- list(sum=sum,
#                 upper=upper,
#                 lower=lower,
#                 toc=toc,
#                 w.initial=w.initial)
#print(list.arg)
#cset <- NULL
#cset <-combine.cset(clist=clist,returns=returns,list.arg)
#gmv(returns, cset=cset, wts.only=T,digits=4)
#efrontPlot(returns, cset, rf = .003, npoints = 20,wts.plot = T,bar.ylim = c(-1,4),list.arg=list.arg)
#mtext(paste(clist,collapse="_"),side=1,line=3)
#
#
#clist <- c("sum","box","turnover")
#list.arg <- list(sum=sum,
#		upper=upper,
#		lower=lower,
#		toc=toc,
#		w.initial=w.initial)
#print(list.arg)
#cset <- NULL
#cset <-combine.cset(clist=clist,returns=returns,list.arg)
#gmv(returns, cset=cset, wts.only=T,digits=4)
#efrontPlot(returns, cset, rf = .003, npoints = 20,wts.plot = T,bar.ylim = c(-1,4),list.arg=list.arg)
#mtext(paste(clist,collapse="_"),side=1,line=3)
#
#
#####################################################################################################
#
#
#list.arg <- list(sum=sum,
#                 mu.target=mu.target, 
#                 group=group, 
#                 upper.group=upper.group,
#                 lower.group=lower.group, 
#                 upper=upper, 
#                 lower=lower, 
#                 toc=toc, 
#                 w.initial=w.initial,
#                 ptc=ptc)
#
## semario 0
#cset=NULL # gmv
#efrontPlot(returns, cset=cset, rf = .003, npoints = 20,wts.plot = T,bar.ylim = c(-1,4),list.arg=list.arg)
#mtext(paste(clist,collapse="_"),side=1,line=3)
#
## scenario 1
#clist <- c("sum")
#cset <- NULL
#cset <-combine.cset(clist=clist,returns=returns,list.arg=list.arg)
#gmv(returns, cset=cset, wts.only=T,digits=4)
#efrontPlot(returns, cset, rf = .003, npoints = 20,wts.plot = T,bar.ylim = c(-1,4),list.arg=list.arg)
#mtext(paste(clist,collapse="_"),side=1,line=3)
#
## scenario 2
#clist <- c("sum","lo")
#cset <- NULL
#cset <-combine.cset(clist=clist,returns=returns,list.arg=list.arg)
#gmv(returns, cset=cset, wts.only=T,digits=4)
#efrontPlot(returns, cset, rf = .003, npoints = 20,wts.plot = T,bar.ylim = c(-1,4),list.arg=list.arg)
#mtext(paste(clist,collapse="_"),side=1,line=3)
#
#
## scenario 3
#clist <- c("sum","lo","box")
#cset <- NULL
#cset <-combine.cset(clist=clist,returns=returns,list.arg=list.arg)
#gmv(returns, cset=cset, wts.only=T,digits=4)
#efrontPlot(returns, cset, rf = .003, npoints = 20,wts.plot = T,bar.ylim = c(-1,4),list.arg=list.arg)
#mtext(paste(clist,collapse="_"),side=1,line=3)
#
## scenario 4
#clist <- c("sum","lo","groups")
#cset <- NULL
#cset <-combine.cset(clist=clist,returns=returns,list.arg=list.arg)
#gmv(returns, cset=cset, wts.only=T,digits=4)
#efrontPlot(returns, cset, rf = .003, npoints = 20,wts.plot = T,bar.ylim = c(-1,4),list.arg=list.arg)
#mtext(paste(clist,collapse="_"),side=1,line=3)
#
## scenario 5
#clist <- c("sum","lo","mu.target")
#cset <-combine.cset(clist=clist,returns=returns,list.arg=list.arg)
#gmv(returns, cset=cset, wts.only=T,digits=4)
#
#
## scenario 6
#clist <- c("sum","lo","box","groups")
#cset <- NULL
#cset <-combine.cset(clist=clist,returns=returns,list.arg=list.arg)
#gmv(returns, cset=cset, wts.only=T,digits=4)
#efrontPlot(returns, cset, rf = .003, npoints = 20,wts.plot = T,bar.ylim = c(-1,4),list.arg=list.arg)
#mtext(paste(clist,collapse="_"),side=1,line=3)
#
#
## list.arg$toc <- 0.2
#
## scenario 7
#clist <- c("sum","lo","turnover")
#cset <- NULL
#cset <-combine.cset(clist=clist,returns=returns,list.arg=list.arg)
#gmv(returns, cset=cset, wts.only=T,digits=4)
#efrontPlot(returns, cset, rf = .003, npoints = 20,wts.plot = T,bar.ylim = c(-1,4),list.arg=list.arg)
#mtext(paste(clist,collapse="_"),side=1,line=3)
## 1+4+1+4+4+2+2+1+4+4+4  sum+lo+mu.target+box+box+group+group+turnover+w.sell+w.buy+w.initial
#
## scenario 8
#clist <- c("lo","box","groups")
#cset <- NULL
#cset <-combine.cset(clist=clist,returns=returns,list.arg)
#gmv(returns, cset=cset, wts.only=T,digits=4)
#efrontPlot(returns, cset, rf = .003, npoints = 20,wts.plot = T,bar.ylim = c(-1,4),list.arg=list.arg)
#mtext(paste(clist,collapse="_"),side=1,line=3)
#
## scenario 9
#clist <- c("lo","box","groups","propcost")
#cset <- NULL
#cset <-combine.cset(clist=clist,returns=returns,list.arg)
#gmv(returns, cset=cset, wts.only=T,digits=4)
#
#efrontPlot(returns, cset, rf = .003, npoints = 20,wts.plot = T,bar.ylim = c(-1,4),list.arg=list.arg)
#mtext(paste(clist,collapse="_"),side=1,line=3)
#
#
#
#
