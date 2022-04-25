# TODO: Add comment
# 
# Author: cocok
###############################################################################
library(xtable)
library(data.table)

portRetfun <- function(retRate){
  
  ret	=	retRate + 1
  eval(parse(text=paste0(paste0("r",1:length(retRate)),"=",retRate)))
  
  avgRet	=	mean(retRate)
  cumRet	=	prod(ret) - 1
  varRet	=	var(retRate)	
  lnRet	=	log(ret) 
  eval(parse(text=paste0(paste0("lnr",1:length(retRate)),"=",lnRet)))
  
  
  avgLnRet	=	mean(lnRet)
  pedG	=	prod(ret)^(1/length(retRate)) - 1
  annualG	=	(1+pedG)^4-1
  gappGood	=	avgRet - 0.5 * varRet/(1 + avgRet)^2
  gappSimple	=	avgRet - varRet/2
  percErrGood	=	(gappGood - pedG)/pedG
  percErrSimple	=	(gappSimple - pedG)/pedG
  #	cumRet	=	exp(length(return)*avgLnRet)-1
  eval(parse(text=paste0('return(list(',
						  paste0(paste0("r",1:length(retRate)),"=",paste0("r",1:length(retRate)),collapse=","),',',
						  paste0(paste0("lnr",1:length(retRate)),"=",paste0("lnr",1:length(retRate)),collapse=","),',','
    avgRet=avgRet,
    cumRet=cumRet,
    varRet=varRet,
    avgLnRet=avgLnRet,
    pedG=pedG,
    annualG=annualG,
    gappGood=gappGood,
    gappSimple=gappSimple,
    percErrGood=percErrGood,
    percErrSimple=percErrSimple))')))
  
}

retRate1=c(0.1, 0.2, 0.3)
retRate2=c(0.1, 0.7, -0.2)
retRate3=c(0.1, 1.4, -0.9)
bmk=c(0.05, 0.15, 0.25)
#portRetfun(bmk)


output = data.frame(rbind(portRetfun(retRate1),portRetfun(retRate2),portRetfun(retRate3),portRetfun(bmk)
		))

row.names(output)=c(1,2,3,"B")

output = data.table(output)

output1 = output[,lapply(.SD,function(x)paste0(round(as.numeric(x)*100,2),'%')),.SDcols=c('r1','r2','r3','lnr1','lnr2','lnr3','avgRet','avgLnRet','pedG','gappGood','gappSimple')]

output1$varRet = output$varRet

output1 = output1[,c('r1','r2','r3','lnr1','lnr2','lnr3','avgRet','avgLnRet','pedG','varRet','gappGood','gappSimple'),with=F]

x = xtable(output1,
		caption = "Properties of Portfolios and Their Returns\\label{tab:PPR}",
		label = "tab:PPR")
#print(x, caption.placement = "top",include.rownames = F)












