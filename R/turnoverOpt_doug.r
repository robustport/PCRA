#' Specify turnover constraints using a version derived by Doug Martin
#' @param returns, stock returns in xts format.
#' @param mu.target, default is NULL, specify the target mean return
#' @param wts.inital, the initial weight
#' @param toc, the turnover cost
#' @param long.only, default is TRUE, specify if long only constraint is required
#' @param printDandA, default is FALSE, specify if print the D and A matrix
#' @details use \code{table.Performance.pool} to check available metrics. recoded SharpeRatio 
#' @author Doug Martin, Kirk Li  \email{kirkli@@stat.washington.edu} 
#' @seealso \code{\link{turnoverOpt}}}
#' @keywords turnover, constraints
#' @examples
#' @export



TurnoverOpt_doug <- function(returns,mu.target = NULL,wts.initial,toc,
                        long.only = TRUE,printDandA = FALSE){
  nassets <- ncol(returns)
  nobs = nrow(returns)
  returns0 = matrix(rep(0,nassets*nobs),ncol = nassets)
  # Use 3 sets of variables wts, wtsBuy, wtsSell
  # Construct  returns matrix to yield 3Nx3N covariance matrix
  returns <- cbind(returns,returns0,returns0)
  # Compute 3Nx3N block diag. cov.mat with 0's except in first NxN block
  cov.mat <- cov(returns)
  Dmat <- 2*cov.mat
  # Compute "nearest" positive definite covariance matrix (Higham,1988)
  Dmat <- make.positive.definite(Dmat)
  dvec <- rep(0,3*nassets) #no linear part in this problem
  
  # Build components of constraint matrix A
  cSum <- c(rep(1,nassets),rep(0,2*nassets))
  cWts = rbind(diag(1,nassets),diag(-1,nassets),diag(1,nassets))
  dimnames(cWts)[[2]] = paste("cWts",1:nassets,sep = "")
  cTurnover <- c(rep(0,nassets),rep(-1,nassets),rep(-1,nassets))
  cWtsBuy <- rbind(diag(0,nassets),diag(1,nassets),diag(0,nassets))
  dimnames(cWtsBuy)[[2]] = paste("cWtsBuy",1:nassets,sep = "")
  cWtsSell = rbind(diag(0,nassets),diag(0,nassets),diag(1,nassets))
  dimnames(cWtsSell)[[2]] = paste("cWtsSell",1:nassets,sep = "")
  
  muVec <- apply(returns,2,mean)
  
  
  if(!is.null(mu.target)){
    # Constraint A matrix
    cMean = muVec
    # A matrix with mean return constraint
    Amat <- cbind(cSum, cMean, cWts,cTurnover,
                cWtsBuy, cWtsSell)
    # Constraints b vector
    bvec <- c(1,mu.target, wts.initial, -toc, rep(0,2*nassets))
    n.eq <- 2+nassets  # First n.eq constraints are equalities
  } else {
    # GMV portfolio - no mean return constraint
    Amat <- cbind(cSum, cWts,cTurnover, 
                cWtsBuy, cWtsSell)
    bvec <- c(1,wts.initial,-toc,rep(0,2*nassets))
    n.eq <- 1 + nassets # First n.eq constraints are equalities
  }
  
  #optional long only constraint
  if(long.only == TRUE){
    if ( length(wts.initial[wts.initial<0]) > 0 ){
      stop("Long-Only specified but some initial weights are negative")
    }
    cLongOnly <- rbind(diag(nassets),diag(0,nassets),diag(0,nassets))
    dimnames(cLongOnly)[[2]] = paste("longOnly",1:nassets,sep = "")
    Amat <- cbind(Amat, cLongOnly)
    bvec <- c(bvec,rep(0,nassets))
  }
if(printDandA) {print(Dmat);print(round(t(Amat),3))}
 
  solution <- solve.QP(Dmat,dvec,Amat,bvec,meq=n.eq)
  
  port.var <- solution$value
  wts = solution$solution[1:nassets]
  wts.buy <- solution$solution[(nassets+1):(2*nassets)]
  wts.sell <- solution$solution[(2*nassets+1):(3*nassets)]
  turnover <- sum(wts.buy,wts.sell)
  port.mu <- wts%*%(muVec[1:nassets])
  out = list(wts = wts, wts.buy = wts.buy,wts.sell=wts.sell,
       turnover = turnover,
       port.var=port.var,port.mu=port.mu)
  lapply(out,round,3)
}

#TurnoverOpt_doug(returns, mu.target = NULL, wts.initial = rep(.1,10),toc = .6)



