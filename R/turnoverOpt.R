#' turnoverOpt
#'
#' @param returns 
#' @param mu.target 
#' @param wts.initial 
#' @param toc 
#' @param long.only 
#' @param printD 
#' @param printA 
#' @param printout 
#'
#' @return
#' @export
#'
#' @examples
turnoverOpt <- function(returns,mu.target = NULL,wts.initial,toc,
                long.only = TRUE,printD = F,printA = F,printout = T){
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
  DmatPd <- make.positive.definite(Dmat)
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
  
  if(!is.null(mu.target)){
    # Constraint A matrix
    muVec <- apply(returns,2,mean)
    cMean = muVec
    # A matrix with mean return constraint
    Amat <- cbind(cSum, cMean, cWts,cTurnover,
                cWtsBuy, cWtsSell)
    # Constraints b vector
    bvec <- c(1,mu.target,wts.initial,-toc,rep(0,2*nassets))
    n.eq <- 2+nassets  # First n.eq constraints are equalities
  } else {
    # GMV portfolio - no mean return constraint
    Amat <- cbind(cSum, cTurnover, cWts,
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
if(printD) {print(Dmat);print(DmatPd)}
if(printA) {print(round(t(Amat),4))}
 
  solution <- solve.QP(DmatPd,dvec,Amat,bvec,meq=(n.eq))
  port.var <- solution$value
  wts = solution$solution[1:nassets]
  wts.buy <- solution$solution[(nassets+1):(2*nassets)]
  wts.sell <- solution$solution[(2*nassets+1):(3*nassets)]
  turnover <- sum(wts.buy,wts.sell)
  #turnover <- sum(abs(wts - rep(1/nassets,nassets))) #This gives same answer
  port.mu <- wts%*%(muVec[1:nassets])
  out = list(wts = wts, wts.buy = wts.buy,wts.sell=wts.sell,
       turnover = turnover,
       port.var=port.var,port.mu=port.mu)
  if(printout) lapply(out,round,4)
}
