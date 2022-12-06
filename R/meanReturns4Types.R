#' @title Four Types of Mean Returns
#' 
#' @description Computation of arithmetic mean, logarithmic mean, geometric,
#' mean and an approximate geometric mean.
#'
#' @param return An xts object or a numeric vector of returns
#' @param robust A logical value controlling whether a classical or robust
#' sample mean and standard deviation is computed. Default is FALSE
#' @param eff Normal distribution efficiency of RobStatTM function locScaleM()
#' used for computing a robust location estimate 
#'
#' @return fourMeans numeric vector of the four means
#' @export
#'
#' @examples
#' args(meanReturns4Types)
meanReturns4Types <- function(return, robust = FALSE, eff = 0.95)
{
  if(robust == FALSE) {retMu <- mean(ret)} else
  {x <- locScaleM(ret, eff = eff)
  retMu <- x$mu
  }
  logret <- log(ret+1)
  if(robust == FALSE) {logretMu <- mean(logret)} else
  {x <- locScaleM(logret, eff = eff)
  logretMu <- x$mu
  }
  g <- exp(logretMu)-1
  if(robust == FALSE) {S_squared <- var(ret)} else
  {scale <- scaleTau2(ret)
  S_squared <- scale^2
  }
  gApprox <- retMu - S_squared/2
  fourMeans <- c(retMu,logretMu,g,gApprox)
  names(fourMeans) <- c("ArithMean","LogMean","GeomMean","ApproxGMean")
  return(fourMeans)
}
