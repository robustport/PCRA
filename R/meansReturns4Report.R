#' Four types of returns means for reporting
#' 
#' Computes arithmetic mean, logarithmic mean, geometric mean and an approximate
#' geometric mean.
#'
#' @param x An xts or numeric vector of return
#' @param robust A logical value controlling whether a classical or robust sample
#' mean and standard deviation is computed. Default is FALSE 
#'
#' @return FourMeans
#' @export
#'
#' @examples
#' args(meanRetReport)
meansReturns4Report <- function(ret,robust = FALSE,eff = 0.95)
{
  if(robust == FALSE) {retMu <- mean(ret)} else
  {x <- RobStatTM::locScaleM(ret,eff = eff)
  retMu <- x$mu
  }
  logret <- log(ret+1)
  if(robust == FALSE) {logretMu <- mean(logret)} else
  {x <- RobStatTM::locScaleM(logret,eff = eff)
  logretMu <- x$mu
  }
  g <- exp(logretMu)-1
  if(robust == FALSE) {S_squared <- var(ret)} else
  {scale <- robustbase::scaleTau2(ret)
  S_squared <- scale^2
  }
  gApprox <- retMu - S_squared/2
  fourMeans <- c(retMu,logretMu,g,gApprox)
  names(fourMeans) <- c("ArithMean","LogMean","GeomMean","ApproxGMean")
  return(fourMeans)
}
