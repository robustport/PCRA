#' Four types of returns means for reporting
#' 
#' Computes arithmetic mean, logarithmic mean, geometric mean and an approximate
#' geometric mean.
#'
#' @param x An xts or numeric vector of return
#' @param robust A logical value controlling whether a classical or robust sample
#' mean and standard deviation is computed. Default is FALSE 
#'
#' @return Arithmetic, logarithmic, geometric and approximate geometric means
#' @export
#'
#' @examples
#' args(meanRetReport)
meanReturns4Report <- function(x,robust = FALSE)
{
  if(robust == FALSE) {retMu <- mean(ret)} else
    {x <- RobStatTM::locScaleM(ret)
    retMu <- x$mu
  }
  logret <- log(ret+1)
  if(robust == FALSE) {logretMu <- mean(logret)} else
    {x <- RobStatTM::locScaleM(logret)
    logretMu <- x$mu
  }
  g <- exp(logretMu)-1
  if(robust == FALSE) {S_squared <- var(ret)} else
    {scale <- robustbase::scaleTau2(ret)
    S_squared <- scale^2
  }
  gApprox <- retMu - S_squared/2
  c(retMu,logretMu,g,gApprox)
}
  
  
 