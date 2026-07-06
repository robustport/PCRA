#' @title EWMA Mean and Volatility
#'
#' @description Computes EWMA joint mean and vol, with options for
#' robust versus classic EWMA estimates.
#' 
#' @param x An xts returns object 
#' @param nstart Integer number of returns for initial ewma estimates
#' @param robMean Logical variable, if TRUE compute robust ewmaMean,
#' if FALSE compute classic classic ewmaMean. Default is TRUE
#' @param robVol Logical variable, if TRUE compute robust ewmaVol,
#' if FALSE compute classic ewmaVol. Default is TRUE. 
#' @param cc Numeric value, robustness tuning constant. Default is 2.5
#' @param lambdaMean Numeric value, decay rate constant for ewmaMean 
#' @param lambdaVol Numeric value, decay rate constant for ewmaVol 
#'
#' @return A bivarate xts object containing the ewmaMean and ewmaVol
#' @export
#'
#' @examples
#' args(ewmaMeanVol)
ewmaMeanVol <- function(x,nstart = 10, robMean = T, robVol = T, 
                        cc = 2.5, lambdaMean = 0.9, lambdaVol = 0.9)
{ 
  n <- length(x)
  index = index(x)
  x <- coredata(x)
  # Compute initial robust mean and vol estimates
  mean.start <- median(x[1:nstart])
  vol.start  <- mad(x[1:nstart])
  # Create output vectors with initial estimates and zeros
  ewmaMean <- c(rep(mean.start, nstart), rep(0, n - nstart))
  ewmaVol  <- c(rep(vol.start, nstart), rep(0, n - nstart))
  # EWMA recursion
  ewmaMean.old <- mean.start
  ewmaVol.old  <-vol.start
  ns1 <- nstart + 1
  for(i in ns1:n) 
  {
    resid <- x[i]-ewmaMean.old
    
    if(robMean) {
      resid <- ewmaVol.old*psiHuber(resid/ewmaVol.old,cc = cc)
    }
    
    ewmaMean.new <- ewmaMean.old + (1 - lambdaMean) * resid
    ewmaMean[i]  <- ewmaMean.new
    residNew     <- x[i]-ewmaMean.new
    ewmaVar.old  <- ewmaVol.old^2
    residVar     <- residNew^2 - ewmaVar.old
    
    if(robVol) {
      sPsi <- ewmaVol.old*psiHuber(resid/ewmaVol.old,cc = cc)
      residVar <- sPsi^2 - ewmaVar.old
    }
    
    ewmaVar.new  <- ewmaVar.old + (1-lambdaVol)*residVar
    ewmaVol.new  <- sqrt(ewmaVar.new)
    ewmaVol[i]   <- ewmaVol.new
    ewmaMean.old <- ewmaMean.new
    ewmaVol.old  <- ewmaVol.new
  }
  ewmaMeanVolOut <- xts(cbind(ewmaMean, ewmaVol), order.by = index)
  return(ewmaMeanVolOut)
}
