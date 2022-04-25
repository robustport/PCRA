#' Exponential Weighted Moving Average (EWMA) Mean Volitility 
#'
#' @param x returns of the portfolio
#' @param nstart 
#' @param robMean if the robust mean is used, default is T
#' @param robVol if the robust vol is used, default is T
#' @param cc 
#' @param lambdaMean 
#' @param lambdaVol 
#' @param Dyn 
#' @param lambdaMeanDyn 
#' @param lambdaVolDyn 
#'
#' @return
#' 
#' @details The robust EWMA mean algorithm has the form 
#\hat{\mu}_t = \hat{\mu}_{t-1} + (1-\lambda)\hat{\sigma}_{t-1}\psi_{\texttt{hub}}
#\left(\frac{x_t-\hat{\mu}_{t-1}}{\hat{\sigma}_{t-1}}\right)
#'
#' @examples
#' @export


ewmaMeanVol <- function(x,nstart = 10,robMean = T,robVol = T,cc = 2.5,
                        lambdaMean = 0.9,lambdaVol = 0.9, Dyn = F,
                        lambdaMeanDyn = 0.7,lambdaVolDyn = 0.7)
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
    #if(robMean) {resid <- ewmaVol.old*psi_modOpt(resid/ewmaVol.old,
    #                  cc = c(0.01316352,1.05753107,3.00373939,1.0))}
    if(robMean) {
      resid <- ewmaVol.old*psiHuber(resid/ewmaVol.old,cc = cc)
      }
    if(Dyn & abs(resid/ewmaVol.old) >= cc) {
      lambda = lambdaMeanDyn
    } else {
      lambda = lambdaMean  
    }
    ewmaMean.new <- ewmaMean.old + (1 - lambda) * resid
    ewmaMean[i]  <- ewmaMean.new
    residNew     <- x[i]-ewmaMean.new
    ewmaVar.old  <- ewmaVol.old^2
    residVar     <- residNew^2 - ewmaVar.old
    #if(robVol) {sPsi <- ewmaVol.old*psi_modOpt(resid/ewmaVol.old,
    #                  cc = c(0.01316352,1.05753107,3.00373939,1.0))
    #           residVar <- sPsi^2 - ewmaVar.old}
    if(robVol) {
      sPsi <- ewmaVol.old*psiHuber(resid/ewmaVol.old,cc = cc)
      residVar <- sPsi^2 - ewmaVar.old
      }
    if(Dyn & abs(resid/ewmaVol.old) >= cc) {
      lambda = lambdaVolDyn
    } else {
      lambda = lambdaVol  
    }
    ewmaVar.new  <- ewmaVar.old + (1-lambda)*residVar
    ewmaVol.new  <- sqrt(ewmaVar.new)
    ewmaVol[i]   <- ewmaVol.new
    ewmaMean.old <- ewmaMean.new
    ewmaVol.old  <- ewmaVol.new
  }
  ewmaMeanVol <- xts(cbind(ewmaMean,ewmaVol),order.by = index)
  return(ewmaMeanVol)
}
