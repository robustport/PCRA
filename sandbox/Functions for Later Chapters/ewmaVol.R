#' Exponential Weighted Moving Average (EWMA) Volitility 
#'
#' @param x returns of the portfolio
#' @param nstart startup values, default is nstart = 10
#' @param robVol if the robust vol is used, default is TRUE
#' @param cc robustness tuning constant, default 2.5
#' @param lambda EWMA decay parameter, default 0.9
#'
#' @return Time series of classical or robust EWMA volatility
#'
#' @examples
#' args(ewmaVol)
#' @export
ewmaVol <- function(x,nstart = 10,robVol = TRUE,cc = 2.5,lambda = 0.9)
{
  n <- length(x)
  index = index(x)
  x <- coredata(x)
  # Compute initial robust mean and vol estimates
  vol.start  <- mad(x[1:nstart])
  # Create output vectors with initial estimates and zeros
  ewmaVol  <- c(rep(vol.start, nstart), rep(0, n - nstart))
  
  # EWMA recursion
  ewmaVol.old  <-vol.start
  ns1 <- nstart + 1
  for(i in ns1:n) 
  {
    ewmaVar.old  <- ewmaVol.old^2
    if(robVol) {
      xPsi <- ewmaVol.old*psiHuber(x[i]/ewmaVol.old,cc = cc)
      residVar <- xPsi^2 - ewmaVar.old
      } else
    {residVar <- x[i]^2 - ewmaVar.old}
    ewmaVar.new  <- ewmaVar.old + (1-lambda)*residVar
    ewmaVol.new  <- sqrt(ewmaVar.new)
    ewmaVol[i]   <- ewmaVol.new
    ewmaVol.old  <- ewmaVol.new
  }
  ewmaVol <- xts(ewmaVol,order.by = index)
  return(ewmaVol)
}
