#' chart.Efront
#'
#' @param returns 
#' @param pspec 
#' @param firstEfront 
#' @param gmv 
#' @param maxSR 
#' @param rf 
#' @param xlim 
#' @param ylim 
#' @param xlab 
#' @param ylab 
#' @param n.portfolios 
#'
#' @return
#' @export
#'
#' @examples
chart.Efront <- function(returns,pspec,firstEfront = T,gmv = T, maxSR = T,rf = 0,xlim = NULL,
                         ylim = NULL, xlab = NULL, ylab = NULL, n.portfolios = 10)
{
  efront = create.EfficientFrontier(returns,pspec,type = "mean-StdDev",n.portfolios)
  ef.sd = efront$frontier[,"StdDev"]
  ef.mu = efront$frontier[,"mean"]
  # Compute minimum variance portfolio
  port.gmv = c(ef.sd[1], ef.mu[1])
  names(port.gmv) = c("SD.GMV", "MU.GMV")
  # Compute tangency portfolio
  ef.sharpe = (ef.mu - rf)/ef.sd
  iopt = which.max(ef.sharpe)
  sharpe.max = ef.sharpe[iopt]
  names(sharpe.max) = "SHARPE"
  port.maxSR = c(ef.sd[iopt], ef.mu[iopt])
  names(port.maxSR) = c("SD.maxSR", "MU.maxSR")
  # Plot results
  if(firstEfront == T)
  {plot(ef.sd, ef.mu,xlim = xlim,ylim = ylim, xlab = xlab, ylab = ylab,lwd = 2, type = "l")} 
  else 
  {lines(ef.sd, ef.mu, lty = "dashed")}
  if(gmv == T)
    points(port.gmv[1], port.gmv[2], pch = 19)
  if(maxSR == T)
    points(port.maxSR[1], port.maxSR[2], pch = 19)
  round(c(port.gmv, port.maxSR,sharpe.max),4)
}