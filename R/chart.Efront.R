#' @title Create Efficient Frontier
#'
#' @description Utility function for creating initial efficient frontier, and
#' for creating subsequent bootstrap efficient frontiers created, all of which
#' are created by the PortfolioAnalytics function create.EfficientFrontier.
#' 
#' @param returns A multivarite xts returns object
#' @param pspec PortfolioAnalytics portfolio specification object
#' @param firstEfront Logical variable, default TRUE
#' @param gmv Logical variable, default TRUE
#' @param maxSR Logical variable, default TRUE
#' @param rf Risk-free rate, default 0.003
#' @param xlim Numeric value, default NULL
#' @param ylim Numeric value, default NULL 
#' @param xlab Numeric value, default NULL 
#' @param ylab Numeric value, default NULL 
#' @param n.portfolios Number of efficient frontier portfolios, default 10
#' 
#' @details The variable firstEfront is set to TRUE for the initial efficient
#' frontier plot, but is set to FALSE for the bootstrap replicate efficient
#' frontier plots. The choices gmv = TRUE and maxSR = TRUE result in bullet points
#' at those locations on the initial efficient frontier plot
#'
#' @return No value returned, instead plots of efficient frontiers for use by bootEfronts()
#' @export
#'
#' @examples
#' args(chart.Efront)
chart.Efront <- function(returns, pspec, firstEfront = TRUE, gmv = TRUE, 
                         maxSR = TRUE, rf = 0.003, xlim = NULL, ylim = NULL,
                         xlab = NULL, ylab = NULL, n.portfolios = 10)
{
  efront <- create.EfficientFrontier(returns, pspec, type = "mean-StdDev",
                                     n.portfolios)
  ef.sd <- efront$frontier[,"StdDev"]
  ef.mu <- efront$frontier[,"mean"]
  
  # Compute minimum variance portfolio
  port.gmv <- c(ef.sd[1], ef.mu[1])
  names(port.gmv) <- c("SD.GMV", "MU.GMV")
  
  # Compute tangency portfolio
  ef.sharpe <- (ef.mu - rf)/ef.sd
  iopt <- which.max(ef.sharpe)
  sharpe.max <- ef.sharpe[iopt]
  names(sharpe.max) <- "SHARPE"
  port.maxSR <- c(ef.sd[iopt], ef.mu[iopt])
  names(port.maxSR) <- c("SD.maxSR", "MU.maxSR")
  
  # Plot results
  if(firstEfront == TRUE)
  {plot(ef.sd, ef.mu, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, 
        lwd = 2, type = "l")} 
  else 
  {lines(ef.sd, ef.mu, lty = "dashed")}
  if(gmv == TRUE)
    points(port.gmv[1], port.gmv[2], pch = 19)
  if(maxSR == TRUE)
    points(port.maxSR[1], port.maxSR[2], pch = 19)
  round(c(port.gmv, port.maxSR, sharpe.max),4)
}