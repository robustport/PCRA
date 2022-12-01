#' @title Bootstrapped Efficient Frontiers
#'
#' @description Computes and plots bootstrapped portfolio efficient frontiers, with 
#' optional points for GMV portfolios and tangency portfolios.
#' 
#' @param returns A multivarite xts returns data set
#' @param pspec Portfoliol analytics portfolio specification object
#' @param rf Risk-free rate as a decimal, default is 0.003
#' @param npoints Number of points on efficient frontier, default = 10
#' @param B Number of bootstrap samples, default = 3 
#' @param Seed Seed of bootstrap random number generator, default = NULL 
#' @param gmv Logical variable with default is gmv = TRUE
#' @param maxSR Logical variable with default maxSR = FALSE
#' gmv = TRUE 
#' @param xlim Numeric x axis plot limits, default = NULL
#' @param ylim Numeric y axis plot limits, default = NULL
#' @param k.sigma Numeric value
#' @param k.mu Numeric value
#' @param digits Number of significant digits for numeric values displayed 
#' @param figTitle Optional figure title with default figTitle = NULL
#' 
#' @details k.sigma controls horizontal axis plotting range if xlim = NULL, and
#' k.mu controls vertical axis plotting range if ylim = NULL. Adjust k.mu and
#' k.sigma to eliminate plot "Line out of bounds" Warnings.
#' gmv = TRUE to display bullet at global minimum var. portfolio
#' maxSR = TRUE to display bullet at tangency portfolio 
#'
#' @return Bootstrapped efficient frontiers plot
#' @export
#'
#' @examples
#' args(bootEfronts)
bootEfronts <- function(returns,pspec, rf = 0.003, npoints = 20, B = 3, 
              Seed = NULL, gmv = TRUE, maxSR = FALSE, xlim  = NULL, ylim = NULL,
              k.sigma = 2, k.mu = 2, digits = 4, figTitle = NULL)
{
  # Set axes ranges
  if(is.null(xlim))
  {sigma <- apply(returns, 2, sd); xlim <- k.sigma*c(0, max(sigma))} else
  {xlim <- xlim}
  if(is.null(ylim))
  {mu <- apply(returns, 2, mean); xlim <- k.mu*c(0, max(mu))} else
  {xlim <- xlim}
  
  # Plot Original Mean-Variance Efficient Frontier
  xlab <- "STANDARD DEVIATION"
  ylab <- "MEAN RETURN"
  chart.Efront(returns, pspec, firstEfront = T, gmv = gmv, maxSR = maxSR, 
               rf = rf, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab,
               n.portfolios = 20)
  if(!is.null(figTitle)) {title(main = figTitle)}
  
  # Compute Bootstrap Samples Indices
  returns.df <- coredata(returns)
  n <- nrow(returns.df)
  m <- ncol(returns.df)
  if(!is.null(Seed)) {set.seed(Seed)}
  boot.idx <- sample(n, n*B, replace=T)
  boot.index <- matrix(boot.idx,n,B)
  gmvMaxSR <- matrix(rep(0,5*B),B)
  
  # Compute and Plot Classic Bootstrapped EfficientFrontiers
  for(i in 1:B) 
  {gmvMaxSR[i,] = chart.Efront(returns[boot.index[,i],], pspec, firstEfront = F,
                    gmv = gmv, maxSR = maxSR, rf = rf, xlim=xlim, ylim = ylim,
                    xlab = xlab, ylab = ylab ,n.portfolios = 20)}
  out = round(apply(gmvMaxSR, 2, sd), digits)
  legend("topleft", bty = "n", title = "   STANDARD DEVIATIONS", legend = 
           c(paste("gmvMu:", out[1]), paste("gmvSd:", out[2]),
             paste("tanMu:", out[3]), paste("tanSd:", out[4])))
}