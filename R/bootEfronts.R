#' bootEfronts
#'
#' @param returns 
#' @param pspec 
#' @param rf 
#' @param npoints 
#' @param B 
#' @param Seed 
#' @param gmv 
#' @param maxSR 
#' @param xlim 
#' @param ylim 
#' @param k.sigma 
#' @param k.mu 
#' @param digits 
#' @param figTitle 
#'
#' @return
#' @export
#'
#' @examples
bootEfronts = function(returns,pspec, rf=.03, npoints=20, B = 3, Seed = NULL, gmv=T, maxSR=F,
                       xlim  = NULL, ylim = NULL, k.sigma = 2, k.mu = 2, digits = 4, figTitle = NULL)
{
  # k.sigma controls horizontal axis plotting range if no xlim
  # k.mu controls vertical axis plotting range if no ylim
  # Adjust k.mu, k.sigma to minimize plot "Line out of bounds" Warnings
  # gmv = T to display bullet at global minimum var. portfolio
  # maxSR = T to display bullet at tangency portfolio
  
  # Set axes ranges
  if(is.null(xlim))
  {sigma = apply(returns,2,sd);xlim = k.sigma*c(0,max(sigma))} else
  {xlim = xlim}
  if(is.null(ylim))
  {mu = apply(returns,2,mean);xlim = k.mu*c(0,max(mu))} else
  {xlim = xlim}
  
  # Plot Original Mean-Variance Efficient Frontier
  xlab = "STANDARD DEVIATION"
  ylab = "MEAN RETURN"
  chart.Efront(returns,pspec,firstEfront = T,gmv = gmv, maxSR = maxSR,rf = rf, xlim=xlim,
               ylim=ylim,xlab = xlab,ylab = ylab,n.portfolios = 20)
  if(!is.null(figTitle)) {title(main = figTitle)}
  
  # Compute Bootstrap Samples Indices
  returns.df = coredata(returns)
  n = nrow(returns.df)
  m = ncol(returns.df)
  if(!is.null(Seed)) {set.seed(Seed)}
  boot.idx = sample(n,n*B,replace=T)
  boot.index = matrix(boot.idx,n,B)
  gmvMaxSR = matrix(rep(0,5*B),B)
  
  # Compute and Plot Classic Bootstrapped Frontiers
  for(i in 1:B) 
  {gmvMaxSR[i,] = chart.Efront(returns[boot.index[,i],],pspec,firstEfront = F,gmv = gmv, maxSR = maxSR,
                               rf = rf, xlim=xlim,ylim=ylim,xlab = xlab,ylab = ylab,n.portfolios = 20)}
  out = round(apply(gmvMaxSR,2,sd),digits)
  legend("topleft",bty = "n",title = "   STANDARD DEVIATIONS",legend = 
           c(paste("gmvMu:", out[1]),paste("gmvSd:", out[2]),
             paste("tanMu:", out[3]),paste("tanSd:", out[4])))
}