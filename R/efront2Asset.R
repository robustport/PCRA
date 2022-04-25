#' Efficient Frontier for 2-Asset Portfolio
#'
#' @param wts 
#' @param rho 
#' @param muVol 
#'
#' @return
#' @export
#'
#' @examples
efront2Asset = function(wts,rho,muVol = c(.20,.10,.15,.04))
{
  sigma1 = muVol[1]
  mu1 = muVol[2]
  sigma2 = muVol[3]
  mu2 = muVol[4]
  n = length(wts)
  efront = data.frame(matrix(rep(0,3*n),ncol = 3))
  names(efront) = c("SIGMA","MU","WTS")
  w = wts
  for(i in 1:n){
    mu = w[i]*mu1 + (1-w[i])*mu2
    var = w[i]^2*sigma1^2 + 2*w[i]*(1-w[i])*rho*sigma1*sigma2 + (1-w[i])^2*sigma2^2
    sigma = sqrt(var)
    efront[i,] = c(sigma,mu,w[i])
  }
  efront
}