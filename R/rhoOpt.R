#' rhoOpt
#'
#' @param r 
#' @param c 
#'
#' @return
#' @export
#'
#' @examples
rhoOpt = function(r,c)
{
  n = length(r)
  rho = rep(0,n)
  h0 = 1.792
  h2 = -.972
  h4 = .432
  h6 = -.052
  h8 = .002
  c2 = c^2
  k = c/3
  k2 = k^2
  for(i in 1:n)
  {
  x = r[i]/k
  x2 = x^2
  x4 = x^4
  x6 = x^6
  x8 = x^8
  if((abs(x)>2) & (abs(x)<=3))
    {rho[i] = k2*(h0+h2*x2+h4*x4+h6*x6+h8*x8)}
    else
    {if(abs(x)<=2) 
      {rho[i] = .5*r[i]^2} else {rho[i] = 3.25*k2}}
    }
  rho
}