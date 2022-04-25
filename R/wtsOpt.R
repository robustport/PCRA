#' wtsOpt
#'
#' @param r 
#' @param c 
#'
#' @return
#' @export
#'
#' @examples
wtsOpt = function(r,c)
{
  n = length(r)
  psi = rep(0,n)
  g1 = -1.944
  g3 = 1.728
  g5 = -.312
  g7 = .016
  c2 = c^2
  k = c/3
  k2 = k^2
  for(i in 1:n)
  {
    x = r[i]/k
    x3 = x^3
    x5 = x^5
    x7 = x^7
    if((abs(x)>2) & (abs(x)<=3))
    {psi[i] = k*(g1*x+g3*x3+g5*x5+g7*x7)}
    else
    {if(abs(x)<=2) 
    {psi[i] = r[i]} else {psi[i] = 0}}
  }
  wts = psi/r
  wts
}