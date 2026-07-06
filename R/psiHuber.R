#' @title Huber psi function
#'
#' @description The Huber psi function psi(x) is the derivative
#' of the Huber rho function, with a tuning constant cc that 
#' controls the trade-off between robustness toward outliers, 
#' and normal distribution estimator efficiency.
#' 
#' @param x Numeric argument of psi function
#' @param cc numeric robustness tuning constant
#' 
#' @details The choice cc = 1.345 results in a 95% normal 
#' distribution efficiency for the Huber location M-estimator.
#' The default value cc = 2.0 is better for EWMA robust filters.
#'
#' @return Numeric value of psi(x)
#' @export
#'
#' @examples
#' psiHuber(0.5)
psiHuber <-  function(x, cc = 2.0)
{
  psi = ifelse(abs(x/cc) < 1, x, cc) 
  psi = ifelse(x/cc <= -1, -cc, psi)
  return(psi)
}