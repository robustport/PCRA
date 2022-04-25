#' Huber Rho
#' @param x
#' @param cc
#' @author Kirk Li  \email{kirkli@@stat.washington.edu} 
#' @seealso \code{\link{}}
#' @keywords huber
#' @examples
#' @export
rhoHuber = function(x,cc=1.345)
{rho = ifelse(abs(x/cc) <1, 0.5*x^2, cc*abs(x)-0.5*cc^2) 
return(rho)
}
#' Huber Psi
#' @param x
#' @param cc
#' @author Kirk Li  \email{kirkli@@stat.washington.edu} 
#' @seealso \code{\link{}}
#' @keywords huber
#' @examples
#' @export
psiHuber = function(x,cc=1.345)
{psi = ifelse(abs(x/cc) < 1, x, cc) 
psi = ifelse(x/cc <= -1, -cc, psi)
return(psi)
}
#' Huber Wts
#' @param x
#' @param cc
#' @author Kirk Li  \email{kirkli@@stat.washington.edu} 
#' @seealso \code{\link{}}
#' @keywords huber
#' @examples
#' @export
wtsHuber = function(x,cc=1.345)
{wts = ifelse(abs(x/cc) < 1, 1, cc/(sign(x)*x)) 
return(wts)
}

#' Efficiency From Constant Huber Psi
#' @param x
#' @param cc
#' @author Kirk Li  \email{kirkli@@stat.washington.edu} 
#' @seealso \code{\link{}}
#' @keywords huber
#' @examples
#' @export
effFromConstHuber <- function(cc = 1.345)
{
  integrand.top <- function(x,cc)
    psiHuber(x,cc)^2 * dnorm(x)
  
  nu.top <- 2.0 * integrate(integrand.top, 0.0, cc, cc = cc)$value
  nu.top <- nu.top + 2*pnorm(-cc)*cc^2
  
  nu.bottom <-  (pnorm(cc) - pnorm(-cc))^2
  varHuber <- nu.top / nu.bottom
  1.0 / varHuber
}
#' Constant from Efficiency Huber Psi
#' @param x
#' @param cc
#' @author Kirk Li  \email{kirkli@@stat.washington.edu} 
#' @seealso \code{\link{}}
#' @keywords huber
#' @examples
#' @export
constFromEffHuber <- function(eff, interval = c(1e-6, 3))
{
  obj <- function(cc, e)
    e - effFromConstHuber(cc)
  
  uniroot(obj, interval = interval, e = eff, check.conv = TRUE, tol = 1e-8)$root
}



#' Psi Hard Rejection
#' @param x
#' @param cc
#' @author Kirk Li  \email{kirkli@@stat.washington.edu} 
#' @seealso \code{\link{}}
#' @keywords huber
#' @examples
#' @export
psiHardRej = function(x,cc = 2.5)
{psi = ifelse(abs(x/cc) < 1, x, 0)
	return(psi)
}



