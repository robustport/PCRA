#' opt.outputMeanVol
#'
#' @param opt 
#' @param returns 
#' @param digits 
#' @param names 
#'
#' @return
#' @export
#'
#' @examples
opt.outputMeanVol <-
function(opt,returns,digits = NULL,names = NULL)
{
wts = opt$weights
sigmasq = as.numeric(t(wts)%*%var(returns)%*%wts)
sigma = sqrt(sigmasq)
mu.ret = apply(returns,2,mean)
mu = as.numeric(t(wts)%*%mu.ret)
if(is.null(digits))
    {names(sigma) = "sigma"
    names(mu) = "mu"
    output = c(wts,mu,sigma)} else
    {if(is.null(names))
        {output = list(wts=wts,mean=mu,vol=sigma)
         output = lapply(output,round,digits)}
         else
        {output = list(wts,mu,sigma)
         names(output) = names
         output = lapply(output,round,digits)}
    }
output
}
