#' opt.outputMvo
#'
#' @param opt 
#' @param returns 
#' @param digits 
#' @param names 
#' @param rf 
#'
#' @return
#' @export
#'
#' @examples
opt.outputMvo <-
function(opt,returns,digits = NULL,names = NULL,rf = 0)
{
wts = opt$weights
sigmasq = as.numeric(t(wts)%*%var(returns)%*%wts)
sigma = sqrt(sigmasq)
mu.ret = apply(returns,2,mean)
mu = as.numeric(t(wts)%*%mu.ret)
sr = (mu-rf)/sigma
if(is.null(digits))
    {names(sigma) = "sigma"
    names(mu) = "mu"
    output = c(wts,mu,sigma)} else
    {if(is.null(names))
        {output = list(wts=wts,mean=mu,stdev=sigma,sr=sr)
         output = lapply(output,round,digits)}
         else
        {output = list(wts,mu,sigma,sr)
         names(output) = names
         output = lapply(output,round,digits)}
    }
output
}
