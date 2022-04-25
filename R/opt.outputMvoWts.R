#' opt.outputMeanVolWts
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param opt %% ~~Describe \code{opt} here~~
#' @param returns %% ~~Describe \code{returns} here~~
#' @param digits %% ~~Describe \code{digits} here~~
#' @param names %% ~~Describe \code{names} here~~
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#' @note %% ~~further notes~~
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' ##---- Should be DIRECTLY executable !! ----
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.
#' 
#' ## The function is currently defined as
#' function (opt, returns, digits = NULL, names = NULL) 
#' {
#'     wts = opt$weights
#'     sigmasq = as.numeric(t(wts) %*% var(returns) %*% wts)
#'     sigma = sqrt(sigmasq)
#'     mu.ret = apply(returns, 2, mean)
#'     mu = as.numeric(t(wts) %*% mu.ret)
#'     if (is.null(digits)) {
#'         output = wts
#'     }
#'     else {
#'         if (is.null(names)) {
#'             output = list(wts = wts)
#'             output = lapply(output, round, digits)
#'         }
#'         else {
#'             output = list(wts = wts)
#'             names(output) = names
#'             output = lapply(output, round, digits)
#'         }
#'     }
#'     output
#'   }
#' @export
opt.outputMeanVolWts = function(opt,returns,digits = NULL,names = NULL)
{
wts = opt$weights
sigmasq = as.numeric(t(wts)%*%var(returns)%*%wts)
sigma = sqrt(sigmasq)
mu.ret = apply(returns,2,mean)
mu = as.numeric(t(wts)%*%mu.ret)
if(is.null(digits))
    {output = wts} else
    {if(is.null(names))
        {output = list(wts=wts)
         output = lapply(output,round,digits)}
         else
        {output = list(wts=wts)
         names(output) = names
         output = lapply(output,round,digits)}
    }
output
}
