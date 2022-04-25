#' turnOverSimple
#'
#' @param wts 
#' @param wtsInit 
#' @param digits 
#'
#' @return
#' @export
#'
#' @examples
turnoverSimple = function(wts,wtsInit = NULL,digits = NULL)
{
if(is.null(wtsInit)) {
    n = length(wts)
    wtsInit = rep(1/n,n)}
to = sum(abs(wts-wtsInit))
if(is.null(digits)) {list(turnover = to)}
  else {to = round(to,digits)
  list(turnover = to)}
}