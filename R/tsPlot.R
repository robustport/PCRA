#' tsPlot
#'
#' @param ret 
#' @param add.grid 
#' @param cex 
#'
#' @return
#' @export
#'
#' @examples
tsPlot = function(ret,add.grid = T,cex = 1.0)
{
  if(add.grid) {type = c("l","g")} else
  {type = "l"}
  xyplot(100*ret,par.strip.text = list(cex = cex),type = type,
         xlab = "", ylab = list(label="RETURNS (%)",cex = cex),
         scales = list(y = list(cex = cex),x = list(cex = cex) )) 
}
