#' Power Utility Function Plots
#' 
#' Uses the power utility function formulas to plot power utility 
#' functions for gamma = 0.5, gamma = -0.5 and the log utility function
#' limiting case of a power utility function as gamma -> 0.
#'  NOTE: Need to add  LaTex form of the power utility function formula
#'
#' @return
#' A figue of the above three utility functions overlaid
#'
#' @examples
#' args(powerUtilityPlots)
#' @export
powerUtilityPlots = function()
{
  x = seq(.01,3,.01)
  y = log(x)
  lwd = 1.0
  plot(x,y,axes=F,type = "l", ylim =c(-8,2),lwd = lwd, xlab = "v", ylab = "U(v)")
  axis(side = 1,pos = 0)
  axis(side = 2,pos = 0)
  gamma = -.5
  shift = 1
  y = (x^gamma - shift)/gamma
  lines(x,y,lty = 8,lwd = lwd)
  gamma = .5
  y = (x^gamma - shift)/gamma
  lines(x,y,lty = 3,lwd = lwd)
  abline(v = 0)
  legend(1.2,-5.5,c("Gamma  =  .5", "Log Utility","Gamma  =  -.5"),lty = c(3,1,8),lwd = 1.0)
}