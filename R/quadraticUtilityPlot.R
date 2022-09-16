#' Quadratic Utility Function Plot
#' 
#' Plots the quadratic utility function U(v) = v - v^2.
#' NOTE:  Add LaTeX formula
#'
#' @return
#' A plot of the quadratic utility
#'
#' @examples
#' args(quadraticUtilityPlot)
#' @export
quadraticUtilityPlot <- function()
{
  v = seq(0,1.5,.01)
  u = v-v^2
  ylim = c(-0.7,0.4)
  plot(v,u,type = "l",ylim = ylim, xlab = "v", ylab = "U(v)", lwd = 1.5)
  abline(v = .5, lty = "dotted")
  abline(h = .25, lty = "dotted")
}