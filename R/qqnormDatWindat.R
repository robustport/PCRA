#' @title qqnormDatWindat
#' 
#' @description Normal QQPlot of data and Winsorized data
#' 
#' @param dat Numeric data vector
#' @param windat Numeric Winsorized data set
#' @param fraction Fraction of data that is Winsorized
#' @param ylim Numeric data with two values that control vertical plot range
#' @param main Character main title of plot
#' @param facName Character data for y axis label
#' 
#' @details The result plot displays a normal QQPlot of the orginal data
#' as solid points, along with the horizontal display of the Winsorized
#' data as small circles.
#'
#' @return A normal QQPlot of data with overlaid Winsorized data
#' @export
#'
#' @examples
#' args(qqnormDatWindat)
qqnormDatWindat <- function(dat, windat, fraction = 0.01, ylim = NULL, 
                           main = main, facName = NULL)
{
  qqnorm(dat, pch = 16, cex = 0.5, ylim = ylim, main = main,
         xlab = "N(0,1) Quantiles", ylab = facName, cex.lab = 1.2)
  n = length(dat)
  p = (seq(1,n) - 0.5)/n
  qtile = qnorm(p)
  y = sort(windat)
  lo <- floor(n * fraction) + 1
  hi <- n + 1 - lo
  qtileVals <- qtile[c(1:lo, hi:n)]
  yVals <- y[c(1:lo, hi:n)]
  points(qtileVals, yVals, pch = 1, cex = 1.1)
  yloc = ylim[2]
  legend(-3.5, yloc, legend = c("Original Data", "1% Winsorized"),
         pch = c(16,1), cex = 1.1, bty = "n")     
}