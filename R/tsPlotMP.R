#' @title Lattice Multi-Panel Time Series Plots
#' 
#' @description Lattice multi-panel time series with several plotting style
#' control parameters 
#'
#' @param ret A multivariate xts object 
#' @param add.grid Logical variable, if 'TRUE', type = c('l', 'g'), and if
#' 'FALSE', type = c('l')
#' @param layout Numeric vector of length 2 or 3 giving the number of columns,
#' rows, and pages (optional) for a multipanel lattice display
#' @param type Character variable type of plot: 'l' for a line, 'p' for a
#' point, and 'b' and 'o' both denote both together, deafault 'l'
#' @param yname Character or expression giving label(s) for the y-axis 
#' @param Pct Logical variable with default TRUE
#' @param scaleType Character variable that controls scale of y-axis, choose 
#' from c('same', 'free')
#' @param stripLeft Logical variable to choose the position of Lattice strip,
#' TRUE for drawing strips at the left of each panel, FALSE for drawing
#' strips at the top of each panel
#' @param main A character string, or possibly an expression, for main title 
#' @param lwd The line width, a positive number, defaulting to 1
#' @param stripText.cex Numeric factor by which strip text in the plot(s)
#' are scaled relative to the default 1, 1.5 is 50 percent larger
#' @param axis.cex Numeric factor by which axis in the plot(s) are scaled
#' relative to default of 1, 1.5 is 50 larger larger, 0.5 is 50 percent smaller
#' @param color Specification of plotting color, with default black
#' @param zeroLine Logical variable specifying whether or not a dotted 
#' horizontal line is location at the zero vertical distance, default TRUE
#' 
#' @return Multi-panel Lattice time series plot
#' @export
#' 
#' @author Kirk Li and Doug Martin
#' @examples 
#' #Load the data
#' library(xts)
#' data("stocksCRSP")
#' dat = stocksCRSP
#' returns = tapply(dat$Return,list(dat$Date,dat$TickerLast),I)
#' ret = xts(returns[,1:5],as.yearmon(rownames(returns)))
#' 
#' #generate return time series plot               
#' tsPlotMP(ret, color = 'Blue')
#' tsPlotMP(ret, scaleType = "same", zeroLine = FALSE)
#' tsPlotMP(ret, stripLeft = FALSE, main = 'Time Series Plot')
tsPlotMP <- function (ret, add.grid = FALSE, layout = NULL, type = "l",
              yname = "RETURNS (%)", Pct = F, scaleType = "free", 
              stripLeft = TRUE, main = NULL, lwd = 1, stripText.cex = 1,
              axis.cex = 1, color = "black", zeroLine = TRUE) 
{
  strip.left <- stripLeft
  strip <- !strip.left
  if (add.grid) {
    type <- c("l", "g")
  }
  else {
    type <- type
  }
  if (zeroLine) {
    panel = function(...) {
      panel.abline(h = 0, lty = 3)
      panel.xyplot(...)
    }
  }
  else {
    panel = function(...) {
      panel.xyplot(...)
    }
  }
  if(Pct) ret <- ret*100
  pl = xyplot(ret, par.strip.text = list(cex = stripText.cex), 
          type = type, xlab = "", ylab = list(label = yname, 
          cex = stripText.cex), lwd = lwd, 
          scales = list(y = list(cex = axis.cex, relation = scaleType, rot = 0),
          x = list(cex = axis.cex)), layout = layout, main = main, col = color,
          strip = strip, strip.left = strip.left, panel = panel)
  print(pl)
}

