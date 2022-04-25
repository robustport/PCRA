#' This is code for plotting a point mass distribution
#'
#' @param r A vector of values for the plot x axis
#' @param x1 Lower limite of plotting range
#' @param x2 Upper limit of plotting range
#'
#' @return
#' @export
#'
#' @examples
#' edfPointMassFig(.005)
edfPointMassFig <- function(r,x1 = -0.02,x2 = 0.02)
{
x1 = -0.02
x2 = 0.02
r = .005
plot(c(x1,x2),c(0,1.1), type = "n",axes = F, xlab = "x", ylab = "",cex.lab = 1.3,
     main = expression(paste(delta ["r"], "(x)")),cex.main = 1.5)
axis(side=1, pos=0)
axis(side=1, pos=0, at=0.005, labels = "r",cex.axis = 1.3)
yaxistickplaces = seq(.5,1,l=2)
axis(side=2, pos=0,at = yaxistickplaces)
segments(0,0,0,1.2)
abline(h = 0)
segments(x1,0,r,0,lwd = 2)
segments(r,0,r,1,lty = 3)
segments(r,1,x2,1,lwd = 2)
points(r,1,pch = 20,cex = 1.5)
}

