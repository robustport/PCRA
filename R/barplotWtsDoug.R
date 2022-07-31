#' Barplot of Efficient Frontier Weights
#'
#' @param wts.efront 
#' @param legend.text 
#' @param col 
#' @param ylab 
#' @param xlab 
#' @param bar.ylim 
#' @param ... 
#'
#' @return A figure
#' @export
#'
#' @examples
#' args(barplotWtsDoug)
barplotWtsDoug <- function (wts.efront, legend.text = NULL, col = NULL,
                            ylab = NULL, xlab = "VOL", bar.ylim = NULL, ...) 
{
  x <- wts.efront[-c(1, 2), ]
  n = ncol(x)
  p = nrow(x)
  xpos = (abs(x) + x)/2
  xneg = (x - abs(x))/2
  if (is.null(bar.ylim)) {
    ymax <- max(colSums(xpos, na.rm = T))
    ymin <- min(colSums(xneg, na.rm = T))
    ylim = c(ymin * 1.2, ymax * 1.2)
  }
  else {
    ylim = bar.ylim
  }
  # colnames(xpos) <- xlab
  barplot(xpos, legend.text = legend.text, col = col, ylab = ylab, 
          xlab = xlab, ylim = ylim, las = 2, cex.names = 0.8, 
          bty = "n", args.legend = list(x = "topright", 
                                        xjust = 1, y = max(apply(xpos, 2, max)), bty = "n"), 
          ...)
  barplot(xneg, add = T, col = col, axisnames = FALSE, axes = FALSE)
  abline(h = 0)
}