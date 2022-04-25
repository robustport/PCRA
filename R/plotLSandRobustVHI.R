#' plotLSandRobustVHI
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
plotLSandRobustVHI <- function (x) 
{
  ret = x
  x = (ret[, 2] - ret[, 3]) * 100
  y = (ret[, 1] - ret[, 3]) * 100
  fit.ls = lm(y ~ x)
  fit.rob = lmrobdetMM(y ~ x, control = lmrobdet.control(efficiency = 0.95))
  plot(x, y, pch = 20, xlab = "Market Returns %", ylab = "VHI Returns (%)", 
       type = "n", main = "")
  abline(fit.rob, col = "black", lty = 1, lwd = 2)
  abline(fit.ls, col = "red", lty = 2, lwd = 2)
  abline(fit.rob$coef[1] + 3 * fit.rob$scale, fit.rob$coef[2], 
         lty = 3, col = "black")
  abline(fit.rob$coef[1] - 3 * fit.rob$scale, fit.rob$coef[2], 
         lty = 3, col = "black")
  ids = which(fit.rob$rweights == 0)
  points(x[-ids], y[-ids], pch = 20)
  points(x[ids], y[ids], pch = 1)
  legend("topleft", legend = c(expression("Robust " ~ hat(beta) == 0.63 ~ (0.23)),
                               expression("       LS " ~ hat(beta) == 1.16 ~ (0.31))),
                               lty = 1:2, col = c("black", "red"), bty = "n",
                                lwd = c(2, 2), cex = 1.2)
}
