#' Robust and Least Square Single Factor Model (SFM) Fits
#' 
#' @description Plot of Least squares and robust single factor model (SFM) fits,
#' with outliers identified, and legend containing slope and intercept coefficient
#' estimates with standard errors in parentheses.
#'
#' @param x A univariate xts object.
#' @param family Robust loss function choice with default mopt
#' @param efficiency Estimator Normal distribution efficiency, default 0.95
#' @param mainText Main title, if any.
#' @param ylimits Vertical axis limits.
#' @param legendPos Legend position.
#' @param makePct Logical variable with default FALSE
#' 
#' @details The robust fit is computed using the lmrobdetMM() function in the
#' R package RobStatTM. For other choices of efficiency and family see the
#' RobStatTM package help(lmrobdetMM)
#'
#' @return No value returned, instead plot with straight line fits and legend
#' is displayed
#'  
#' @export
#'
#' @examples
#' args(plotLSandRobustSFM)
plotLSandRobustSFM <- function(x,family = "mopt", efficiency = 0.95,
                               mainText = NULL, ylimits = NULL, 
                               legendPos = "topleft", makePct = FALSE) {
  ret <- coredata(x)
  # The three columns are security return, market return and risk free rate 
  # Compute excess returns, scale by 100 if we want percent returns
  x <- ret[, 2] - ret[, 3]
  y <- ret[, 1] - ret[, 3]
  
  x_label <- "Market Returns"
  y_label <- "Asset Returns"
  
  if (makePct) {
    x <- x * 100
    y <- y * 100
    x_label <- paste0(x_label,  " (%)") 
    y_label <- paste0(y_label,  " (%)")
  }
  
  control <- RobStatTM::lmrobdet.control(efficiency=efficiency,
                                         family=family)
  
  fit.mOpt <- RobStatTM::lmrobdetMM(y~x, control=control)
  fit.ls   <- lm(y~x)
  
  x <- fit.ls$model$x
  y <- fit.ls$model$y
  
  plot(x, y, xlab = x_label, ylab =y_label, type="n",
       ylim = ylimits, main = mainText, cex.main =1.5, cex.lab=1.5)
  
  abline(fit.mOpt, col="black", lty=1, lwd=2)
  abline(fit.ls, col="red", lty=2, lwd=2)
  
  # fit.mOpt$scale is the robust estimate of scale for residuals
  # +/- 3*fit.mOpt$scale is an approximate 3 sigma confidence band
  abline(fit.mOpt$coef[1] + 3*fit.mOpt$scale, fit.mOpt$coef[2], lty=3, col="black")
  abline(fit.mOpt$coef[1] - 3*fit.mOpt$scale, fit.mOpt$coef[2], lty=3, col="black")
  
  ids <- which(fit.mOpt$rweights==0)
  
  if (length(ids) == 0) {
    points(x, y, pch = 20)
  } else {
    points(x[-ids], y[-ids], pch = 19)
    points(x[ids], y[ids], pch = 1, cex = 2.0)
  }
  
  legend(x = legendPos,
         legend = as.expression(c(bquote("  mOpt   " ~ hat(beta) == .(round(summary(fit.mOpt)$coefficients[2, 1], 2)) ~
                                           "(" ~ .(round(summary(fit.mOpt)$coefficients[2, 2], 2)) ~ ")"),
                                  bquote("  LS       " ~ hat(beta) == .(round(summary(fit.ls)$coefficients[2, 1], 2)) ~
                                           "(" ~ .(round(summary(fit.ls)$coefficients[2, 2], 2)) ~ ")"))),
         lty=c(1,2), col=c("black", "red"), bty="n", cex=1.5 )
  
  # Authors:  Doug Martin and Dan Xia 2020, Thomas Philips 2025
}