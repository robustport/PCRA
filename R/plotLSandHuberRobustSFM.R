#' @title Plot LS and Huber SFM Fits
#' 
#' @description Computes LS and Huber robust single factor model fits with 
#' standard errors and plots the results
#'
#' @param x xts time series vector 
#' @param mainText Character variable with NULL default
#' @param ylimits Numeric vector of vertical axis limits with NULL default
#' @param legendPos Character variable with default "topleft"
#' @param goodOutlier Logical variable with default FALSE
#' @param makePct Logical variable with default FALSE
#'
#' @returns A plot of the LS and robust Huber SFM fits
#' @export
#'
#' @examples
#' args(plotLSandHuberRobustSFM)
plotLSandHuberRobustSFM  <-  function(x, mainText = NULL, ylimits = NULL,
                                          legendPos = "topleft", 
                                          goodOutlier = FALSE, makePct = FALSE){
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
  
  control  <- RobStatTM::lmrobdet.control(efficiency=0.95, family="mopt")
  fit.mOpt <- RobStatTM::lmrobdetMM(y~x, control=control)
  fit.ls   <- lm(y~x)
  
  plot(x, y, xlab = x_label, ylab = y_label, type = "n",
       ylim = ylimits, main = mainText, cex.main = 1.5, cex.lab = 1.5)
  
  fit.huber <- MASS::rlm(y~x)
  
  abline(fit.mOpt,  col="black", lty=1, lwd=2)
  abline(fit.ls,    col="red",   lty=2, lwd=2)
  abline(fit.huber, col ="blue", lty=5, lwd=2)
  
  # fit.mOpt$scale is the robust estimate of scale for residuals
  # +/- 3*fit.mOpt$scale is an approximate 3 sigma confidence band
  abline(fit.mOpt$coef[1] + 3*fit.mOpt$scale.S, fit.mOpt$coef[2], lty=3, col="black")
  abline(fit.mOpt$coef[1] - 3*fit.mOpt$scale.S, fit.mOpt$coef[2], lty=3, col="black")
  
  ids <- which(fit.mOpt$rweights==0)
  if (length(ids) == 0) {
    points(x, y, pch = 20)
  } else {
    points(x[-ids], y[-ids], pch = 19)
    points(x[ids],  y[ids],  pch = 1, cex = 2.0)
  }
  
  legend(x = legendPos,
         legend = as.expression(c(bquote("  mOpt   " ~ hat(beta) == .(round(summary(fit.mOpt)$coefficients[2, 1], 2)) ~
                                           "(" ~ .(round(summary(fit.mOpt)$coefficients[2, 2], 2)) ~ ")"),
                                  bquote("  Huber  " ~ hat(beta) == .(round(summary(fit.huber)$coefficients[2, 1], 2)) ~
                                           "(" ~ .(round(summary(fit.huber)$coefficients[2, 2], 2)) ~ ")"),
                                  bquote("  LS       " ~ hat(beta) == .(round(summary(fit.ls)$coefficients[2, 1], 2)) ~
                                           "(" ~ .(round(summary(fit.ls)$coefficients[2, 2], 2)) ~ ")"))),
         lty=c(1, 2, 5), col=c("black","blue", "red"), bty="n", cex=1.5 )
  
  if(goodOutlier){
    id <- which(x <= -20)
    if(length(id) > 0){
      print(id)
      arrows(x[id]+1, y[id]+11, x[id]+0.1, y[id]+1, angle=15, length=0.1)
      text(x[id]+1, y[id]+12.5, labels="Oct. 20 1987", cex=1.2)
    }
  }
  # Authors:  Doug Martin and Dan Xia 2020, Thomas Philips 2025
}
