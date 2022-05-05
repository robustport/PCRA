plotLSandRobustSFM = function(x,family = "mopt", efficiency = 0.95,
                mainText = NULL, ylimits = NULL, legendPos = "topleft",
                goodOutlier = F, makePct = FALSE)
{
  ret = coredata(x)
  x = ret[,2]-ret[,3]
  y = ret[,1]-ret[,3]
  if (makePct) {
    x = x * 100
    y = y * 100
  }
  control <- RobStatTM::lmrobdet.control(efficiency=efficiency,
                                         family=family)
  fit.mOpt = RobStatTM::lmrobdetMM(y~x, control=control)
  fit.ls = lm(y~x)
  
  x = fit.ls$model$x
  y = fit.ls$model$y
  plot(x,y, xlab="Market Returns (%)", ylab="Returns (%)", type="n",
       ylim = ylimits, main = mainText, cex.main =1.5, cex.lab=1.5)
  abline(fit.mOpt, col="black", lty=1, lwd=2)
  abline(fit.ls, col="red", lty=2, lwd=2)
  # 3 is the approximate value here, true value is get using 
  abline(fit.mOpt$coef[1]+3*fit.mOpt$scale, fit.mOpt$coef[2], lty=3, col="black")
  abline(fit.mOpt$coef[1]-3*fit.mOpt$scale, fit.mOpt$coef[2], lty=3, col="black")
  
  ids=which(fit.mOpt$rweights==0)
  
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
  # Authors:  Doug Martin and Dan Xia 2020
}
