#' plotLSandRobustDD
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
plotLSandRobustDD = function(x)
{
ret = retDD
x=(ret[,2]-ret[,3])*100
y=(ret[,1]-ret[,3])*100
fit.ls = lm(y~x)
fit.rob = lmRob(y~x, control=lmRob.control(efficiency=0.99))
plot(x,y, pch=20, xlab="Market Returns (%)", ylab="DD Returns (%)", type="n")
abline(fit.rob, col="black", lty=1, lwd=2)
abline(fit.ls, col="red", lty=2, lwd=2)
abline(fit.rob$coef[1]+3*1.29*fit.rob$scale, fit.rob$coef[2], lty=3, col="black")
abline(fit.rob$coef[1]-3*1.29*fit.rob$scale, fit.rob$coef[2], lty=3, col="black")
points(x, y, pch=20)
legend("topleft",
       legend=c(expression("Robust " ~ hat(beta)==1.21 ~ (0.128)), 
                expression("      LS " ~ hat(beta)==1.19 ~ (0.076))),
       lty=1:2, col=c("black", "red"), bty="n", cex=1.2 )
id = which(retDD <=-0.24)
arrows(x[id]+1, y[id]+11, x[id]+0.1, y[id]+1, angle=15, length=0.1)
text(x[id]+1, y[id]+12.5, labels="Oct. 20 1987", cex=0.9)
}