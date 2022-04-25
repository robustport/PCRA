#' Title
#'
#' @param wts.efront 
#' @param legend.text 
#' @param col 
#' @param ylab 
#' @param xlab 
#' @param bar.ylim 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
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

#' Title
#'
#' @param p 
#' @param n 
#'
#' @return
#' @export
#'
#' @examples
consRockeCh2 <- function(p,n)
{
  beta <- c(-6.1357, -1.0078, 0.81564)
  if (p >= 15) {
    a <- c(1, log(p), log(n))
    alpha <- exp(sum(beta * a))
    gamma <- qchisq(1 - alpha, df = p)/p - 1
    gamma <- min(gamma, 1)
  }
  else {
    gamma <- 1
    alpha <- 1e-06
  }
  return(list(gamma = gamma, alpha = alpha))
}

#' Title
#'
#' @param r 
#' @param x1 
#' @param x2 
#'
#' @return
#' @export
#'
#' @examples
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

#' Title
#'
#' @param x 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
ellipsesPlotNewKjell.covfm = function (x, ...) 
{
  n.models <- length(x)
  mod.names <- names(x)
  p <- dim(x[[1]]$cov)[1]
  if (p == 2) {
    old.par <- par(pty = "s")
    on.exit(par(old.par))
    ellipse <- function(loc, A) {
      detA <- A[1, 1] * A[2, 2] - A[1, 2]^2
      dist <- sqrt(qchisq(0.95, 2))
      ylimit <- sqrt(A[2, 2]) * dist
      y <- seq(-ylimit, ylimit, 0.01 * ylimit)
      sqrt.discr <- detA/A[2, 2]^2 * (A[2, 2] * dist^2 - 
                                        y^2)
      sqrt.discr[c(1, length(sqrt.discr))] <- 0
      sqrt.discr <- sqrt(sqrt.discr)
      b <- loc[1] + A[1, 2]/A[2, 2] * y
      x1 <- b - sqrt.discr
      x2 <- b + sqrt.discr
      y <- loc[2] + y
      rbind(cbind(x1, y), cbind(rev(x2), rev(y)))
    }
    z <- list()
    x.min <- Inf
    x.max <- -Inf
    y.min <- Inf
    y.max <- -Inf
    for (i in 1:n.models) {
      z[[i]] <- ellipse(x[[i]]$center, x[[i]]$cov)
      x.min <- min(x.min, z[[i]][, 1])
      x.max <- max(x.max, z[[i]][, 1])
      y.min <- min(y.min, z[[i]][, 2])
      y.max <- max(y.max, z[[i]][, 2])
    }
    X <- try(eval(x[[1]]$call$data, envir = sys.parent(2)), 
             silent = TRUE)
    if (!inherits(X, "try-error")) {
      X <- as.matrix(X)
      x.min <- min(x.min, X[, 1])
      x.max <- max(x.max, X[, 1])
      y.min <- min(y.min, X[, 2])
      y.max <- max(y.max, X[, 2])
    }
    else {
      X <- matrix(NA, 1, 2)
    }
    center <- c(mean(c(x.min, x.max)), mean(c(y.min, y.max)))
    s.range <- max(abs(c(center[1] - x.min, x.max - center[1], 
                         center[2] - y.min, y.max - center[2])))
    if (n.models == 1) 
      header <- "95% Density Ellipse"
    else header <- "95% Density Ellipses"
    plot(X, xlim = c(center[1] - s.range, center[1] + s.range), 
         ylim = c(center[2] - s.range, center[2] + s.range), 
         main = header, col = "black", pch = 16, asp = 1)
    #    for (i in 1:length(z)) polygon(z[[i]], density = 0, lty = i, 
    #                                   col = i, lwd = 4)
    for (i in 1:length(z)) polygon(z[[i]], density = 0,
                                   lty = n.models-i+1, col = n.models-i+1, lwd = 2)
    # Change in lwd above and below
    pos <- ifelse(x[[1]]$cov[1, 2] > 0, "topleft", "topright")
    #    legend(x = pos, legend = mod.names, col = 1:n.models, 
    #          lty = 1:n.models, lwd = 2, bty = "n")
    mod.names = c("Robust Correlation","Classic Correlation")
    legend(x = pos, legend = mod.names, col = 1:n.models, 
           lty = 1:n.models, lwd = c(2,2), bty = "n")
  }
  else {
    old.par <- par(mar = rep(0.1, 4), pty = "s")
    on.exit(par(old.par))
    labels <- dimnames(x[[1]]$cov)[[1]]
    tl.margin <- max(strwidth(paste("WW", labels, sep = ""), 
                              units = "inches"))
    tl.margin <- 1.05 * tl.margin/par()$fin[1]
    if (tl.margin < 0.05) {
      tl.margin <- 0.05
      cex.labels <- 1
    }
    else if (tl.margin > 0.2) {
      cex.labels <- 0.2/tl.margin
      tl.margin <- 0.2
    }
    else {
      cex.labels <- 1
    }
    
    br.margin <- 0.025 * (n.models - 1)
    plot(NA, NA, xlim = c(0.5 - p * tl.margin, 0.5 + (1 + 
                                                        br.margin) * p), ylim = c(0.5 - p * br.margin, (1 + 
                                                                                                          tl.margin) * p + 0.5), type = "n", axes = FALSE, 
         xlab = "", ylab = "")
    ht.corr <- (1.5 * n.models - 0.5) * strheight("8", units = "user")
    wt.corr <- strwidth("-0.00", units = "user")
    cex.corr <- min(c(0.75/max(c(ht.corr, wt.corr)), 1.25))
    vert <- (1:n.models) * 1.6
    vert <- vert - mean(vert) + 0.5
    for (k in 1:n.models) {
      # if (x[[k]]$corr)
      #if (x[[k]]$cor)
      #X <- x[[k]]$cov
      #else {
      s <- sqrt(diag(x[[k]]$cov))
      X <- x[[k]]$cov/(s %o% s)
      #}
      xc <- col(X)
      yc <- row(X)[p:1, ]
      ut <- row(X) < col(X)
      lt <- row(X) > col(X)
      pts <- c(seq(0, 2 * pi, length.out = 181), NA)
      xs <- sapply(X[ut], function(u, v) cos(v + acos(u)/2), 
                   v = pts)
      xs <- 0.475 * xs + rep(xc[ut], each = 182)
      ys <- sapply(X[ut], function(u, v) cos(v - acos(u)/2), 
                   v = pts)
      ys <- 0.475 * ys + rep(yc[ut], each = 182)
      # Line width and color control below
      #      polygon(x = as.vector(xs), y = as.vector(ys), density = 0, 
      #              lwd = n.models - k + 1, col = k, lty = k)
      polygon(x = as.vector(xs), y = as.vector(ys), density = 0, 
              lwd = 1, col = n.models - k + 1, lty = n.models - k + 1)
      corr <- X[lt]
      corr[corr > 0.99 & corr < 1] <- 0.99
      corr[corr < -0.99 & corr > -1] <- -0.99
      corr <- format(round(corr, digits = 2))
      # Text color control
      #      text(xc[lt] + 0.5 * strwidth("0.00", cex = cex.corr), 
      #           yc[lt], labels = corr, adj = c(1, vert[k]), col = k, 
      #           cex = cex.corr)
      text(xc[lt] + 0.5 * strwidth("0.00", cex = cex.corr), 
           yc[lt], labels = corr, adj = c(1, vert[k]), col = n.models-k+1, 
           cex = cex.corr)
    }
    lines(c(1, p), c(p, 1), col = "gray")
    text(1:p, p + 0.5, paste("  ", labels, sep = ""), adj = 0, 
         srt = 90, cex = cex.labels)
    text(0.5, p:1, paste(labels, "  ", sep = ""), adj = 1, 
         cex = cex.labels)
    # Legend line color and width control below
    #    legend(x = "bottomright", legend = mod.names, lwd = n.models:1, 
    #           col = 1:n.models, lty = 1:n.models, bty = "n", horiz = TRUE)
    legend(x = "bottomright", legend = rev(mod.names), lwd = 1, 
           col = 1:n.models, lty = 1:n.models, bty = "n", horiz = TRUE)
  }
  invisible(x)
}

#' Title
#'
#' @param muRet 
#' @param volRet 
#' @param corrRet 
#' @param npoints 
#' @param display 
#' @param efront.only 
#' @param print 
#' @param digits 
#'
#' @return
#' @export
#'
#' @examples
mathEfrontRiskyMuCov <- function(muRet,volRet,corrRet, npoints = 100,display = T,
                                 efront.only = T, print = F,  digits = NULL) 
{
  covRet = diag(volRet)%*%corrRet%*%diag(volRet)
  names(muRet) = c("Stock 1","Stock 2","Stock 3")
  mu = muRet
  V = covRet
  one = rep(1, nrow(V))
  z1 = solve(V, one)  # Vinv*one
  a = as.numeric(t(mu) %*% z1) # a = mu*Vinv*one
  cc = as.numeric(t(one) %*% z1) # c = one*Vinv*one
  z2 = solve(V, mu) # Vinv*mu
  b = as.numeric(t(mu) %*% z2) # b = mu*Vinv*mu
  d = b * cc - a^2
  muGmv = a/cc
  varGmv = 1/cc
  sigmaGmv = sqrt(varGmv)
  sigma.stocks = sqrt(diag(V))
  mu.max = 1.2 * max(mu)
  sigma.max = (varGmv + 1/(d*varGmv) * (mu.max - muGmv)^2)^0.5
  #sigma.max = 1.2 * sigma.max
  sigma = seq(sigmaGmv + .000001, sigma.max, length = npoints)
  mu.efront = muGmv + (d*varGmv*(sigma^2 - varGmv))^0.5
  if (!efront.only) {
    mu.front = muGmv - (d*varGmv*(sigma^2 - varGmv))^0.5
  }
  xlim = c(0, max(sigma))
  if (efront.only) {
    ylim = range(mu.efront, mu, 0)
  } else
  {ylim = range(mu.efront, mu.front)}
  if (display) {
    plot(sigma, mu.efront, type = "l", lwd = 1.5, xlim = xlim, 
         ylim = ylim, xlab = "VOLATILITY", ylab = "MEAN RETURN")
    if (!efront.only) {
      lines(sigma, mu.front)
    }
    points(sigmaGmv, muGmv, pch = 19, cex = 1)
    text(sigmaGmv, muGmv, "GMV", cex = 1.2, pos = 2)
    points(sigma.stocks, mu, pch = 20, cex = 1.5)
    text(sigma.stocks, mu, names(mu), cex = 1.2, pos = 4)
    text(0.07, 0.095, "EFFICIENT FRONTIER", cex = 1.2)
    arrows(0.07, 0.09, sigma[15], mu.efront[15], length = 0.1, lwd= 1.5)
  }
  if (is.null(digits)) {
    out = list(mu.efront = mu.efront, vol.efront = sigma)
  } else {
    vol.efront = sigma
    out = rbind(mu.efront, vol.efront)
    out = round(out, digits = digits)
  }
  out
}

#' Title
#'
#' @param returns 
#' @param mu.efront 
#' @param digits 
#'
#' @return
#' @export
#'
#' @examples
mathWtsEfrontRisky <- function (returns, mu.efront, digits = NULL) 
{
  V = var(returns)
  mu = apply(returns, 2, mean)
  n.assets <- length(mu)
  one <- rep(1, n.assets)
  z1 = solve(V, one)
  a = as.numeric(t(mu) %*% z1)
  cc = as.numeric(t(one) %*% z1)
  z2 = solve(V, mu)
  b = as.numeric(t(mu) %*% z2)
  d = b * cc - a^2
  g1 = as.matrix((b * z1 - a * z2)/d, ncol = 1)
  g2 = as.matrix((cc * z2 - a * z1)/d, ncol = 1)
  n = length(mu.efront)
  a1 = matrix(rep(g1, n), ncol = n)
  a2 = g2 %*% mu.efront
  wts.efront = a1 + a2
  wts.efront <- as.data.frame(wts.efront)
  vol.efront <- (1/cc + (cc/d) * (mu.efront - a/cc)^2)^0.5
  out <- rbind(vol.efront, mu.efront, wts.efront)
  rowNames <- c("Vol", "Mean", paste("W-", sep = "", names(returns)))
  row.names(out) <- rowNames
  names(out) <- paste("P", sep = "", 1:n)
  if (is.null(digits)) {
    out
  }
  else {
    out <- sapply(out, FUN = round, digits = digits)
    row.names(out) = rowNames
    data.frame(out)
  }
}

#' Title
#'
#' @param muRet 
#' @param volRet 
#' @param corrRet 
#' @param mu.efront 
#' @param digits 
#'
#' @return
#' @export
#'
#' @examples
mathWtsEfrontRiskyMuCov <- function (muRet, volRet, corrRet, mu.efront,
                                     digits = NULL) 
{
  covRet <- diag(volRet) %*% corrRet %*% diag(volRet)
  mu <- muRet
  V <- covRet
  n.assets <- length(mu)
  print(n.assets)
  one <- rep(1, n.assets)
  z1 <- solve(V, one)
  a <- as.numeric(t(mu) %*% z1)
  cc <- as.numeric(t(one) %*% z1)
  z2 <- solve(V, mu)
  b <- as.numeric(t(mu) %*% z2)
  d <- b * cc - a^2
  g1 <- as.matrix((b * z1 - a * z2)/d, ncol = 1)
  g2 <- as.matrix((cc * z2 - a * z1)/d, ncol = 1)
  n <- length(mu.efront)
  a1 <- matrix(rep(g1, n), ncol = n)
  a2 <- g2 %*% mu.efront
  wts.efront <- a1 + a2
  wts.efront <- as.data.frame(wts.efront)
  vol.efront <- (1/cc + (cc/d) * (mu.efront - a/cc)^2)^0.5
  out <- rbind(vol.efront, mu.efront, wts.efront)
  rowNames <- c("Vol", "Mean", paste("W", sep = "", 1:n.assets))
  row.names(out) <- rowNames
  names(out) <- paste("P", sep = "", 1:n)
  if (is.null(digits)) {
    out
  }
  else {
    out <- sapply(out, FUN = round, digits = digits)
    row.names(out) = rowNames
    data.frame(out)
  }
}

#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
plotLSfitVHI <- function (x) 
{
  ret = x
  x = (ret[, 2] - ret[, 3]) * 100
  y = (ret[, 1] - ret[, 3]) * 100
  fit.ls = lm(y ~ x)
  xlim <- c(-6.5,5.5)
  ylim <- c(-25,60)
  plot(x, y, pch = 16, xlab = "Market Returns %",xlim = xlim, 
       ylim = ylim, ylab = "VHI Returns (%)", main = "",
       cex = 1.5,cex.lab = 1.5)
  abline(fit.ls, col = "black", lty = 1, lwd = 1.5)
  legend(-6.0,55.0, legend = c(expression(hat(alpha) ==  -0.01 ~ (0.01)),
                               expression(hat(beta) ==    1.16 ~ (0.31))),
         cex = 1.5,bty = "n")
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
powerUtilityPlots = function()
{
  x = seq(.01,3,.01)
  y = log(x)
  lwd = 1.0
  plot(x,y,axes=F,type = "l", ylim =c(-8,2),lwd = lwd, xlab = "v", ylab = "U(v)")
  axis(side = 1,pos = 0)
  axis(side = 2,pos = 0)
  gamma = -.5
  shift = 1
  y = (x^gamma - shift)/gamma
  lines(x,y,lty = 8,lwd = lwd)
  gamma = .5
  y = (x^gamma - shift)/gamma
  lines(x,y,lty = 3,lwd = lwd)
  abline(v = 0)
  legend(1.2,-5.5,c("Gamma  =  .5", "Log Utility","Gamma  =  -.5"),lty = c(3,1,8),lwd = 1.0)
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
quadraticUtilityPlot <- function()
{
  v = seq(0,1.5,.01)
  u = v-v^2
  ylim = c(-0.7,0.4)
  plot(v,u,type = "l",ylim = ylim, xlab = "v", ylab = "U(v)", lwd = 1.5)
  abline(v = .5, lty = "dotted")
  abline(h = .25, lty = "dotted")
}

#' Title
#'
#' @param tt 
#' @param gamma 
#' @param q 
#'
#' @return
#' @export
#'
#' @examples
WRoTruCh2 <- function(tt,gamma,q = 2)
{
  ss <- (tt - 1)/gamma
  w <- 1 - ss^q
  w[abs(ss) > 1] <- 0
  return(w)
}
