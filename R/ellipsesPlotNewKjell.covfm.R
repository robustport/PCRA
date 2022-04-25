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
  