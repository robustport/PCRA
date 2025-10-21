# The first 9 R packages below need to be installed on your computer,
# which can be easily done in RStudio using Tools/Install Packages

# PCRA
# data.table
# facmodCS
# xts
# RobStatTM
# fit.models
# sandwhich
# MASS
# tensr

# The R package optimalRhoPsi is also needed, and it can be installed
# at the R Console with:
# devtools::install_github("kjellpk/optimalRhoPsi")


##################################################################
##################################################################

# title: "Reproducibility Code for September 2023 JPM Paper"
# authors:  Doug Martin and Stoyan Stoyanov
# date: 8/28/2023

# Load  auxiliary functions needed for EXHIBITS code

plotLSandRobustSFM_SS <- function (x, family = "mopt", efficiency = 0.95, mainText = NULL, 
                                   ylimits = NULL, legendPos = "topleft", makePct = FALSE, 
                                   xlab = NULL, ylab = NULL){
  ret = coredata(x)
  x = ret[, 2] - ret[, 3]
  y = ret[, 1] - ret[, 3]
  if (makePct) {
    x = x * 100
    y = y * 100
  }
  control <- RobStatTM::lmrobdet.control(efficiency = efficiency, family = family)
  fit.mOpt = RobStatTM::lmrobdetMM(y ~ x, control = control)
  fit.ls = lm(y ~ x)
  x = fit.ls$model$x
  y = fit.ls$model$y
  plot(x, y, xlab = xlab, ylab = ylab, 
       type = "n", ylim = ylimits, main = mainText, cex.main = 1.2, 
       cex.lab = 1.2)
  abline(fit.mOpt, col = "black", lty = 1, lwd = 2)
  abline(fit.ls, col = "red", lty = 2, lwd = 2)
  abline(fit.mOpt$coef[1] + 3 * fit.mOpt$scale, fit.mOpt$coef[2], 
         lty = 3, col = "black")
  abline(fit.mOpt$coef[1] - 3 * fit.mOpt$scale, fit.mOpt$coef[2], 
         lty = 3, col = "black")
  ids = which(fit.mOpt$rweights == 0)
  if (length(ids) == 0) {
    points(x, y, pch = 20)
  }
  else {
    points(x[-ids], y[-ids], pch = 19)
    points(x[ids], y[ids], pch = 1, cex = 2)
  }
  
  
  legend(x = legendPos, legend = as.expression(c(bquote("mOpt   "
                                                        ~ hat(f) == .(round(summary(fit.mOpt)$coefficients[2, 1], 2))
                                                        ~ "(" ~ .(round(summary(fit.mOpt)$coefficients[2, 2], 2)) ~ ")"),
                                                 bquote("LS       " ~ hat(f) == .(round(summary(fit.ls)$coefficients[2,1], 2))
                                                        ~ "(" ~ .(round(summary(fit.ls)$coefficients[2, 2], 2)) ~ ")"))), 
         lty = c(1, 2), lwd = c(2, 2), col = c("black", "red"), bty = "n", cex = 1.2)
  grid()
}

plotLSandRobustSFM_win <- function (x, family = "mopt", efficiency = 0.95, mainText = NULL, 
                                    ylimits = NULL, legendPos = "topleft", makePct = FALSE, 
                                    xlab = NULL, ylab = NULL, win_fraction = 0.01){
  ret = coredata(x)
  x = ret[, 2] - ret[, 3]
  y = ret[, 1] - ret[, 3]
  if (makePct) {
    x = x * 100
    y = y * 100
  }
  
  x_win <- winsorize_1D(x, fraction = win_fraction)
  
  fit.ls = lm(y ~ x)
  x = fit.ls$model$x
  y = fit.ls$model$y
  
  fit.ls.win <- lm(y ~ x_win)
  
  plot(x, y, xlab = xlab, ylab = ylab, 
       type = "n", ylim = ylimits, main = mainText, cex.main = 1.2, 
       cex.lab = 1.2)
  abline(fit.ls, col = "red", lty = 2, lwd = 2)
  abline(fit.ls.win, col = "blue", lty = 2, lwd = 2)
  
  ids = which((x - x_win) != 0)
  if (length(ids) == 0) {
    points(x, y, pch = 20)
  }
  else {
    points(x[-ids], y[-ids], pch = 19)
    points(x[ids], y[ids], pch = 1, cex = 2)
    points(x_win[ids], y[ids], pch = 2, cex = 2)
  }
  
  
  legend(x = legendPos, legend = as.expression(c(bquote("LS Win   "
                                                        ~ hat(f) == .(round(summary(fit.ls.win)$coefficients[2, 1], 2))
                                                        ~ "(" ~ .(round(summary(fit.ls.win)$coefficients[2, 2], 2)) ~ ")"),
                                                 bquote("LS       " ~ hat(f) == .(round(summary(fit.ls)$coefficients[2,1], 2))
                                                        ~ "(" ~ .(round(summary(fit.ls)$coefficients[2, 2], 2)) ~ ")"))), 
         lty = c(2, 2), lwd = c(2, 2), col = c("blue", "red"), bty = "n", cex = 1.2)
  grid()
}

t_stat_Andrews <- function(x){mean(x)/sqrt(lrvar(x, type = 'Andrews'))}

winsorize_1D <- function(x, fraction = 0.1)
{
  n <- length(x)
  lo <- ceiling(n * fraction) + 1
  hi <- n + 1 - lo
  x_sorted <- sort(x)
  x[x <= x_sorted[lo]] <- x_sorted[lo]
  x[x >= x_sorted[hi]] <- x_sorted[hi]
  return(x)
}

winsorize_dt <- function(dt, col_names, fraction = 0.1){
  win_dt <- copy(dt)
  for(i in 1:length(col_names)){
    eval(parse(text = paste0('win_dt[, ', col_names[i], ' := winsorize_1D(', 
                             col_names[i], ', fraction = fraction), by = ',"'", 'Date', "'", ']')))
  }
  return(win_dt)
}

fitReturnsToFF3model <- function(returns,datFF,digits = 2, title = NULL, QQplots = FALSE)
{
  reg <- cbind(returns,datFF)
  names(reg) <- c("RET",names(reg)[2:4])
  #tsPlotMP(reg)
  regdf <- as.data.frame(reg)
  fitLS <- lm(RET ~ .,data = regdf)
  fitRob <- RobStatTM::lmrobdetMM(RET~.,data = regdf)
  fitRobLS <- fit.models(fitLS,fitRob)
  if(QQplots){
    sideBySideQQPlot(fitRobLS,fun = residuals,main = title,xlab = "Standard Normal Quantiles",
                     ylab = "Ordered Residuals")
  }
  tLS = sapply(summary(fitLS)$coefficients[, 3], function(x) paste("(", round(x,1), ")", sep=""))
  LS <- c(paste0(round(coef(fitLS), digits), tLS), round(summary(fitLS)$adj.r.squared, digits))
  tRob = sapply(summary(fitRob)$coefficients[, 3], function(x) paste("(", round(x,1), ")", sep=""))
  Robust <- c(paste0(round(coef(fitRob),digits), tRob), round(summary(fitRob)$adj.r.squared, digits))
  LSRobFit <- data.frame(rbind(LS,Robust))
  names(LSRobFit) <- c("Alpha", names(reg)[2:4], "AdjRSQ")
  test <- lsRobTestMM(fitRob)
  MODEL <- round(test$full$p.value,3)
  pvalCoefs <- round(t(test$coefs[,6]),3)
  pvals <- data.frame(cbind(MODEL,pvalCoefs))
  row.names(pvals) <- "p-Values"
  list("LSRobfit" = LSRobFit,"pvalsCompare" = pvals)
}

fitReturnsToFF4model <- function(returns,datFF,digits = 2, title = NULL, QQplots = FALSE)
{
  reg <- cbind(returns,datFF)
  names(reg) <- c("RET",names(reg)[2:5])
  #tsPlotMP(reg)
  regdf <- as.data.frame(reg)
  fitLS <- lm(RET ~ .,data = regdf)
  fitRob <- RobStatTM::lmrobdetMM(RET~.,data = regdf)
  fitRobLS <- fit.models(fitLS,fitRob)
  if(QQplots){
    sideBySideQQPlot(fitRobLS,fun = residuals,xlab = "Standard Normal Quantiles",
                     pch = 16,col = "black",cex = .5, ylab = "Ordered Residuals")
  }
  tLS = sapply(summary(fitLS)$coefficients[, 3], function(x) paste("(", round(x,1), ")", sep=""))
  LS <- c(paste0(round(coef(fitLS), digits), tLS), round(summary(fitLS)$adj.r.squared, digits))
  tRob = sapply(summary(fitRob)$coefficients[, 3], function(x) paste("(", round(x,1), ")", sep=""))
  Robust <- c(paste0(round(coef(fitRob),digits), tRob), round(summary(fitRob)$adj.r.squared, digits))
  LSRobFit <- data.frame(rbind(LS,Robust))
  names(LSRobFit) <- c("Alpha", names(reg)[2:5], "AdjRSQ")
  test <- lsRobTestMM(fitRob)
  MODEL <- round(test$full$p.value,3)
  pvalCoefs <- round(t(test$coefs[,6]),3)
  pvals <- data.frame(cbind(MODEL,pvalCoefs))
  row.names(pvals) <- "p-Values"
  list("LSRobfit" = LSRobFit,"pvalsCompare" = pvals)
}

plotLSandRobustWithHuber = function(x,mainText = NULL, ylimits = NULL,
                                    legendPos = "topleft",goodOutlier = F, 
                                    makePct = FALSE)
{
  ret = coredata(x)
  x = ret[,2]-ret[,3]
  y = ret[,1]-ret[,3]
  if (makePct) {
    x = x * 100
    y = y * 100
  }
  control <- RobStatTM::lmrobdet.control(efficiency=0.95,family="mopt")
  fit.mOpt = RobStatTM::lmrobdetMM(y~x, control=control)
  fit.ls = lm(y~x)
  plot(x,y, xlab="Market Returns", ylab="Asset Returns", type="n",
       ylim = ylimits, main = mainText, cex.main =1.5, cex.lab=1.5)
  fit.huber = MASS::rlm(y~x)
  abline(fit.mOpt, col="black", lty=1, lwd=2)
  abline(fit.ls, col="red", lty=2, lwd=2)
  abline(fit.huber,col ="blue", lty = 5, lwd = 2)
  abline(fit.mOpt$coef[1]+3*fit.mOpt$scale.S, fit.mOpt$coef[2], lty=3, col="black")
  abline(fit.mOpt$coef[1]-3*fit.mOpt$scale.S, fit.mOpt$coef[2], lty=3, col="black")
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
                                  bquote("  Huber  " ~ hat(beta) == .(round(summary(fit.huber)$coefficients[2, 1], 2)) ~
                                           "(" ~ .(round(summary(fit.huber)$coefficients[2, 2], 2)) ~ ")"),
                                  bquote("  LS       " ~ hat(beta) == .(round(summary(fit.ls)$coefficients[2, 1], 2)) ~
                                           "(" ~ .(round(summary(fit.ls)$coefficients[2, 2], 2)) ~ ")"))),
         lty=c(1,2,5), col=c("black","blue", "red"), bty="n", cex=1.5 )
  if(goodOutlier) {
    id = which(x <=-20)
    print(id)
    arrows(x[id]+1, y[id]+11, x[id]+0.1, y[id]+1, angle=15, length=0.1)
    text(x[id]+1, y[id]+12.5, labels="Oct. 20 1987", cex=1.2)}
}

lsRobTestMM <- function(object, test = c("T2", "T1"), ...)
{
  # require(RobStatTM)
  test <- match.arg(test)
  
  # family: one of bisquare, opt and mopt
  family <- object$control$family
  tune <- object$control$tuning.psi
  
  # the prefix probably can be removed when added into RobStatTM
  eff = RobStatTM:::computeGaussianEfficiencyFromFamily(family, tune)
  
  if(is.null(object$weights)) {
    LS <- lm(formula(object$terms), data = object$model)
  } else {
    LS <- lm(formula(object$terms), data = object$model, weights = object$weights)
  }
  
  rmm <- residuals(object)
  rls <- residuals(LS)
  rob.sigma <- object$scale
  
  # require(robust) # probably not needed anymore, check later
  # tune <- lmRob.effvy(eff, ipsi) # control$tuning.psi
  # rw <- object$T.M.weights 
  
  rw <- object$rweights
  
  X <- model.matrix(object)
  n <- nrow(X)
  p <- ncol(X)
  
  if (!is.null(object$weights)) {
    X <- X * sqrt(object$weights)
  }
  
  V <- (t(rw * X) %*% X) / sum(rw) 
  V.inv <- solve(V)
  
  if(test == "T1") {
    d <- mean(rhoprime2(rmm/rob.sigma, family=family,cc=tune))
    tau <- n * mean( (rhoprime(rmm/rob.sigma, family=family,cc=tune)/d)^2 ) / (n - p)
    mat <- (1 - eff)/n * tau * rob.sigma^2 * V.inv 
  }
  
  if(test == "T2") {
    d <- mean(rhoprime2(rmm/rob.sigma, family=family,cc=tune))
    delta2 <- mean( (rls - (rob.sigma * rhoprime(rmm/rob.sigma, family=family,cc=tune)) / d)^2 )
    mat <- delta2 / n * V.inv
  }
  
  brob <- coef(object)
  coef.names <- names(brob)
  bls <- coef(LS)
  x <- bls - brob
  
  if(attributes(object$terms)$intercept) {
    brob <- brob[-1]
    bls <- bls[-1]
    x <- x[-1]
    mat <- mat[-1, -1, drop = FALSE]
    coef.names <- coef.names[-1]
  }
  
  se <- sqrt(diag(mat))
  uniV <- x / se
  coefs <- cbind(bls, brob, x, se, uniV, 2*pnorm(-abs(uniV)))
  dimnames(coefs) <- list(coef.names, c("LS", "Robust", "Delta", "Std. Error", "Stat", "p-value"))
  T <- drop(t(x) %*% solve(mat) %*% x)
  
  ans <- list(coefs = coefs,
              full = list(stat = T, df = length(x), p.value = 1 - pchisq(T, length(x))),
              test = test,
              efficiency = eff)
  
  # class here is not changed yet, also the postfix in the name of print
  oldClass(ans) <- "lsRobTest"
  ans
}

print.lsRobTest <- function(x, digits = 4, ...)
{
  cat("Test for least squares bias\n")
  if(x$test == "T1")
    cat("H0: normal regression error distribution\n")
  if(x$test == "T2")
    cat("H0: composite normal/non-normal regression error distribution\n")
  
  cat("\n")
  cat("Individual coefficient tests:\n")
  print(format(as.data.frame(x$coefs), digits = digits, ...))
  cat("\n")
  cat("Joint test for bias:\n")
  cat("Test statistic: ")
  cat(format(x$full$stat, digits = digits, ...))
  cat(" on ")
  cat(format(x$full$df, digits = digits, ...))
  cat(" DF, p-value: ")
  cat(format(x$full$p.value, digits = digits, ...))
  cat("\n")
  
  invisible(x)
}

############################################################

# Load the following packages needed for parts of the code below
library(PCRA)
library(data.table)
library(facmodCS)
library(xts)
library(RobStatTM)
library(fit.models)

# The following packages are loaded in the EXHIBITS where they are
# needed:  optimalRhoPsi, sandwhich, MASS


## EXHIBIT 1

y <- invensysEPS
x = seq(1984,2000,1)
plot(x,y,pch = 19, xlab ="YEAR",ylab = "EARNINGS PER SHARE", ylim =c(-0.09,0.21),
     main = "", cex = 1.0)
ctrl=RobStatTM::lmrobdet.control(efficiency=0.99,family="mopt")
fitMM = RobStatTM::lmrobdetMM(y~x,control = ctrl)
fitLS = lm(y~x)
# Add lines
abline(fitMM,lwd = 1.0)
abline(fitLS,lty = "longdash", lwd = 1.0, col = "black")
# Add text
text(1996,0.07,"LEAST SQUARES",cex = 1.1,col = "black")
text(1991,0.105,"ROBUST",cex = 1.1)


## EXHIBIT 2
 
# Huber 95% Efficiency Tuning Constant
cc <- 1.345
# Huber rho function
rhoHuber = function(x, cc = cc)
{rho = ifelse(abs(x/cc) < 1, 0.5*x^2, cc*abs(x) -  0.5*cc^2) 
return(rho)
}

# Get mOpt rhoMax
library(optimalRhoPsi)
ccModOpt <- computeTuningPsi_modOpt(0.95)
rhoMax <- rho_modOpt(3, cc = ccModOpt)
par(mfrow = c(1,2))
# Plot Huber rho
x <- seq(-5, 5, 0.01)
ylim <- c(0, 2)
ylab <- "rho(x)"

# Plot Huber rho
plot(x,rhoHuber(x, cc)/rhoMax, ylim = ylim, ylab = ylab, 
     type = "l", cex.lab = 1.3, main = "Huber Rho Function")
abline(h = 0, lty = "dotted", lwd = 1.3)
sub = "(a) Huber Rho Function"

# Plot mOpt rho
ccModOpt <- computeTuningPsi_modOpt(0.95)
rhoMax <- rho_modOpt(3, cc = ccModOpt)
plot(x, rho_modOpt(x, cc = ccModOpt)/rhoMax, ylim = ylim, ylab = ylab, 
	 type = "l", cex.lab = 1.3, main = "mOpt Rho Function")
sub = "(a) mOpt Rho Function"
par(mfrow = c(1,1))


## EXHIBIT 3
ccopt <- computeTuningPsi_modOpt(0.95)
x <- seq(-4.5, 4.5, by = 0.001)
plot(x, wgt_modOpt(x, cc = ccopt), type = "l", ylim = c(0,1.1),
     xlab = "x", ylab = "w(x)",cex = 1.5,cex.lab = 1.5)
abline(h = 1.0, lty = "dotted")


# Load data needed
stocks_factors <- selectCRSPandSPGMI(dateRange = c("1993-01-31", "2015-12-31"), 
                                     stockItems = c("Date", "TickerLast", "CapGroupLast", "Return"), 
                                     factorItems = c('BP', 'EP'), outputType = 'data.table') 


## EXHIBIT 4 (takes about 15 seconds on a fast laptop)
# Fit LS and mOpt single factor BP and EP models
fm_ls_BP <- fitFfm(data = stocks_factors, asset.var = "TickerLast", ret.var = "Return", date.var = "Date", 
                   exposure.vars = 'BP', fit.method = 'LS', addIntercept = TRUE) 
fm_rob_BP <- fitFfm(data = stocks_factors, asset.var = "TickerLast", ret.var = "Return", date.var = "Date", 
                    exposure.vars = 'BP', fit.method = 'Rob', addIntercept = TRUE) 
fm_ls_EP <- fitFfm(data = stocks_factors, asset.var = "TickerLast", ret.var = "Return", date.var = "Date", 
                   exposure.vars = 'EP', fit.method = 'LS', addIntercept = TRUE) 
fm_rob_EP <- fitFfm(data = stocks_factors, asset.var = "TickerLast", ret.var = "Return", date.var = "Date", 
                    exposure.vars = 'EP', fit.method = 'Rob', addIntercept = TRUE) 

# Extract factor returns for above models
# Simplify the BP.Diff and EP.Diff calculations
BP.LS <- fm_ls_BP$factor.returns[,2]
BP.mOpt <- fm_rob_BP$factor.returns[,2]
BP.Diff <- fm_rob_BP$factor.returns[,2] - fm_ls_BP$factor.returns[,2]
EP.LS <- fm_ls_EP$factor.returns[,2]
EP.mOpt <- fm_rob_EP$factor.returns[,2]
EP.Diff <- fm_rob_EP$factor.returns[,2] - fm_ls_BP$factor.returns[,2]

# Trellis Plot Two column format
BPandEPall <- cbind(BP.LS, EP.LS, BP.mOpt, EP.mOpt, BP.Diff, EP.Diff)
names(BPandEPall) <- c("BP LS", "EP LS", "BP mOpt", "EP mOpt", 
                       "BP Diff", "EP Diff")
tsPlotMP(BPandEPall, yname = "FACTOR RETURNS", scaleType = "free",
         layout = c(2,3))


## EXHIBIT 5
# Both plots in this exhibit use the data loaded in EXHIBIT 4

# BP plot
# Compute the summary statistics
ls_reg_BP <- summary(fm_ls_BP)
rob_reg_BP <- summary(fm_rob_BP)

# set the date to be 2009-08-31 and extract the summary statistics and the returns data
id <- 199
ls_reg_BP$sum.list[[id]] 
rob_reg_BP$sum.list[[id]]
date_max <- index(fm_ls_BP$factor.returns[id])
date_max_m1 <- index(fm_ls_BP$factor.returns[id-1])
y <- as.matrix(stocks_factors[Date == date_max, c('Return')])
x <- as.matrix(stocks_factors[Date == date_max_m1, get('BP')])

# Produce the plot by calling one of the plotting functions defined at the beginning of the script
to_plot_BP <- data.frame(y = y, x = x, rf = rep(0, length(x))) 
plotLSandRobustSFM_SS(to_plot_BP, xlab = 'BP', ylab = 'Returns', mainText = paste0('BP, scatter plot as of ', date_max)) 


# EP plot
# Compute the summary statistics
ls_reg_EP <- summary(fm_ls_EP)
rob_reg_EP <- summary(fm_rob_EP)

# set the date to be 2008-01-31 and extract the summary statistics and the returns data
id <- 180
ls_reg_EP$sum.list[[id]] 
rob_reg_EP$sum.list[[id]]
date_max <- index(fm_ls_EP$factor.returns[id])
date_max_m1 <- index(fm_ls_EP$factor.returns[id-1])
y <- as.matrix(stocks_factors[Date == date_max, c('Return')])
x <- as.matrix(stocks_factors[Date == date_max_m1, get('EP')])

# Produce the plot by calling one of the plotting functions defined at the beginning of the script
to_plot_EP <- data.frame(y = y, x = x, rf = rep(0, length(x))) 
plotLSandRobustSFM_SS(to_plot_EP, xlab = 'EP', ylab = 'Returns', 
                      mainText = paste0('EP, scatter plot as of ', date_max), legendPos = 'bottomleft') 


## EXHIBIT 6
# This plot uses the same data as the EP plot in EXHIBIT 5
# Produce the plot by calling one of the plotting functions defined at the beginning of the script
plotLSandRobustSFM_win(to_plot_EP, xlab = 'EP', ylab = 'Returns',
                       mainText = paste0('EP, scatter plot as of ', date_max), legendPos = 'bottomleft')


## EXHIBIT 7
# This table uses the data loaded for EXHIBIT 4
library(sandwich)

# split the data into pre- and post-2007
post_2007 <- index(fm_ls_BP$factor.returns) >= '2007-01-31' 
pre_2007 <- index(fm_ls_BP$factor.returns) < '2007-01-31'

# Winsorize the data by calling the winsorize_dt function defined at the beginning of the script
win_stocks_factors_only_1prc <- winsorize_dt(stocks_factors, c('BP', 'EP'), fraction = 0.01) 
fm_ls_BP_win1prc <- fitFfm(data = win_stocks_factors_only_1prc, asset.var = "TickerLast", ret.var = "Return", date.var = "Date", 
                           exposure.vars = 'BP', fit.method = 'LS', addIntercept = TRUE) 
fm_ls_EP_win1prc <- fitFfm(data = win_stocks_factors_only_1prc, asset.var = "TickerLast", ret.var = "Return", date.var = "Date", 
                           exposure.vars = 'EP', fit.method = 'LS', addIntercept = TRUE)

# build Panel C of the table with the help of the t_stat_Andrews function defined at the beginning of the script
t_stats_post_2007 <-rbind(c(t_stat_Andrews(fm_rob_BP$factor.returns[post_2007, 2]),
                            t_stat_Andrews(fm_ls_BP$factor.returns[post_2007, 2]),
                            t_stat_Andrews(fm_ls_BP_win1prc$factor.returns[post_2007, 2])),
                          c(t_stat_Andrews(fm_rob_EP$factor.returns[post_2007, 2]),
                            t_stat_Andrews(fm_ls_EP$factor.returns[post_2007, 2]),
                            t_stat_Andrews(fm_ls_EP_win1prc$factor.returns[post_2007, 2])))

# build Panel B of the table with the help of the t_stat_Andrews function defined at the beginning of the script
t_stats_pre_2007 <- rbind(c(t_stat_Andrews(fm_rob_BP$factor.returns[pre_2007, 2]),
                            t_stat_Andrews(fm_ls_BP$factor.returns[pre_2007, 2]),
                            t_stat_Andrews(fm_ls_BP_win1prc$factor.returns[pre_2007, 2])),
                          c(t_stat_Andrews(fm_rob_EP$factor.returns[pre_2007, 2]),
                            t_stat_Andrews(fm_ls_EP$factor.returns[pre_2007, 2]),
                            t_stat_Andrews(fm_ls_EP_win1prc$factor.returns[pre_2007, 2])))

# build Panel A of the table with the help of the t_stat_Andrews function defined at the beginning of the script
t_stats_full <- rbind(c(t_stat_Andrews(fm_rob_BP$factor.returns[, 2]),
                        t_stat_Andrews(fm_ls_BP$factor.returns[, 2]),
                        t_stat_Andrews(fm_ls_BP_win1prc$factor.returns[, 2])),
                      c(t_stat_Andrews(fm_rob_EP$factor.returns[, 2]),
                        t_stat_Andrews(fm_ls_EP$factor.returns[, 2]),
                        t_stat_Andrews(fm_ls_EP_win1prc$factor.returns[, 2])))

# put the panels together
exhibit_7 <- rbind(t_stats_full, t_stats_pre_2007, t_stats_post_2007)
colnames(exhibit_7) <- c('mOpt', 'LS', 'LS Win 1%')
rownames(exhibit_7) <- c('BP', 'EP', 'BP', 'EP', 'BP', 'EP') 
exhibit_7


## EXHIBIT 8  (takes about 15 seconds on a fast laptop)
# # Load data needed
stocks_factors_mvt <- selectCRSPandSPGMI(dateRange = c("1993-01-31", "2015-12-31"), 
                                         stockItems = c("Date", "TickerLast", "CapGroupLast", "Return"), 
                                         factorItems = c('Beta60M', "BP", "LogMktCap", 'EP'), outputType = 'data.table') 

# Fit LS and mOpt FF3 models
fm_ls_ff3_mvt <- fitFfm(data = stocks_factors_mvt, asset.var = "TickerLast", ret.var = "Return", date.var = "Date", 
                        exposure.vars = c('Beta60M', "BP", 'EP', 'LogMktCap'), fit.method = 'LS', addIntercept = TRUE)
fm_rob_ff3_mvt <- fitFfm(data = stocks_factors_mvt, asset.var = "TickerLast", ret.var = "Return", date.var = "Date", 
                         exposure.vars = c('Beta60M', "BP", 'EP', 'LogMktCap'), fit.method = 'Rob', addIntercept = TRUE)

# Extract factor returns 
Beta.LSmOptDiff <- fm_rob_ff3_mvt$factor.returns$Beta60M - fm_ls_ff3_mvt$factor.returns$Beta60M
EP.LSmOptDiff <- fm_rob_ff3_mvt$factor.returns$EP - fm_ls_ff3_mvt$factor.returns$EP
BP.LSmOptDiff <- fm_rob_ff3_mvt$factor.returns$BP - fm_ls_ff3_mvt$factor.returns$BP
Size.LSmOptDiff <- fm_rob_ff3_mvt$factor.returns$LogMktCap - fm_ls_ff3_mvt$factor.returns$LogMktCap
facReturns4 <- cbind(Beta.LSmOptDiff, BP.LSmOptDiff, 
                     EP.LSmOptDiff, Size.LSmOptDiff)
names(facReturns4) <- c("Beta Diffs", "BP Diffs", "EP Diffs", 
                        "Size Diffs")

# plot
tsPlotMP(facReturns4, yname = "FACTOR RETURNS", scaleType = "free")


## EXHIBIT 9
# This table relies on the data loaded for EXHIBIT 8
# Winsorize the data by using winsorize_dt loaded at the beginning of the script
win_factors_1prc <- winsorize_dt(stocks_factors_mvt, c('Beta60M', "BP", 'EP', 'LogMktCap'), fraction = 0.01) 

fm_ls_ff3_mvt_win <- fitFfm(data = win_factors_1prc, asset.var = "TickerLast", ret.var = "Return", date.var = "Date", 
                            exposure.vars = c('Beta60M', "BP", 'EP', 'LogMktCap'), fit.method = 'LS', addIntercept = TRUE) 

# Build the full-sample panel
t_stats_full_period <- cbind(apply(fm_rob_ff3_mvt$factor.returns, 2, t_stat_Andrews), 
                             apply(fm_ls_ff3_mvt$factor.returns, 2, t_stat_Andrews), 
                             apply(fm_ls_ff3_mvt_win$factor.returns, 2, t_stat_Andrews)) 

# split the data into pre- and post-2007
post_2007 <- index(fm_ls_ff3_mvt$factor.returns) >= '2007-01-31' 
prior_2007 <- index(fm_ls_ff3_mvt$factor.returns) < '2007-01-31'

# Build the post-2007 panel
t_stats_post_2007 <- cbind(apply(fm_rob_ff3_mvt$factor.returns[post_2007, ], 2, t_stat_Andrews),
                           apply(fm_ls_ff3_mvt$factor.returns[post_2007], 2, t_stat_Andrews),
                           apply(fm_ls_ff3_mvt_win$factor.returns[post_2007], 2, t_stat_Andrews)) 

# Build the pre-2007 panel
t_stats_pre_2007 <- cbind(apply(fm_rob_ff3_mvt$factor.returns[prior_2007, ], 2, t_stat_Andrews),
                          apply(fm_ls_ff3_mvt$factor.returns[prior_2007], 2, t_stat_Andrews),
                          apply(fm_ls_ff3_mvt_win$factor.returns[prior_2007], 2, t_stat_Andrews)) 

# put the panels together
exhibit_9 <- cbind(t_stats_full_period, t_stats_pre_2007, t_stats_post_2007) 
colnames(exhibit_9) <- c('mOpt', 'LS', 'LSWin1prc', 'mOpt', 'LS', 'LSWin1prc', 'mOpt', 'LS', 'LSWin1prc') 
rownames(exhibit_9) <- c('Alpha', 'Beta', 'BP', 'EP', 'Size')
exhibit_9


## EXHIBIT 10
par(mfrow = c(2,2))
plotLSandRobustSFM(retEDS,mainText = "Stock EDS 2013-2014",
                legendPos = "bottomright")
plotLSandRobustSFM(retWTS,mainText = "Stock WTS 2009-2010")
plotLSandRobustSFM(retOFG,mainText = "Stock OFG 2007-2008")
plotLSandRobustSFM(retDD, mainText = "Stock DD 1986-1987")
par(mfrow = c(1,1))


## EXHIBIT 11  Not reproducible. See JAM paper by copy/pasting the URL
## https://link.springer.com/content/pdf/10.1057/s41260-022-00258-0.pdf
## in your browser.


## EXHIBIT 12
par(mfrow = c(1,2))
plotLSandRobustWithHuber(retPSC,mainText = "Stock PSC 1987-1988")
plotLSandRobustWithHuber(retKBH,mainText = "Stock KBH 2007-2008")
par(mfrow = c(1,1))


## EXHIBIT 13
datFF3 <- datFF3W["2008-01-01/2008-12-31"] # Short2
names(datFF3)[1] <- "MKT"
datFFC4 <- datFF4W["2008-01-01/2008-12-31"] # Short2
names(datFFC4)[c(1,4)] <- c("MKT","MOM")
retFNB <- retFNB["2008-01-01/2008-12-31"]
datFFC4.df <- as.data.frame(datFFC4)

# Create regDatFF3.df
regDatFF3 <- cbind(retFNB,datFF3)
names(regDatFF3) <- c("FNB",names(regDatFF3)[2:4])
regDatFF3.df <- as.data.frame(regDatFF3)

# Create regDatFFC4.df
regDatFFC4 <- cbind(retFNB,datFFC4)
names(regDatFFC4) <- c("FNB",names(regDatFFC4)[2:5])
regDatFFC4.df <- as.data.frame(regDatFFC4)

# Make pairwise scatter plots
pairs(regDatFFC4.df, pch = 16)


## EXHIBIT 14
# The next line uses the fit.models package
fmclass.add.class("lmfm","lmrobdetMM")
datFF3 <- datFF3W["2008-01-01/2008-12-31",]
names(datFF3)[1] <- "MKT"
datFF4 <- datFF4W["2008-01-01/2008-12-31",]
names(datFF4)[c(1,4)] <- c("MKT","MOM")
title <- "Robust mOpt and LS FF3 fit of FNB returns"
fitFF3 <- fitReturnsToFF3model(retFNB,datFF3,title = title)
title <- "Robust mOpt and LS FFC4 fit of FNB returns"
fitFF4 <- fitReturnsToFF4model(retFNB,datFF4,title = title)
fitFF3tab <- fitFF3$LSRobfit
fitFF3tab$MOM <- c("","")
fitFF3tab <- fitFF3tab[,c(1:4,6,5)]
row.names(fitFF3tab) <- c("FF3-LS","FF3-mOpt")
fitFF4tab <- fitFF4$LSRobfit
row.names(fitFF4tab) <- c("FFC4-LS","FFC4-mOpt")
blankRow <- rep("",6)
fitFF4tab5row <- rbind(fitFF3tab,blankRow,fitFF4tab)
row.names(fitFF4tab5row)[3] <- ""
fitFF4tab5row


## EXHIBIT 15
fitFFC4 <- RobStatTM::lmrobdetMM(FNB ~ ., data = regDatFFC4.df)
step.lmrobdetMM(fitFFC4) 
Factors4 <- c("ALL","MKT","SMB","HML","MOM")
RFPE4 <- c(0.221,0.235,0.234,0.217,0.230)
Factors3 <- c("ALL","MKT","SMB","MOM","")
RFPE3 <- c(0.217,0.248,0.230,0.260,"")
stepsRFPE <- data.frame(cbind(Factors4,RFPE4,Factors3,RFPE3))
headerText <- c("FULL MODEL"=2,"MKT+SMB+MOM"=2)
names(stepsRFPE) <- rep(c("Factor","RFPE"),2)
row.names(stepsRFPE) <- NULL
stepsRFPE


## EXHIBIT 16
## Need to reference code used for the AIC4, AIC values below
Factors4 <- c("ALL","MKT","SMB","HML","MOM")
AIC4 <- c(-294.6,-284.5,-292.1,-293.0,-296.5)
Factors3 <- c("ALL","MKT","SMB","HML","")
AIC3 <- c(-296.5,-282.6,-293.9,-292.4,"")
stepsAIC <- data.frame(cbind(Factors4,AIC4,Factors3,AIC3))
headerText <- c("FULL MODEL"=2,"MKT+SMB+HML"=2)
names(stepsAIC) <- rep(c("Factor","AIC"),2)
row.names(stepsAIC) <- NULL
stepsAIC


## EXHIBIT 17
wgtSHR <- function(d)
{
  q <- -1.944 + 1.728*d - 0.312*d^2 + 0.016*d^3
  if(d <= 4) {out <- 1}
  else
  {out <- ifelse(d <= 9, q, 0)}
  out
}

x <- seq(0, 11, 0.1)
nx <- length(x)
y <- rep(0,nx)
for(i in 1:nx) 
 {y[i] <- wgtSHR(x[i])}
plot(x,y, type = "l", ylab = "W-SHR(x)", lwd = 1.3)
abline(h = 0, lty = "dotted")


## EXHIBIT 18
stocksCRSPweekly <- getPCRAData("stocksCRSPweekly")
stockItems <- c("Date","TickerLast","CapGroupLast","Return")
dateRange <- c("2009-01-01","2009-12-31")
stocksDat <- selectCRSPandSPGMI("weekly",
                                dateRange = dateRange, 
                                stockItems = stockItems, 
                                factorItems = NULL, 
                                subsetType = "CapGroupLast",
                                subsetValues = "SmallCap", 
                                outputType = "xts")
k1 <- 101
k2 <- 104
stocks <- stocksDat[, c(k1:k2)]
tsPlotMP(100*stocks,scaleType = "free", type ="l",
         stripText.cex = .7, axis.cex = .7,color = "black")


## EXHIBIT 19
stocks.df <- data.frame(coredata(stocks))
pairs(100*stocks.df, pch = 20, cex = 1, cex.axis = 2, cex.labels = 2)


## EXHIBIT 20
classicFit <- covClassic(stocks.df)
set.seed(1024)
robFit <- covRob(stocks.df)
covClassicRob <- fit.models(classicFit, robFit)
ellipsesPlotPCRA.covfm(covClassicRob, which = 2)


## EXIBIT 21
muT <- c(1,1)
sdT <- c(1.5,3.0)
datCorT <- c(1,0.9,0.9,1)
rhoCorT <- matrix(datCorT,nrow = 2)
sigMatT <- diag(sdT)%*%rhoCorT%*%diag(sdT)
set.seed(60)
library(MASS) # For mvrnorm() below
set.seed(570000) 
retN <- mvrnorm(100, muT, sigMatT)

# Sample means, covariances, and correlations
mu <- apply(retN,2,mean)
sigMat <- cov(retN)
cor(retN)

# Inverse square-root of sample covariance
sigMatInv <- solve(sigMat)
sigMatInvSqrt <- tensr::mhalf(sigMatInv)

# Sample transform of returns 
z <- (retN-mu)%*%sigMatInvSqrt
apply(z,2,mean)
cor(z)

# Make plots
par(mfrow = c(1,2))
par(pty = "s")
plot(retN, pch = 16, xlab = "ret1(%)", xlim=c(-5.0,5.0), ylim=c(-8.0,8.0), cex.sub = 1.4,
     cex.lab = 1.2, cex = 0.9, sub = "Correlated Returns", ylab = "ret2(%)")
abline(h = 0, v = 0, lty="dotted")
plot(z, pch = 16, xlab = "z1(%)", ylab = "z2(%)", xlim = c(-5.0,5.0), ylim = c(-8.0,8.0), 
		cex.sub = 1.4, cex.lab = 1.2, cex = 1.1, cex.axis = 1.2, 
		sub = "Uncorrelated MD Transformed Returns")
abline(h=0, v=0, lty="dotted")
par(mfrow = c(1,1))


## EXHIBIT 22
ClassicMD <- covClassic(stocks.df)
set.seed(1024)
RobustMD <- covRob(stocks.df)
covClassicRob <- fit.models(ClassicMD, RobustMD)
plot(covClassicRob, which.plots = 1,cex = 0.5)


## EXHIBIT 23
# Get data
dateRange <- c("2009-01-01","2009-12-31")
stockItems <- c("Date", "TickerLast",
                "CapGroupLast", "Sector", "Return",
                "Ret13WkBill", "MktIndexCRSP")
factorItems <- c("BP", "EP", "CFROIC", "SEV", "FCFP", "EBITDAEV")
stocksDatAll <- selectCRSPandSPGMI(periodicity = "monthly",
                                dateRange = dateRange, 
                                stockItems = stockItems,
                                factorItems = factorItems, 
                                outputType = "data.table")
names(stocksDatAll)
stocksDat12 <- stocksDatAll[,c(2,8:13)]
stocksDat1 <- stocksDat12[Date == "2009-01-31"]
fac6 <- stocksDat1[, 2:7]

# Make pairwise scatterplots
stocks.df <- data.frame(coredata(stocks))
pairs(100*fac6, pch = 20, cex = 1, cex.axis = 2, cex.labels = 2)


## EXHIBIT 24
classicFit <- covClassic(fac6)
set.seed(1024)
robFit <- covRob(fac6)
covClassicRob <- fit.models(classicFit, robFit)
ellipsesPlotPCRA.covfm(covClassicRob, which = 2)


## EXHIBIT 25
ClassicMD <- covClassic(fac6)
set.seed(1024)
RobustMD <- covRob(fac6)
covClassicRob <- fit.models(ClassicMD, RobustMD)
plot(covClassicRob, which.plots = 1,cex = 0.5)

