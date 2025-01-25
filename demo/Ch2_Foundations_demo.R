######
# You first need to install all the R packages listed as arguments to the
# library(packageName) functions below in lines 20 to 32, as follows:
# 
# Install the devtools package using the RStudio Tools/Install Packages
# drop-down menu, or with install.packages("devtools") in the RStudio Console.
#
# Next install PCRA with the devtools function at the RStudio Console with:
# devtools::install_github("robustport/PCRA")
#
# Then install all the other packages below, except the optimalRhoPsi package,
# using the RStudio Tools/Install Packages drop-down menu.
#
# Finally, install the optimalRhoPsi package at the RStudio Console with
# devtools::install_github("kjellpk/optimalRhoPsi")
#
# NOW YOU ARE READY TO RUN THE CH 2 REPRODUCIBILITY CODE BELOW
######

library(PCRA)          ## Install with devtools as above
library(data.table)
library(xts)
library(PerformanceAnalytics) 
library(PortfolioAnalytics)
library(foreach)
library(CVXR)
library(RPESE)
library(RPEIF)
library(ggplot2)
library(dplyr)
library(RobStatTM)
library(optimalRhoPsi)  ## Install with devtools as above


##  Table 2.1

# Largecaps
stockItems <- c("Date","TickerLast","CapGroupLast","Return")
dateRange <- c("1993-01-31","2015-12-31")
returns <- selectCRSPandSPGMI("monthly",dateRange = dateRange, stockItems =
                                stockItems, factorItems = NULL, 
                              subsetType = "CapGroupLast",
                              subsetValues = "LargeCap", outputType = "xts")

ret <- coredata(returns) # Package xts
n <- ncol(ret)
acfLarge <- rep(0,n)
for(i in 1:n){
  acfLarge[i] <- acf(ret[,i], lag.max = 1, 
                     plot = FALSE)$acf[2]
}
muLarge <- mean(acfLarge)
sdLarge <- sd(acfLarge)

# Midcaps
returns <- selectCRSPandSPGMI("monthly",dateRange = dateRange, stockItems =
                                stockItems, factorItems = NULL, subsetType = "CapGroupLast",
                              subsetValues = "MidCap", outputType = "xts")
ret <- coredata(returns) # Package xts
n <- ncol(ret)
acfMid <- rep(0,n)
for(i in 1:n){
  acfMid[i] <- acf(ret[,i], lag.max = 1, 
                   plot = FALSE)$acf[2]
}
muMid <- mean(acfMid)
sdMid <- sd(acfMid)

## Smallcaps
returns <- selectCRSPandSPGMI("monthly",dateRange = dateRange, stockItems =
                                stockItems, factorItems = NULL, subsetType = "CapGroupLast",
                              subsetValues = "SmallCap", outputType = "xts")
ret <- coredata(returns) # Package xts
n <- ncol(ret)
acfSmall <- rep(0,n)
for(i in 1:n){
  acfSmall[i] <- acf(ret[,i], lag.max = 1, 
                     plot = FALSE)$acf[2]
}
muSmall <- mean(acfSmall)
sdSmall <- sd(acfSmall)

## Microcaps
returns <- selectCRSPandSPGMI("monthly",dateRange = dateRange, stockItems =
                                stockItems, factorItems = NULL, subsetType = "CapGroupLast",
                              subsetValues = "MicroCap", outputType = "xts")
ret <- coredata(returns) # Package xts
n <- ncol(ret)
acfMicro <- rep(0,n)
for(i in 1:n){
  acfMicro[i] <- acf(ret[,i], lag.max = 1, 
                     plot = FALSE)$acf[2]
}

muMicro <- mean(acfMicro)
sdMicro <- sd(acfMicro)

dat <- cbind(c(muLarge, sdLarge), c(muMid, sdMid),
             c(muSmall, sdSmall), c(muMicro, sdMicro))
dat <- round(dat,3)
dat <- data.frame(dat)
names(dat) <- c("LargeCap","MidCap","SmallCap","MicroCap")
row.names(dat) <- c("  Mean Lag-1 Acf", 
                    "StdDev Lag-1 Acf")
dat


##  Figure 2.1

dat <- list(acfLarge, acfMid, acfSmall, acfMicro)
names(dat) <- c("87 LargeCaps","67 MidCaps","106 SmallCaps","34 MicroCaps")
boxplot(dat, varwidth = TRUE, col = "cyan")



##  Figure 2.2

returns <- PerformanceAnalytics::edhec
returns <- returns["2002-01-31/2019-12-31", -13]
names(returns) <- c("CA","CTA","DIST","EM","EMN","ED","FIA",
                    "GM","LSE","MA","RV","SS")
PCRA::tsPlotMP(returns)
# range(index(returns))
Ret <- coredata(returns)
n <- ncol(Ret)
acfRet <- rep(0,n)
for(i in 1:n){
  acfRet[i] <- acf(Ret[,i], lag.max = 1, 
                   plot = FALSE)$acf[2]
}
hist(acfRet, main = "EDHEC Hedge Fund Indexes",
     xlab = "Lag-1 ACF Values")


## Figure 2.3

# names(acfRet) <- names(returns)
# (names(sort(acfRet)[1:4]))
# delete "CTA", "GM", "SS", "EMN"

returns8 <- returns[ , c("CA","DIST","EM","ED",
                         "FIA","LSE","MA","RV")]
PCRA::tsPlotMP(returns8, yname = "RETURNS", 
               stripText.cex = 0.7, axis.cex = 0.7)



##  Figure 2.4

acf(returns8$EM, main = "EM", lag.max = 5)



##  Figure 2.5

muVol <- c(.20,.10,.15,.04)
wts <- seq(0,1,.01)
efront2Asset <- function(wts,rho,muVol = c(.20,.10,.15,.04))
{
  sigma1 <- muVol[1]
  mu1 <- muVol[2]
  sigma2 <- muVol[3]
  mu2 <- muVol[4]
  n <- length(wts)
  efront <- data.frame(matrix(rep(0,3*n),ncol = 3))
  names(efront) <- c("SIGMA","MU","WTS")
  w <- wts
  for(i in 1:n){
    mu <- w[i]*mu1 + (1-w[i])*mu2
    var <- w[i]^2*sigma1^2 + 2*w[i]*(1-w[i])*rho*sigma1*sigma2 + (1-w[i])^2*sigma2^2
    sigma <- sqrt(var)
    efront[i,] <- c(sigma,mu,w[i])
  }
  return(efront)
}
ef <- efront2Asset(wts,0,muVol = muVol)
gmv <- ef[ef$SIGMA == min(ef$SIGMA),]
xlab <- expression(sigma [P])
ylab <- expression(mu [P])
par(pty = "s")
plot(ef$SIGMA,ef$MU,type = "l", xlab = xlab, ylab = ylab,
     xlim=c(0,.25),ylim=c(0.03,.11),lwd = 2, cex.lab = 1.5)
points(muVol[c(1,3)], muVol[c(2,4)], pch = 19, cex = 1.3)
points(gmv,pch = 19, cex = 1.3)
text(.04,.10,expression(paste(rho, " = 0")),cex = 1.5)
text(0.12,.0616,adj = c(1,NA),"MinRisk   ",cex = 1.1)
text(0.13,.0616,adj = c(0,NA), "(.12, .0616)",cex = 1.1)
text(0.2,.1,adj = c(0,NA),"  (.20, .10)",cex = 1.1)
text(0.15,.04,adj = c(0,NA),"  (.15, .04)",cex = 1.1)


##  Figure 2.6

muVol <- c(.20,.10,.15,.04)
wts <- seq(0,1,.01)
ef <- efront2Asset(wts,0,muVol = muVol)
ef1 <- efront2Asset(wts,1,muVol = muVol)
ef2 <- efront2Asset(wts,-1,muVol = muVol)
gmv <- ef[ef$SIGMA == min(ef$SIGMA),]
gmv2 <- ef2[ef2$SIGMA == min(ef2$SIGMA),]
xlab <- expression(sigma [P])
ylab <- expression(mu [P])
par(pty = "s")
plot(ef$SIGMA,ef$MU,type = "l", xlab = xlab, ylab = ylab,
     xlim=c(0,.25),ylim=c(0.03,.11),lwd = 2, cex.lab = 1.5)
points(muVol[c(1,3)], muVol[c(2,4)], pch = 19, cex = 1.5)
points(gmv,pch = 19, cex = 1.3)
text(0.2,.1,adj = c(0,NA),"  (.20, .10)",cex = 1.1)
text(0.15,.04,adj = c(0,NA),"  (.15, .04)",cex = 1.1)
lines(ef1$SIGMA,ef1$MU,lty = 2,lwd = 2)
lines(ef2$SIGMA,ef2$MU,lty = 2,lwd = 2)
points(gmv2,pch = 19, cex = 1.3)
text(.12,.07,expression(paste(rho, " = 0 ")),adj = c(1,NA),cex = 1.5)
text(.02,.08,expression(paste(rho, " = -1  ")),adj = c(0,NA),cex = 1.5)
text(.18,.07,expression(paste(rho, " = +1 ")),adj = c(0,NA),cex = 1.5)


##   Figure 2.7

muVol <- c(.20,.10,.15,.04)
wts   <- seq(0,1,.01)
efLO  <- efront2Asset(wts,0,muVol = muVol)
wts   <- seq(1,1.25,.01)
efSS  <- efront2Asset(wts,0,muVol = muVol)
gmv   <- ef[ef$SIGMA == min(ef$SIGMA),]
maxMu <- ef[ef$MU == max(ef$MU),]
maxMuSS <- efSS[efSS$MU == max(efSS$MU),]
xlab  <- expression(sigma [P])
ylab  <- expression(mu [P])
par(pty = "s")
plot(efLO$SIGMA,efLO$MU,type = "l", xlab = xlab, ylab = ylab,
     xlim=c(0,.40),ylim=c(.02,.13),lwd = 2, cex.lab = 1.5)
lines(efSS$SIGMA,efSS$MU,lty = "dashed", lwd = 2)
points(gmv[1:2],pch = 19, cex = 1.3)
points(maxMu[1:2], pch = 19, cex = 1.3)
points(maxMuSS[1:2], pch = 19, cex = 1.3)
text(.04,.12,expression(paste(rho, " = 0")),cex = 1.5)
text(gmv[1:2],adj = c(0,NA),
     paste("  (",toString(round(gmv[1:2],2)),")"),cex = 1.1)
text(maxMu[1:2],adj = c(0,NA),
     paste("  (",toString(maxMu[1:2]),")"),cex = 1.1)
text(maxMuSS[1:2],adj = c(0,NA), 
     paste("  (",toString(round(maxMuSS[1:2],2)),")"), cex = 1.1)


##   Figure 2.8

volMu1 <- c(.20,.10)
volMu2 <- c(.15,.04)
volMu3 <- c(.10,.02)
names(volMu1) <- c("SIGMA","MU")
names(volMu2) <- c("SIGMA","MU")
names(volMu3) <- c("SIGMA","MU")
wts <- seq(0,1,.01)
ef1 <- efront2Asset(wts,0,muVol = c(volMu1,volMu2))
ef2 <- efront2Asset(wts,0,muVol = c(volMu1,volMu3))
ef3 <- efront2Asset(wts,0,muVol = c(volMu2,volMu3))
xlab <- expression(sigma [P])
ylab <- expression(mu [P])
par(pty = "s")
plot(ef1$SIGMA,ef1$MU,type = "l", xlab = xlab, ylab = ylab,
     xlim=c(0,.25),ylim=c(0,.11),lwd = 2, cex.lab = 1.5)
lines(ef2$SIGMA,ef2$MU,lty = 2, lwd = 2.0)
lines(ef3$SIGMA,ef3$MU,lty = 3, lwd = 2.0)
xy <- rbind(volMu1,volMu2,volMu3)
points(xy, pch = 19, cex = 1.3)
text(volMu1[1]+0.01,volMu1[2],adj = c(0,NA),toString(volMu1),cex = 1.1)
text(volMu2[1]+0.01,volMu2[2],adj = c(0,NA),toString(volMu2),cex = 1.1)
text(volMu3[1]+0.01,volMu3[2],adj = c(0,NA),toString(volMu3),cex = 1.1)
text(.04,.10,expression(paste(rho, " = 0")),cex = 1.5)


##  Figure 2.9

volMu1 <- c(.20,.10)
volMu2 <- c(.15,.04)
volMu3 <- c(.10,.02)
vol <- c(volMu1[1],volMu2[1],volMu3[1])
mu <- c(volMu1[2],volMu2[2],volMu3[2])
corrMat0 <- matrix(rep(0,9),nrow = 3)+ diag(rep(1,3))
covMat0 <- diag(vol)%*%corrMat0%*%diag(vol)
n <- 500
port <- matrix(rep(0,2*n),ncol = 2)
dimnames(port)[[2]] = c("SIG.P","MU.P")
wts = hitandrun::simplex.sample(3,n)$samples
for(i in 1:n) {
  x <- wts[i,] 
  port[i,1] <- sqrt(x%*%covMat0%*%x)
  port[i,1]
  port[i,2] <- x%*%mu
}
xlab <- expression(sigma [P])
ylab <- expression(mu [P])
plot(port[,1],port[,2],xlim = c(0,.25),ylim = c(0,.11),
     xlab = xlab, ylab = ylab, pch = 20, cex = .7, cex.lab = 1.5)
points(vol,mu, pch = 19, cex = 1.3)
text(volMu1[1]+0.01,volMu1[2],adj = c(0,NA),toString(volMu1),cex = 1.1)
text(volMu2[1]+0.01,volMu2[2],adj = c(0,NA),toString(volMu2),cex = 1.1)
text(volMu3[1]+0.01,volMu3[2],adj = c(0,NA),toString(volMu3),cex = 1.1)
text(.04,.10,expression(paste(rho, " = 0")),cex = 1.5)


##  Example 2.3

muRet = c(.10,.04,.02)
volRet = c(.20,.15,.10)
corrRet = diag(c(1,1,1))
PCRA::mathGmvMuCov(muRet,volRet,corrRet,digits = 3)


##  Figure 2.10

# There is currently no code for this Figure, and while
# IBM and XOM are in our CRSP data, GE is not.
# Will consider replacing GE with a stock in the CRSP data.


## Example 2.6 Figures 2.8 - 2.10 with the following steps
# Get xts object of 106 smallcap stocks, and the Market ("MktIndexCRSP") in
# stocksCRSP for 1997 - 2010, and use the third group of 10 of these
# to compute Gmv portfolios. Change name "MktIndexCRSP" to "Market".


## Figure 2.11

library(data.table)
stockItems <- c("Date","TickerLast","CapGroupLast","Return","MktIndexCRSP")
dateRange <- c("1997-01-31","2010-12-31")
stocksDat <- PCRA::selectCRSPandSPGMI("monthly",dateRange = dateRange, stockItems =
                                        stockItems, factorItems = NULL, subsetType = "CapGroupLast",
                                      subsetValues = "SmallCap", outputType = "xts")

returns10Mkt <- stocksDat[, c(21:30,107)]
names(returns10Mkt)[11] <- "Market"
tsPlotMP(returns10Mkt,scaleType = "free",layout = c(2,6),stripText.cex = .45,
         axis.cex = 0.4,lwd = 0.5)



##  Figures 2.12 and 2.13

# Load PortfolioAnalytics and related packages, and compute time
# series of GmvLO and GmvLS portfolio weights wtsGmvLO and wtsGmvLS

# Create GmvLS portfolio specs
returns <- returns10Mkt[,1:10]
Market  <- returns10Mkt[,11]
funds   <- colnames(returns)

pspec       <- portfolio.spec(assets=funds)
pspec.fi    <- add.constraint(pspec, type="full_investment")
pspec.gmvLS <- add.objective(pspec.fi, type="risk", name="var")

# Optimize Portfolio at Monthly Rebalancing and 5-Year Training
bt.gmvLS <- optimize.portfolio.rebalancing(returns, pspec.gmvLS,
                                           optimize_method="CVXR",
                                           rebalance_on="months",
                                           training_period=60,
                                           rolling_window=60,
                                           trace = TRUE)
# Extract time series of portfolio weights
wtsGmvLS <- extractWeights(bt.gmvLS)

# Compute rebalancing GmvLS arithmetic returns
GmvLS <- Return.rebalancing(returns,wtsGmvLS)

# Combine GmvLS and Market returns and plot their time series
ret.comb <- na.omit(merge.xts(GmvLS, Market, all=F))
names(ret.comb) <- c("GmvLS","Market")


# Figure 2.12
tsPlotMP(wtsGmvLS,layout = c(2,5),scaleType = "same",
         stripText.cex = 0.7, axis.cex = .7)


# Figure 2.13
tsPlotMP(ret.comb,scaleType = "same",stripText.cex = .7, axis.cex = .7)


##  Figure 2.14
# Compute cumulative gross portfolio returns

R <- ret.comb
geometric <- TRUE
c.xts <- if ( geometric ) {
  cumprod(1+R)
} else {
  1 + cumsum(R)
}

# Plot cumulative gross returns of GmvLS and Market portfolios
# Original code contributed by Peter Carl

p <- plot.xts(c.xts[,1], col="black", main = "Cumulative Returns",
              grid.ticks.lwd=1, grid.ticks.lty = "dotted", grid.ticks.on = "years",
              labels.col="grey20", cex.axis=0.8, format.labels = "%b\n%Y",
              ylim = c(min(c.xts), max(c.xts)))
p <- addSeries(c.xts[,2], on=1, lwd=2, col="darkred", lty="dashed")
p <- addLegend("topleft", on = 1,
               legend.names = names(c.xts),
               lty = c(1,2), lwd = rep(2, NCOL(c.xts)),
               col = c("black", "darkred"),
               bty = "o", box.col = "white",
               bg=rgb(t(col2rgb("white")), alpha = 200,
                      maxColorValue = 255) )
d.xts <- PerformanceAnalytics::Drawdowns(R)
p <- xts::addSeries(d.xts[,1], col="darkblue", lwd=2, main="Drawdown",
                    ylim = c(min(d.xts), 0) )
p <- xts::addSeries(d.xts[,2], on=2, lwd=2, col="darkred", lty="dashed")

# panel 1 and 2 ylim
## ylim1 <- c(p$Env$ylim[[2]][1], p$Env$ylim[[2]][2]) No longer works
## ylim2 <- c(p$Env$ylim[[4]][1], p$Env$ylim[[4]][2]) No longer works

ylim1 <- p$Env$panels[[1]]$ylim
ylim2 <- p$Env$panels[[2]]$ylim
ylim <- c(ylim1, ylim2)

# get longest drawdown dates for xts object
dt <- table.Drawdowns(R, top = 1) # just want to find the worst drawdown
dt2 <- t(dt[,c("From", "To")])
x <- as.vector(dt2[,NCOL(dt2)])
y <- as.xts(matrix(rep(ylim, length(x)),ncol=length(ylim), byrow=TRUE), 			
            order.by=as.Date(x))
i=1
p <- xts::addPolygon(y[i:(i+1),1:2], on=-1, col="lightgrey") # top panel
p <- xts::addPolygon(y[i:(i+1),3:4], on=-2, col="lightgrey") # lower panel
p


##  Table 2.2

dt2mat <- table.Drawdowns(R, top = 2) # find the worst two drawdowns
dt2mat[,4] <- round(dt2mat[,4],2)
dt2 <- data.frame(dt2mat)[,1:5]
names(dt2) <- c("Begin","Minimum","End","Depth","Months")
dt2 <- dt2[,c(1:3,5,4)]
dt2


## Figure 2.15

levg <- levgLongShort(wtsGmvLS)
plot.zoo(levg,ylim = c(0.0,1.5),ylab = "Leverage")
abline(h = 1.0,lty = "dotted")



##  Figure 2.16 Left-Hand Plot

GmvLS.TO <- 100*turnOver(wtsGmvLS)
plot.zoo(GmvLS.TO,ylim = c(0,60), ylab = "TURNOVER (%)",
         xlab = "", cex.axis = 1.5, cex.lab = 1.5)
abline(h = mean(GmvLS.TO),lty = "dashed")
text(as.Date("2004-01-31"),50,"Mean Turnover = 13.4 (%)", cex = 1.5)


## Figure 2.16 Right-Hand Plot

GmvLS.DIV <- 100*divHHI(wtsGmvLS)
plot.zoo(GmvLS.DIV,ylim = c(0,100),lwd = 1.5, ylab = "DIV(%)",
         xlab = "", cex.axis = 1.5, cex.lab = 1.5)
abline(h = mean(GmvLS.DIV),lty = "dashed")
text(as.Date("2006-01-31"),90,"Mean Diversification = 63.5 (%)",
     cex = 1.5)


##  Table 2.3
# Risk & Performance Estimator Standard Errors package

SD12 <- SD.SE(ret.comb, se.method = "IFiid")
SD12 <- printSE(SD12, round.digit = 4)
SSD12 <- SemiSD.SE(ret.comb, se.method = "IFiid")
SSD12 <- printSE(SSD12, round.digit = 4)
ES12 <- ES.SE(ret.comb, se.method = "IFiid")
ES12 <- printSE(ES12, round.digit = 4)
VaR12 <- VaR.SE(ret.comb, se.method = "IFiid")
VaR12 <- printSE(VaR12, round.digit = 4)
# VaR12[,1] <- -VaR12[,1]
RM <- 100*rbind(SD12,SSD12,ES12,VaR12)
colnames(RM)  <- c("Estimate (%)","StdError (%)")
rownames(RM) <- c("GmvLS SD","Market SD",
                  "GmvLS SSD","Market SSD",
                  "GmvLS ES","Market ES",
                  "GmvLS VaR","Market VaR")
RM <- as.data.frame(RM)
RM

##  Figure 2.17
# Risk-free rates were not negligible before 2009

stockItems <- c("Date", "TickerLast", "Return", "Ret13WkBill")
returnsAll <- selectCRSPandSPGMI("monthly", stockItems = stockItems,  
                                 factorItems = NULL, outputType = "xts")
riskFree <- returnsAll[ , "Ret13WkBill"]
tsPlotMP(riskFree, yname = "RISK-FREE RATE")


##  Figure 2.18
# Variation of risk-free rate for 1995 through 2000

returns <- returnsAll["1995-01-31/2000-12-31"]
x <- sort(apply(returns,2,mean))
x0 <- x[x <= 0.007 & x >= 0.005] # Results in 21 stocks & choose FMC
ret <- returns[, c("FMC", "Ret13WkBill")]
names(ret)[2] <- "Risk-Free"
tsPlotMP(ret, yname = "RETURNS", scaleType = "free")


##  Table 2.4

SR12 <- SR.SE(ret.comb, se.method = "IFiid")
SR12 <- printSE(SR12, round.digit = 2)
DSR12 <- DSR.SE(ret.comb, se.method = "IFiid")
DSR12 <- printSE(DSR12, round.digit = 2)
SoR12 <- SoR.SE(ret.comb, se.method = "IFiid")
SoR12 <- printSE(SoR12, round.digit = 2)
ESratio12 <- ESratio.SE(ret.comb, se.method = "IFiid")
ESratio12 <- printSE(ESratio12, round.digit = 2)
Ratios <- rbind(SR12,DSR12,SoR12,ESratio12)
colnames(Ratios)  <- c("Estimate","StdError")
rownames(Ratios) <- c("GmvLS SR","Market SR",
                      "GmvLS DSR","Market DSR",
                      "GmvLS SOR","Market SOR",
                      "GmvLS ESR","Market ESR")
Ratios <- as.data.frame(Ratios)
Ratios

##  Figure 2.19

data(edhec, package = "PerformanceAnalytics")
colnames(edhec) <- c("CA", "CTAG", "DIS", "EM", "EMN", "ED", "FIA", 
                     "GM", "LS", "MA", "RV", "SS", "FoF")
par(mfrow = c(1, 2))
outSD <- IF.SD(returns = edhec$CA, evalShape = T, IFplot = T)
outSR <- IF.SemiSD(returns = edhec$CA, evalShape = T, IFplot = T)
par(mfrow = c(1, 1))


##  Figure 2.20

muRet = c(.10,.04,.02)
volRet = c(.20,.15,.10)
corrRet = diag(c(1,1,1))
mathEfrontRiskyMuCov(muRet,volRet,corrRet,efront.only = F)
text(0.07, 0.095, "EFFICIENT FRONTIER", cex = 1.2)
arrows(0.07, 0.09, .10, .06, length = 0.1, lwd= 1.0)



##  Table 2.5

muRet = c(.10,.04,.02)
volRet = c(.20,.15,.10)
corrRet = diag(c(1,1,1))
efront = mathEfrontRiskyMuCov(muRet,volRet,corrRet, npoints = 5, values = T, display = F)
mu.efront = efront$mu.efront
wtsEfront <- mathWtsEfrontRiskyMuCov(muRet,volRet,corrRet,mu.efront,digits = 3)
wtsEfront


##  Figure 2.21

nColor <- 4
barplotWts(as.matrix(wtsEfront), legend.text = T, ylab = "WEIGHTS",
           col = topo.colors(nColor), bar.ylim = c(-1, 2),cex.lab = 1.2,
           cex.axis = 1.3)


##  Figure 2.22

returns10 <- returns10Mkt[,-11]
efront <- mathEfrontRisky(returns10, display = T, cexGmv = 1.0,
                          cexPoints = 1.1, cexText = 0.9)



##  Figure 2.23

efront10 <- mathEfrontRisky(returns10,npoints = 5, display = F, values = TRUE)
mu.efront <- efront10$mu.efront
wtsEfront <- mathWtsEfrontRisky(returns10, mu.efront,digits = 3)
barplotWts(as.matrix(wtsEfront), legend.text = T, ylab = "WEIGHTS",
           col = topo.colors(10), bar.ylim = c(-1.5,3.0),cex.lab = 1.2,
           cex.axis = 1.3)


##  Figure 2.24

stockItems <- c("Date","TickerLast","CapGroupLast","Return","Ret13WkBill")
dateRange <- c("1997-01-31","2010-12-31")
stocksDat <- selectCRSPandSPGMI("monthly",dateRange = dateRange, 
                                stockItems = stockItems, factorItems = NULL, 
                                subsetType = "CapGroupLast",
                                subsetValues = "SmallCap", outputType = "xts")
returns10andRF <- stocksDat[, c(21:30,107)]
names(returns10andRF)[11] <- "RiskFree"
tsPlotMP(returns10andRF,scaleType = "free",layout = c(2,6),stripText.cex = .45,
         axis.cex = 0.4,lwd = 0.5)


##  Figure 2.25

rf <- mean(returns10andRF[,11])
returns10 <- returns10andRF[,-11]
mathEfrontCashRisky(returns10, rf = rf, cexPoints = 1.0)


##  Figure 2.26

rf <- mean(returns10andRF[,11])
returns10 <- returns10andRF[,-11]
wtsEfront = mathEfrontCashRisky(returns10, plot.efront = FALSE, values = TRUE)
barplotWts(as.matrix(wtsEfront),legend.text = T,col = topo.colors(3),
           ylab = "Weights",xlab = "VOL", bar.ylim = c(-0.5,1.5))



##  Calculation of risk aversion and risk tolerance values for Figure 2.25,
##  reported at end of paragraph below (2.154)

rf = .005
C = var(returns10)
mu.stocks = apply(returns10, 2, mean)
mue = mu.stocks - rf
a = solve(C, mue)
lambda = sum(a)
lambda      # Risk aversion value
1/lambda    # Risk tolerance value


##  Figure 2.27

plot(FRBinterestRates, xaxt = "n", xlab = "", ylab = "InterestRates (%)")
axis(side = 1,at = seq(1930,2020,by=10), labels = seq(1930, 2020, by=10))
grid()


##  Figure 2.28

rf <- mean(returns10andRF[,11])
returns10 <- returns10andRF[,-11]
mathEfront(returns10, rf = rf, mu.max = .035, sigma.max = .19, cexText = 0.8, npoints = 100)


##  Figure 2.29

rf        <- 0.03
rf_lend   <- 0.04
rf_borrow <- 0.06
er_port   <- 0.07
leverage  <- seq(0, 2, .1)

er_rf <- rf + leverage * (er_port - rf)
er_1  <- rf_lend   + leverage * (er_port - rf_lend)
er_2  <- rf_borrow + leverage * (er_port - rf_borrow)

df <- data.frame("Leverage" = leverage,
                 "Single Risk Free Rate for Borrowing and Lending"  	= er_rf,
                 "Different Risk Free Rates for Borrowing and Lending"  = pmin(er_1, er_2))

df_melt <- reshape2::melt(df, id.vars =  "Leverage", variable.name = "Risk_Free_Rate")
df_melt[["Risk_Free_Rate"]] <- gsub("\\.", " ", df_melt[["Risk_Free_Rate"]])

ggplot(df_melt, aes(x = Leverage, y = value )) +
  geom_line(aes(color = Risk_Free_Rate, linetype = Risk_Free_Rate), linewidth = 1) +
  labs(x = "Leverage", y = "Expected Return", 
       color = "Risk Free Rate", linetype = "Risk Free Rate") +
  theme_bw() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.4, 0.9),
        legend.title = element_text(size = 16),
        legend.text  = element_text(size = 14),
        axis.text    = element_text(size = 12),
        axis.title   = element_text(size = 14))


##  Figure 2.30

volMu1 = c(.20,.10)
volMu2 = c(.15,.04)
volMu3 = c(.10,.02)
vol = c(volMu1[1],volMu2[1],volMu3[1])
mu = c(volMu1[2],volMu2[2],volMu3[2])
corrMat = matrix(rep(0,9),nrow = 3)+ diag(rep(1,3))
V = diag(vol)%*%corrMat%*%diag(vol)
# Compute IR for the three stocks
one = rep(1, nrow(V))
z1 = solve(V, one) # Vinv*one
z2 = solve(V, mu)  # Vinv*mu
a = as.numeric(t(mu) %*% z1)   # a = mu*Vinv*one
b = as.numeric(t(mu) %*% z2)   # b = mu*Vinv*mu
cc = as.numeric(t(one) %*% z1) # c = one*Vinv*one
d = b * cc - a^2
muGmv = a/cc
sigmaGmv = 1/sqrt(cc)
IR = sigmaGmv*sqrt(d)
# Plot active efficient frontier
sigmaA = seq(0,20,1)
muA = IR*sigmaA
xlab = "Active Volatility (%)"
ylab = "Active Mean Return (%)" 
plot(sigmaA,muA,xlim = c(0,21),ylim = c(0,8.5),type = "l",lwd = 1.5,
     xlab = xlab, ylab = ylab, xaxs = "i", yaxs = "i", cex.lab = 1.3)
text(2,7,pos = 4, "IR = slope of line = .36", cex = 1.5)


##  Table 2.6

wGmv = z1/cc
w1 = z2/a
mu1 = b/a
sigmaA = c(.02,.05,.10)
wA = as.matrix((IR*sigmaA/(mu1 - muGmv))%*%t(w1-wGmv))
rowSum <- apply(wA,1,sum)
wA <- cbind(wA,rowSum)
wA <- round(wA,3)
wA.df = data.frame(wtA1=wA[,1],wtA2=wA[,2],wtA3=wA[,3],
                   wtAsum=wA[,4])
rnames = c("TE  2% ","TE  5% ","TE 10% ")
row.names(wA.df) = rnames
wA.df


## Figure 2.31
# Actively managed frontier dominated by efficient frontier

volB = 0.12
muB = 0.045
varGmv = sigmaGmv^2
muGmv = muGmv
mu1 = mu1
varB = volB^2
muA = seq(-.02,0.06,.001)
sigmaA = muA/IR
muPA = muB + muA
varA = sigmaA^2
const = (2/(mu1-muGmv))*varGmv*(muB/muGmv-1)
varPA = varB + varA + muA*const
sigmaPA = sqrt(varPA)

# Plot using mathEfrontRiskyMuCov for the efficient frontier
mathEfrontRiskyMuCov(mu,vol,corrMat,efront.only = T, display = T)
lines(sigmaPA,muPA, type = "l", lwd = 1.5)
points(volB,muB,pch = 16,cex = 1.3)
text(volB, muB,"B",pos = 4, cex = 1.3)
text(0.163,0.06,"ACTIVELY MANAGED",pos = 4, cex = 1.1)
arrows(0.165,0.06,sigmaPA[50],muPA[50],length = .1,lwd = 1.5)


##  Figure 2.32

# The two plots in this Figure are from Jorion(2003)
# Proper citation will be added.


##  Figure 2.33

# These two plots were created by the PCRA authors


##  Figure 2.34

#  Left-Hand Plot Code

powerUtilityPlots <- function()
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
powerUtilityPlots()

#  Right-Hand Plot Code

quadraticUtilityPlot <- function()
{
  v = seq(0,1.5,.01)
  u = v-v^2
  ylim = c(-0.7,0.4)
  plot(v,u,type = "l",ylim = ylim, xlab = "v", ylab = "U(v)", lwd = 1.5)
  abline(v = .5, lty = "dotted")
  abline(h = .25, lty = "dotted")
}
quadraticUtilityPlot()


##  Figure 2.35

#  Left-Hand Plot Code

rm1	<- 0.18
Beta1  <- c(1.53, 1.36, 1.24, 1.17, 1.06, 0.92, 0.84, 0.76, 0.63, 0.48)
Mu1    <- c(0.26, 0.22, 0.21, 0.21, 0.18, 0.17, 0.16, 0.15, 0.13, 0.12)
Sigma1 <- c(0.49, 0.43, 0.39, 0.37, 0.33, 0.29, 0.27, 0.24, 0.20, 0.17)
SML1   <- rm1 * Beta1

df1  <- data.frame( Beta1, Mu1, Sigma1, SML1)
df1a <- data.frame(x=1, y = rm1)

p1 <- ggplot(df1) + 
  geom_point(aes(x = Beta1, y = Mu1),  color = "black") + 
  geom_line(aes( x = Beta1, y = SML1), color = "gray20") +
  labs(x = "Beta", y = "Mean Excess Return", title = "1931 \u2013 1965") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate(geom="text", x=1.19, y=0.15,  label="CAPM Security Market Line", color="gray20") +
  annotate(geom="text", x=.78,  y=0.185, label="Market Portfolio", color="dodgerblue4")

p1 + geom_point(data = df1a, aes(x = x, y = y),  
                color = "dodgerblue4", shape = 17, size = 3)
#  Rigt-Hand Plot Code

rm2    <- 0.08
Beta2  <- c(1.50, 1.30, 1.17, 1.09, 1.03, 0.95, 0.87, 0.78, 0.67, 0.51)
Mu2    <- c(0.06, 0.08, 0.08, 0.08, 0.08, 0.08, 0.07, 0.07, 0.07, 0.06)
Sigma2 <- c(0.31, 0.26, 0.24, 0.22, 0.21, 0.19, 0.18, 0.16, 0.14, 0.12)
SML2   <- rm2 * Beta2

df2  <- data.frame( Beta2, Mu2, Sigma2, SML2)
df2a <- data.frame(x=1, y = rm2)

p2 <- ggplot(df2) + 
  geom_point(aes(x = Beta2, y = Mu2),  color = "black")  +
  geom_line(aes( x = Beta2, y = SML2), color = "gray20") +
  ylim(0, 0.12) +
  labs(x = "Beta", y = "Mean Excess Return", title = "1965 \u2013 1991") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate(geom="text", x=1.12, y=0.055, label="CAPM Security Market Line", color="gray20") +
  annotate(geom="text", x=.80,  y=0.095, label="Market Portfolio", color="dodgerblue4")

p2 + geom_point(data = df2a, aes(x = x, y = y),  
                color = "dodgerblue4", shape = 17, size = 3)

##  Figure 2.36

#  Left-Hand Plot Code

ggplot(df1, aes(x = Beta1, y = Sigma1)) + 
  geom_point() +    
  geom_smooth(formula = 'y ~ x', method='lm', se = FALSE, linewidth = 0.6, color = "gray20") +
  labs(x = "Beta", y = "Standard Deviation") +
  geom_text(x = 1.05, y = 0.27, label = "sigma %~~% 0.32 ~ beta", parse=TRUE)

#  Right-Hand Plot Code

ggplot(df2, aes(x = Beta2, y = Sigma2)) + 
  geom_point() +    
  geom_smooth(formula = 'y ~ x', method='lm', se = FALSE, linewidth = 0.6, color = "gray20") +
  labs(x = "Beta", y = "Standard Deviation") +
  geom_text(x = 1.05, y = 0.18, label = "sigma %~~% 0.2 ~ beta", parse=TRUE)


##  Table 2.7

df <- data.frame(matrix(" ", nrow = 6, ncol = 4))
df$X1 <- c("1/31--12/39",  "1/40--12/49", "1/50--12/59", "1/60--12/69", "1/70--12/79","1/80--12/91")
df$X2 <- c(-0.05, 0.03, 0.08, 0.03, 0.01, 0.09)
df$X3 <- c(0.17,  0.10, 0.06, 0.07, 0.10, 0.08)
df$X4 <- c(-0.94,  1.06, 4.25, 1.32, 0.18, 3.90)
#Rename rows and columns and reformat the table
# colnames(df) <- c("Period", "$\\mu_{e}$", "$\\sigma_{e}$", "$t( \\mu )$") # In the text
colnames(df) <- c("Period", "Mean(Excess Return)", "Std. Dev(Excess Return)", "t(Mean(Excess Return))")

df


##  Figure 2.37

stocksCRSPweekly <- getPCRAData("stocksCRSPweekly")
dateRange    <- c("2004-01-01", "2005-12-31")
stockItems <- c("Date", "TickerLast", "CapGroupLast", "Return", 
                "MktIndexCRSP", "Ret13WkBill")
returnsAll <- selectCRSPandSPGMI("weekly",
                                 dateRange = dateRange,
                                 stockItems = stockItems, 
                                 factorItems = NULL, 
                                 subsetType = "CapGroupLast",
                                 subsetValues = "SmallCap", 
                                 outputType = "xts")
returns <- returnsAll[ , 1:10]
tsPlotMP(returns, scaleType = "free",layout = c(2,5),stripText.cex = .45,
         axis.cex = 0.4,lwd = 0.5)


##  Figure 2.38

pspec <- portfolio.spec(assets = names(returns))
pspecFI <- add.constraint(pspec, type = "full_investment")
pspecLO <- add.constraint(portfolio = pspecFI, type = "long_only")
pspecESLO5pct <- add.objective(pspecLO, type = "risk", name = "ES",
                               arguments = list(p = 0.050))

# Increase n.portfolios below for more accurate vertical dot-dash line
lty <- c("dashed", "solid",  "dotted", "dotdash")
col <- c("red", "black", "darkgreen", "darkgreen")
chart.EfficientFrontierCompare(returns, pspecESLO5pct, risk_type = "ES", 
                               guideline = TRUE,  cex.axis = 1.2,
                               match.col = c("StdDev", "ES"),
                               n.portfolios = 10,
                               lwd=c(1.3, 1.4, 1.3, 1.0),
                               col = col, lty = lty,
                               xlim = c(0.02, 0.08), ylim = c(0.0, 0.012), 
                               legend.loc = "topleft", main = NULL)


##  Figure 2.39

chart.EfficientFrontierCompare(returns, pspecESLO5pct, risk_type = "StdDev", 
                               guideline = TRUE,  cex.axis = 1.2,
                               match.col = c("ES", "StdDev"),
                               n.portfolios = 10,
                               lwd=c(1.3, 1.4, 1.3, 1.0),
                               col = col, lty = lty,
                               xlim = c(0.01, 0.06), ylim = c(0.0, 0.012), 
                               legend.loc = "topleft", main = NULL)


##  Figure 2.40

x <- seq(-4.9, 4.9, by = 0.001)
ccopt <- computeTuningPsi_modOpt(0.95)
plot(x, wgt_modOpt(x, cc = ccopt), type = "l",
     xlab = "x", ylab = "",cex = 1.5,cex.lab = 1.5)


##  Figure 2.41

data(edhec)
hfnames <- c("CA","CTA","DIS","EM","EMN","ED","FIA","GM","LSE","MA","RV","SS","FOF")
names(edhec) <- hfnames
retLongFIA <- edhec[,"FIA"]
retFIA <- retLongFIA['1998-01-31/1999-12-31',]
index(retFIA) <- as.yearmon(index(retFIA))

# Plot FIA returns with sample mean and robust mean
mu <- 100*mean(retFIA)
se.mu <- 100*sd(retFIA)/sqrt(24)
x <- locScaleM(retFIA,eff = .95)
muRob <- 100*x$mu
se.muRob <- 100*x$std.mu
plot.zoo(retFIA,type ="b",xlab = "",ylab = "FIA Returns")
abline(h = muRob/100, col = "blue")
abline(h = mu/100, lty = "dashed", col ="red")
legend(1999.2,-.03,legend = c("Robust Mean","Sample Mean"),lwd = c(1,2),
       lty = c("solid","dashed"),col = c("blue","red"), bty = "n", cex = 1.3)


##  Table 2.8

tstat.mu <- mu/se.mu
tstat.muRob <- muRob/se.muRob
SR.classic <- tstat.mu/sqrt(24)
SR.Rob <- tstat.muRob/sqrt(24)
row1 <- round(c(mu,se.mu,tstat.mu,SR.classic),2)
row2 <- round(c(muRob,se.muRob,tstat.muRob,SR.Rob),2)
meanEsts <- data.frame(rbind(row1,row2))
names(meanEsts) <- c("Estimate (%)", "Std. Error (%)", "t-Stat","Sharpe Ratio")
row.names(meanEsts) <- c("Sample Mean", "Robust Mean")
meanEsts


##  Figure 2.42

# mOpt 95% Efficiency Tuning Constant
ccModOpt <- computeTuningPsi_modOpt(0.95)

# mOpt M-scale weight function
wgt_mOptScale <- function(x) {
  rho_modOpt(x, cc = ccModOpt)/(x^2)}
wgt_mOptScaleMax <- wgt_mOptScale(0.0001)

# Plot mOptScale Weight Function
x <- seq(-5.5, 5.5, 0.01)
ylim <- c(0, 1.4)
ylab <- "w_scale,mOpt(x)"
plot(x,wgt_mOptScale(x)/wgt_mOptScaleMax, ylim = ylim, 
     ylab = ylab, type = "l", cex.lab = 1.1)
abline(h = 1.0, lty = "dotted")


##  Figure 2.43

data(edhec, package = "PerformanceAnalytics")
hfnames <- c("CA","CTA","DIS","EM","EMN","ED","FIA","GM","LSE","MA","RV","SS","FOF")
names(edhec) <- hfnames
range(index(edhec))
edhec <- edhec[ , 1:12]
returns <- 100*edhec['1998-01-31/1999-12-31']

# Plot edhec returns for 1998-1999
tsPlotMP(returns, type = "l", stripText.cex = 0.7, axis.cex = 0.7)


##  Table 2.9

StdDev <- apply(returns,2,sd)
MADM <- apply(returns,2,mad)
resid <- returns - median(returns)
RobSD <- apply(resid, 2, scaleM, family = "mopt")
SDestsMat <- cbind(StdDev, MADM, RobSD)
SDestsMat <- round(SDestsMat,2)
SDests <- data.frame(hfnames[1:12], SDestsMat)
names(SDests) <- c("HFindex", "StdDev", "MADM", "RobSD")
row.names(SDests) <- NULL
SDests
######
# You first need to install all the R packages listed as arguments to the
# library(packageName) functions below in lines 20 to 32, as follows:
# 
# Install the devtools package using the RStudio Tools/Install Packages
# drop-down menu, or with install.packages("devtools") in the RStudio Console.
#
# Next install PCRA with the devtools function at the RStudio Console with:
# devtools::install_github("robustport/PCRA")
#
# Then install all the other packages below, except the optimalRhoPsi package,
# using the RStudio Tools/Install Packages drop-down menu.
#
# Finally, install the optimalRhoPsi package at the RStudio Console with
# devtools::install_github("kjellpk/optimalRhoPsi")
#
# NOW YOU ARE READY TO RUN THE CH 2 REPRODUCIBILITY CODE BELOW
######

library(PCRA)          ## Install with devtools as above
library(data.table)
library(xts)
library(PerformanceAnalytics) 
library(PortfolioAnalytics)
library(foreach)
library(CVXR)
library(RPESE)
library(RPEIF)
library(ggplot2)
library(dplyr)
library(RobStatTM)
library(optimalRhoPsi)  ## Install with devtools as above


##  Table 2.1

# Largecaps
stockItems <- c("Date","TickerLast","CapGroupLast","Return")
dateRange <- c("1993-01-31","2015-12-31")
returns <- selectCRSPandSPGMI("monthly",dateRange = dateRange, stockItems =
                                stockItems, factorItems = NULL, 
                              subsetType = "CapGroupLast",
                              subsetValues = "LargeCap", outputType = "xts")

ret <- coredata(returns) # Package xts
n <- ncol(ret)
acfLarge <- rep(0,n)
for(i in 1:n){
  acfLarge[i] <- acf(ret[,i], lag.max = 1, 
                     plot = FALSE)$acf[2]
}
muLarge <- mean(acfLarge)
sdLarge <- sd(acfLarge)

# Midcaps
returns <- selectCRSPandSPGMI("monthly",dateRange = dateRange, stockItems =
                                stockItems, factorItems = NULL, subsetType = "CapGroupLast",
                              subsetValues = "MidCap", outputType = "xts")
ret <- coredata(returns) # Package xts
n <- ncol(ret)
acfMid <- rep(0,n)
for(i in 1:n){
  acfMid[i] <- acf(ret[,i], lag.max = 1, 
                   plot = FALSE)$acf[2]
}
muMid <- mean(acfMid)
sdMid <- sd(acfMid)

## Smallcaps
returns <- selectCRSPandSPGMI("monthly",dateRange = dateRange, stockItems =
                                stockItems, factorItems = NULL, subsetType = "CapGroupLast",
                              subsetValues = "SmallCap", outputType = "xts")
ret <- coredata(returns) # Package xts
n <- ncol(ret)
acfSmall <- rep(0,n)
for(i in 1:n){
  acfSmall[i] <- acf(ret[,i], lag.max = 1, 
                     plot = FALSE)$acf[2]
}
muSmall <- mean(acfSmall)
sdSmall <- sd(acfSmall)

## Microcaps
returns <- selectCRSPandSPGMI("monthly",dateRange = dateRange, stockItems =
                                stockItems, factorItems = NULL, subsetType = "CapGroupLast",
                              subsetValues = "MicroCap", outputType = "xts")
ret <- coredata(returns) # Package xts
n <- ncol(ret)
acfMicro <- rep(0,n)
for(i in 1:n){
  acfMicro[i] <- acf(ret[,i], lag.max = 1, 
                     plot = FALSE)$acf[2]
}

muMicro <- mean(acfMicro)
sdMicro <- sd(acfMicro)

dat <- cbind(c(muLarge, sdLarge), c(muMid, sdMid),
             c(muSmall, sdSmall), c(muMicro, sdMicro))
dat <- round(dat,3)
dat <- data.frame(dat)
names(dat) <- c("LargeCap","MidCap","SmallCap","MicroCap")
row.names(dat) <- c("  Mean Lag-1 Acf", 
                    "StdDev Lag-1 Acf")
dat


##  Figure 2.1

dat <- list(acfLarge, acfMid, acfSmall, acfMicro)
names(dat) <- c("87 LargeCaps","67 MidCaps","106 SmallCaps","34 MicroCaps")
boxplot(dat, varwidth = TRUE, col = "cyan")



##  Figure 2.2

returns <- PerformanceAnalytics::edhec
returns <- returns["2002-01-31/2019-12-31", -13]
names(returns) <- c("CA","CTA","DIST","EM","EMN","ED","FIA",
                    "GM","LSE","MA","RV","SS")
PCRA::tsPlotMP(returns)
# range(index(returns))
Ret <- coredata(returns)
n <- ncol(Ret)
acfRet <- rep(0,n)
for(i in 1:n){
  acfRet[i] <- acf(Ret[,i], lag.max = 1, 
                   plot = FALSE)$acf[2]
}
hist(acfRet, main = "EDHEC Hedge Fund Indexes",
     xlab = "Lag-1 ACF Values")


## Figure 2.3

# names(acfRet) <- names(returns)
# (names(sort(acfRet)[1:4]))
# delete "CTA", "GM", "SS", "EMN"

returns8 <- returns[ , c("CA","DIST","EM","ED",
                         "FIA","LSE","MA","RV")]
PCRA::tsPlotMP(returns8, yname = "RETURNS", 
               stripText.cex = 0.7, axis.cex = 0.7)



##  Figure 2.4

acf(returns8$EM, main = "EM", lag.max = 5)



##  Figure 2.5

muVol <- c(.20,.10,.15,.04)
wts <- seq(0,1,.01)
efront2Asset <- function(wts,rho,muVol = c(.20,.10,.15,.04))
{
  sigma1 <- muVol[1]
  mu1 <- muVol[2]
  sigma2 <- muVol[3]
  mu2 <- muVol[4]
  n <- length(wts)
  efront <- data.frame(matrix(rep(0,3*n),ncol = 3))
  names(efront) <- c("SIGMA","MU","WTS")
  w <- wts
  for(i in 1:n){
    mu <- w[i]*mu1 + (1-w[i])*mu2
    var <- w[i]^2*sigma1^2 + 2*w[i]*(1-w[i])*rho*sigma1*sigma2 + (1-w[i])^2*sigma2^2
    sigma <- sqrt(var)
    efront[i,] <- c(sigma,mu,w[i])
  }
  return(efront)
}
ef <- efront2Asset(wts,0,muVol = muVol)
gmv <- ef[ef$SIGMA == min(ef$SIGMA),]
xlab <- expression(sigma [P])
ylab <- expression(mu [P])
par(pty = "s")
plot(ef$SIGMA,ef$MU,type = "l", xlab = xlab, ylab = ylab,
     xlim=c(0,.25),ylim=c(0.03,.11),lwd = 2, cex.lab = 1.5)
points(muVol[c(1,3)], muVol[c(2,4)], pch = 19, cex = 1.3)
points(gmv,pch = 19, cex = 1.3)
text(.04,.10,expression(paste(rho, " = 0")),cex = 1.5)
text(0.12,.0616,adj = c(1,NA),"MinRisk   ",cex = 1.1)
text(0.13,.0616,adj = c(0,NA), "(.12, .0616)",cex = 1.1)
text(0.2,.1,adj = c(0,NA),"  (.20, .10)",cex = 1.1)
text(0.15,.04,adj = c(0,NA),"  (.15, .04)",cex = 1.1)


##  Figure 2.6

muVol <- c(.20,.10,.15,.04)
wts <- seq(0,1,.01)
ef <- efront2Asset(wts,0,muVol = muVol)
ef1 <- efront2Asset(wts,1,muVol = muVol)
ef2 <- efront2Asset(wts,-1,muVol = muVol)
gmv <- ef[ef$SIGMA == min(ef$SIGMA),]
gmv2 <- ef2[ef2$SIGMA == min(ef2$SIGMA),]
xlab <- expression(sigma [P])
ylab <- expression(mu [P])
par(pty = "s")
plot(ef$SIGMA,ef$MU,type = "l", xlab = xlab, ylab = ylab,
     xlim=c(0,.25),ylim=c(0.03,.11),lwd = 2, cex.lab = 1.5)
points(muVol[c(1,3)], muVol[c(2,4)], pch = 19, cex = 1.5)
points(gmv,pch = 19, cex = 1.3)
text(0.2,.1,adj = c(0,NA),"  (.20, .10)",cex = 1.1)
text(0.15,.04,adj = c(0,NA),"  (.15, .04)",cex = 1.1)
lines(ef1$SIGMA,ef1$MU,lty = 2,lwd = 2)
lines(ef2$SIGMA,ef2$MU,lty = 2,lwd = 2)
points(gmv2,pch = 19, cex = 1.3)
text(.12,.07,expression(paste(rho, " = 0 ")),adj = c(1,NA),cex = 1.5)
text(.02,.08,expression(paste(rho, " = -1  ")),adj = c(0,NA),cex = 1.5)
text(.18,.07,expression(paste(rho, " = +1 ")),adj = c(0,NA),cex = 1.5)


##   Figure 2.7

muVol <- c(.20,.10,.15,.04)
wts   <- seq(0,1,.01)
efLO  <- efront2Asset(wts,0,muVol = muVol)
wts   <- seq(1,1.25,.01)
efSS  <- efront2Asset(wts,0,muVol = muVol)
gmv   <- ef[ef$SIGMA == min(ef$SIGMA),]
maxMu <- ef[ef$MU == max(ef$MU),]
maxMuSS <- efSS[efSS$MU == max(efSS$MU),]
xlab  <- expression(sigma [P])
ylab  <- expression(mu [P])
par(pty = "s")
plot(efLO$SIGMA,efLO$MU,type = "l", xlab = xlab, ylab = ylab,
     xlim=c(0,.40),ylim=c(.02,.13),lwd = 2, cex.lab = 1.5)
lines(efSS$SIGMA,efSS$MU,lty = "dashed", lwd = 2)
points(gmv[1:2],pch = 19, cex = 1.3)
points(maxMu[1:2], pch = 19, cex = 1.3)
points(maxMuSS[1:2], pch = 19, cex = 1.3)
text(.04,.12,expression(paste(rho, " = 0")),cex = 1.5)
text(gmv[1:2],adj = c(0,NA),
     paste("  (",toString(round(gmv[1:2],2)),")"),cex = 1.1)
text(maxMu[1:2],adj = c(0,NA),
     paste("  (",toString(maxMu[1:2]),")"),cex = 1.1)
text(maxMuSS[1:2],adj = c(0,NA), 
     paste("  (",toString(round(maxMuSS[1:2],2)),")"), cex = 1.1)


##   Figure 2.8

volMu1 <- c(.20,.10)
volMu2 <- c(.15,.04)
volMu3 <- c(.10,.02)
names(volMu1) <- c("SIGMA","MU")
names(volMu2) <- c("SIGMA","MU")
names(volMu3) <- c("SIGMA","MU")
wts <- seq(0,1,.01)
ef1 <- efront2Asset(wts,0,muVol = c(volMu1,volMu2))
ef2 <- efront2Asset(wts,0,muVol = c(volMu1,volMu3))
ef3 <- efront2Asset(wts,0,muVol = c(volMu2,volMu3))
xlab <- expression(sigma [P])
ylab <- expression(mu [P])
par(pty = "s")
plot(ef1$SIGMA,ef1$MU,type = "l", xlab = xlab, ylab = ylab,
     xlim=c(0,.25),ylim=c(0,.11),lwd = 2, cex.lab = 1.5)
lines(ef2$SIGMA,ef2$MU,lty = 2, lwd = 2.0)
lines(ef3$SIGMA,ef3$MU,lty = 3, lwd = 2.0)
xy <- rbind(volMu1,volMu2,volMu3)
points(xy, pch = 19, cex = 1.3)
text(volMu1[1]+0.01,volMu1[2],adj = c(0,NA),toString(volMu1),cex = 1.1)
text(volMu2[1]+0.01,volMu2[2],adj = c(0,NA),toString(volMu2),cex = 1.1)
text(volMu3[1]+0.01,volMu3[2],adj = c(0,NA),toString(volMu3),cex = 1.1)
text(.04,.10,expression(paste(rho, " = 0")),cex = 1.5)


##  Figure 2.9

volMu1 <- c(.20,.10)
volMu2 <- c(.15,.04)
volMu3 <- c(.10,.02)
vol <- c(volMu1[1],volMu2[1],volMu3[1])
mu <- c(volMu1[2],volMu2[2],volMu3[2])
corrMat0 <- matrix(rep(0,9),nrow = 3)+ diag(rep(1,3))
covMat0 <- diag(vol)%*%corrMat0%*%diag(vol)
n <- 500
port <- matrix(rep(0,2*n),ncol = 2)
dimnames(port)[[2]] = c("SIG.P","MU.P")
wts = hitandrun::simplex.sample(3,n)$samples
for(i in 1:n) {
  x <- wts[i,] 
  port[i,1] <- sqrt(x%*%covMat0%*%x)
  port[i,1]
  port[i,2] <- x%*%mu
}
xlab <- expression(sigma [P])
ylab <- expression(mu [P])
plot(port[,1],port[,2],xlim = c(0,.25),ylim = c(0,.11),
     xlab = xlab, ylab = ylab, pch = 20, cex = .7, cex.lab = 1.5)
points(vol,mu, pch = 19, cex = 1.3)
text(volMu1[1]+0.01,volMu1[2],adj = c(0,NA),toString(volMu1),cex = 1.1)
text(volMu2[1]+0.01,volMu2[2],adj = c(0,NA),toString(volMu2),cex = 1.1)
text(volMu3[1]+0.01,volMu3[2],adj = c(0,NA),toString(volMu3),cex = 1.1)
text(.04,.10,expression(paste(rho, " = 0")),cex = 1.5)


##  Example 2.3

muRet = c(.10,.04,.02)
volRet = c(.20,.15,.10)
corrRet = diag(c(1,1,1))
PCRA::mathGmvMuCov(muRet,volRet,corrRet,digits = 3)


##  Figure 2.10

# There is currently no code for this Figure, and while
# IBM and XOM are in our CRSP data, GE is not.
# Will consider replacing GE with a stock in the CRSP data.


## Example 2.6 Figures 2.8 - 2.10 with the following steps
# Get xts object of 106 smallcap stocks, and the Market ("MktIndexCRSP") in
# stocksCRSP for 1997 - 2010, and use the third group of 10 of these
# to compute Gmv portfolios. Change name "MktIndexCRSP" to "Market".


## Figure 2.11

library(data.table)
stockItems <- c("Date","TickerLast","CapGroupLast","Return","MktIndexCRSP")
dateRange <- c("1997-01-31","2010-12-31")
stocksDat <- PCRA::selectCRSPandSPGMI("monthly",dateRange = dateRange, stockItems =
                                        stockItems, factorItems = NULL, subsetType = "CapGroupLast",
                                      subsetValues = "SmallCap", outputType = "xts")

returns10Mkt <- stocksDat[, c(21:30,107)]
names(returns10Mkt)[11] <- "Market"
tsPlotMP(returns10Mkt,scaleType = "free",layout = c(2,6),stripText.cex = .45,
         axis.cex = 0.4,lwd = 0.5)



##  Figures 2.12 and 2.13

# Load PortfolioAnalytics and related packages, and compute time
# series of GmvLO and GmvLS portfolio weights wtsGmvLO and wtsGmvLS

# Create GmvLS portfolio specs
returns <- returns10Mkt[,1:10]
Market  <- returns10Mkt[,11]
funds   <- colnames(returns)

pspec       <- portfolio.spec(assets=funds)
pspec.fi    <- add.constraint(pspec, type="full_investment")
pspec.gmvLS <- add.objective(pspec.fi, type="risk", name="var")

# Optimize Portfolio at Monthly Rebalancing and 5-Year Training
bt.gmvLS <- optimize.portfolio.rebalancing(returns, pspec.gmvLS,
                                           optimize_method="CVXR",
                                           rebalance_on="months",
                                           training_period=60,
                                           rolling_window=60,
                                           trace = TRUE)
# Extract time series of portfolio weights
wtsGmvLS <- extractWeights(bt.gmvLS)

# Compute rebalancing GmvLS arithmetic returns
GmvLS <- Return.rebalancing(returns,wtsGmvLS)

# Combine GmvLS and Market returns and plot their time series
ret.comb <- na.omit(merge.xts(GmvLS, Market, all=F))
names(ret.comb) <- c("GmvLS","Market")


# Figure 2.12
tsPlotMP(wtsGmvLS,layout = c(2,5),scaleType = "same",
         stripText.cex = 0.7, axis.cex = .7)


# Figure 2.13
tsPlotMP(ret.comb,scaleType = "same",stripText.cex = .7, axis.cex = .7)


##  Figure 2.14
# Compute cumulative gross portfolio returns

R <- ret.comb
geometric <- TRUE
c.xts <- if ( geometric ) {
  cumprod(1+R)
} else {
  1 + cumsum(R)
}

# Plot cumulative gross returns of GmvLS and Market portfolios
# Original code contributed by Peter Carl

p <- plot.xts(c.xts[,1], col="black", main = "Cumulative Returns",
              grid.ticks.lwd=1, grid.ticks.lty = "dotted", grid.ticks.on = "years",
              labels.col="grey20", cex.axis=0.8, format.labels = "%b\n%Y",
              ylim = c(min(c.xts), max(c.xts)))
p <- addSeries(c.xts[,2], on=1, lwd=2, col="darkred", lty="dashed")
p <- addLegend("topleft", on = 1,
               legend.names = names(c.xts),
               lty = c(1,2), lwd = rep(2, NCOL(c.xts)),
               col = c("black", "darkred"),
               bty = "o", box.col = "white",
               bg=rgb(t(col2rgb("white")), alpha = 200,
                      maxColorValue = 255) )
d.xts <- PerformanceAnalytics::Drawdowns(R)
p <- xts::addSeries(d.xts[,1], col="darkblue", lwd=2, main="Drawdown",
                    ylim = c(min(d.xts), 0) )
p <- xts::addSeries(d.xts[,2], on=2, lwd=2, col="darkred", lty="dashed")

# panel 1 and 2 ylim
## ylim1 <- c(p$Env$ylim[[2]][1], p$Env$ylim[[2]][2]) No longer works
## ylim2 <- c(p$Env$ylim[[4]][1], p$Env$ylim[[4]][2]) No longer works

ylim1 <- p$Env$panels[[1]]$ylim
ylim2 <- p$Env$panels[[2]]$ylim
ylim <- c(ylim1, ylim2)

# get longest drawdown dates for xts object
dt <- table.Drawdowns(R, top = 1) # just want to find the worst drawdown
dt2 <- t(dt[,c("From", "To")])
x <- as.vector(dt2[,NCOL(dt2)])
y <- as.xts(matrix(rep(ylim, length(x)),ncol=length(ylim), byrow=TRUE), 			
            order.by=as.Date(x))
i=1
p <- xts::addPolygon(y[i:(i+1),1:2], on=-1, col="lightgrey") # top panel
p <- xts::addPolygon(y[i:(i+1),3:4], on=-2, col="lightgrey") # lower panel
p


##  Table 2.2

dt2mat <- table.Drawdowns(R, top = 2) # find the worst two drawdowns
dt2mat[,4] <- round(dt2mat[,4],2)
dt2 <- data.frame(dt2mat)[,1:5]
names(dt2) <- c("Begin","Minimum","End","Depth","Months")
dt2 <- dt2[,c(1:3,5,4)]
dt2


## Figure 2.15

levg <- levgLongShort(wtsGmvLS)
plot.zoo(levg,ylim = c(0.0,1.5),ylab = "Leverage")
abline(h = 1.0,lty = "dotted")



##  Figure 2.16 Left-Hand Plot

GmvLS.TO <- 100*turnOver(wtsGmvLS)
plot.zoo(GmvLS.TO,ylim = c(0,60), ylab = "TURNOVER (%)",
         xlab = "", cex.axis = 1.5, cex.lab = 1.5)
abline(h = mean(GmvLS.TO),lty = "dashed")
text(as.Date("2004-01-31"),50,"Mean Turnover = 13.4 (%)", cex = 1.5)


## Figure 2.16 Right-Hand Plot

GmvLS.DIV <- 100*divHHI(wtsGmvLS)
plot.zoo(GmvLS.DIV,ylim = c(0,100),lwd = 1.5, ylab = "DIV(%)",
         xlab = "", cex.axis = 1.5, cex.lab = 1.5)
abline(h = mean(GmvLS.DIV),lty = "dashed")
text(as.Date("2006-01-31"),90,"Mean Diversification = 63.5 (%)",
     cex = 1.5)


##  Table 2.3
# Risk & Performance Estimator Standard Errors package

SD12 <- SD.SE(ret.comb, se.method = "IFiid")
SD12 <- printSE(SD12, round.digit = 4)
SSD12 <- SemiSD.SE(ret.comb, se.method = "IFiid")
SSD12 <- printSE(SSD12, round.digit = 4)
ES12 <- ES.SE(ret.comb, se.method = "IFiid")
ES12 <- printSE(ES12, round.digit = 4)
VaR12 <- VaR.SE(ret.comb, se.method = "IFiid")
VaR12 <- printSE(VaR12, round.digit = 4)
# VaR12[,1] <- -VaR12[,1]
RM <- 100*rbind(SD12,SSD12,ES12,VaR12)
colnames(RM)  <- c("Estimate (%)","StdError (%)")
rownames(RM) <- c("GmvLS SD","Market SD",
                  "GmvLS SSD","Market SSD",
                  "GmvLS ES","Market ES",
                  "GmvLS VaR","Market VaR")
RM <- as.data.frame(RM)
RM

##  Figure 2.17
# Risk-free rates were not negligible before 2009

stockItems <- c("Date", "TickerLast", "Return", "Ret13WkBill")
returnsAll <- selectCRSPandSPGMI("monthly", stockItems = stockItems,  
                                 factorItems = NULL, outputType = "xts")
riskFree <- returnsAll[ , "Ret13WkBill"]
tsPlotMP(riskFree, yname = "RISK-FREE RATE")


##  Figure 2.18
# Variation of risk-free rate for 1995 through 2000

returns <- returnsAll["1995-01-31/2000-12-31"]
x <- sort(apply(returns,2,mean))
x0 <- x[x <= 0.007 & x >= 0.005] # Results in 21 stocks & choose FMC
ret <- returns[, c("FMC", "Ret13WkBill")]
names(ret)[2] <- "Risk-Free"
tsPlotMP(ret, yname = "RETURNS", scaleType = "free")


##  Table 2.4

SR12 <- SR.SE(ret.comb, se.method = "IFiid")
SR12 <- printSE(SR12, round.digit = 2)
DSR12 <- DSR.SE(ret.comb, se.method = "IFiid")
DSR12 <- printSE(DSR12, round.digit = 2)
SoR12 <- SoR.SE(ret.comb, se.method = "IFiid")
SoR12 <- printSE(SoR12, round.digit = 2)
ESratio12 <- ESratio.SE(ret.comb, se.method = "IFiid")
ESratio12 <- printSE(ESratio12, round.digit = 2)
Ratios <- rbind(SR12,DSR12,SoR12,ESratio12)
colnames(Ratios)  <- c("Estimate","StdError")
rownames(Ratios) <- c("GmvLS SR","Market SR",
                      "GmvLS DSR","Market DSR",
                      "GmvLS SOR","Market SOR",
                      "GmvLS ESR","Market ESR")
Ratios <- as.data.frame(Ratios)
Ratios

##  Figure 2.19

data(edhec, package = "PerformanceAnalytics")
colnames(edhec) <- c("CA", "CTAG", "DIS", "EM", "EMN", "ED", "FIA", 
                     "GM", "LS", "MA", "RV", "SS", "FoF")
par(mfrow = c(1, 2))
outSD <- IF.SD(returns = edhec$CA, evalShape = T, IFplot = T)
outSR <- IF.SemiSD(returns = edhec$CA, evalShape = T, IFplot = T)
par(mfrow = c(1, 1))


##  Figure 2.20

muRet = c(.10,.04,.02)
volRet = c(.20,.15,.10)
corrRet = diag(c(1,1,1))
mathEfrontRiskyMuCov(muRet,volRet,corrRet,efront.only = F)
text(0.07, 0.095, "EFFICIENT FRONTIER", cex = 1.2)
arrows(0.07, 0.09, .10, .06, length = 0.1, lwd= 1.0)



##  Table 2.5

muRet = c(.10,.04,.02)
volRet = c(.20,.15,.10)
corrRet = diag(c(1,1,1))
efront = mathEfrontRiskyMuCov(muRet,volRet,corrRet, npoints = 5, values = T, display = F)
mu.efront = efront$mu.efront
wtsEfront <- mathWtsEfrontRiskyMuCov(muRet,volRet,corrRet,mu.efront,digits = 3)
wtsEfront


##  Figure 2.21

nColor <- 4
barplotWts(as.matrix(wtsEfront), legend.text = T, ylab = "WEIGHTS",
           col = topo.colors(nColor), bar.ylim = c(-1, 2),cex.lab = 1.2,
           cex.axis = 1.3)


##  Figure 2.22

returns10 <- returns10Mkt[,-11]
efront <- mathEfrontRisky(returns10, display = T, cexGmv = 1.0,
                          cexPoints = 1.1, cexText = 0.9)



##  Figure 2.23

efront10 <- mathEfrontRisky(returns10,npoints = 5, display = F, values = TRUE)
mu.efront <- efront10$mu.efront
wtsEfront <- mathWtsEfrontRisky(returns10, mu.efront,digits = 3)
barplotWts(as.matrix(wtsEfront), legend.text = T, ylab = "WEIGHTS",
           col = topo.colors(10), bar.ylim = c(-1.5,3.0),cex.lab = 1.2,
           cex.axis = 1.3)


##  Figure 2.24

stockItems <- c("Date","TickerLast","CapGroupLast","Return","Ret13WkBill")
dateRange <- c("1997-01-31","2010-12-31")
stocksDat <- selectCRSPandSPGMI("monthly",dateRange = dateRange, 
                                stockItems = stockItems, factorItems = NULL, 
                                subsetType = "CapGroupLast",
                                subsetValues = "SmallCap", outputType = "xts")
returns10andRF <- stocksDat[, c(21:30,107)]
names(returns10andRF)[11] <- "RiskFree"
tsPlotMP(returns10andRF,scaleType = "free",layout = c(2,6),stripText.cex = .45,
         axis.cex = 0.4,lwd = 0.5)


##  Figure 2.25

rf <- mean(returns10andRF[,11])
returns10 <- returns10andRF[,-11]
mathEfrontCashRisky(returns10, rf = rf, cexPoints = 1.0)


##  Figure 2.26

rf <- mean(returns10andRF[,11])
returns10 <- returns10andRF[,-11]
wtsEfront = mathEfrontCashRisky(returns10, plot.efront = FALSE, values = TRUE)
barplotWts(as.matrix(wtsEfront),legend.text = T,col = topo.colors(3),
           ylab = "Weights",xlab = "VOL", bar.ylim = c(-0.5,1.5))



##  Calculation of risk aversion and risk tolerance values for Figure 2.25,
##  reported at end of paragraph below (2.154)

rf = .005
C = var(returns10)
mu.stocks = apply(returns10, 2, mean)
mue = mu.stocks - rf
a = solve(C, mue)
lambda = sum(a)
lambda      # Risk aversion value
1/lambda    # Risk tolerance value


##  Figure 2.27

plot(FRBinterestRates, xaxt = "n", xlab = "", ylab = "InterestRates (%)")
axis(side = 1,at = seq(1930,2020,by=10), labels = seq(1930, 2020, by=10))
grid()


##  Figure 2.28

rf <- mean(returns10andRF[,11])
returns10 <- returns10andRF[,-11]
mathEfront(returns10, rf = rf, mu.max = .035, sigma.max = .19, cexText = 0.8, npoints = 100)


##  Figure 2.29

rf        <- 0.03
rf_lend   <- 0.04
rf_borrow <- 0.06
er_port   <- 0.07
leverage  <- seq(0, 2, .1)

er_rf <- rf + leverage * (er_port - rf)
er_1  <- rf_lend   + leverage * (er_port - rf_lend)
er_2  <- rf_borrow + leverage * (er_port - rf_borrow)

df <- data.frame("Leverage" = leverage,
                 "Single Risk Free Rate for Borrowing and Lending"  	= er_rf,
                 "Different Risk Free Rates for Borrowing and Lending"  = pmin(er_1, er_2))

df_melt <- reshape2::melt(df, id.vars =  "Leverage", variable.name = "Risk_Free_Rate")
df_melt[["Risk_Free_Rate"]] <- gsub("\\.", " ", df_melt[["Risk_Free_Rate"]])

ggplot(df_melt, aes(x = Leverage, y = value )) +
  geom_line(aes(color = Risk_Free_Rate, linetype = Risk_Free_Rate), linewidth = 1) +
  labs(x = "Leverage", y = "Expected Return", 
       color = "Risk Free Rate", linetype = "Risk Free Rate") +
  theme_bw() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.4, 0.9),
        legend.title = element_text(size = 16),
        legend.text  = element_text(size = 14),
        axis.text    = element_text(size = 12),
        axis.title   = element_text(size = 14))


##  Figure 2.30

volMu1 = c(.20,.10)
volMu2 = c(.15,.04)
volMu3 = c(.10,.02)
vol = c(volMu1[1],volMu2[1],volMu3[1])
mu = c(volMu1[2],volMu2[2],volMu3[2])
corrMat = matrix(rep(0,9),nrow = 3)+ diag(rep(1,3))
V = diag(vol)%*%corrMat%*%diag(vol)
# Compute IR for the three stocks
one = rep(1, nrow(V))
z1 = solve(V, one) # Vinv*one
z2 = solve(V, mu)  # Vinv*mu
a = as.numeric(t(mu) %*% z1)   # a = mu*Vinv*one
b = as.numeric(t(mu) %*% z2)   # b = mu*Vinv*mu
cc = as.numeric(t(one) %*% z1) # c = one*Vinv*one
d = b * cc - a^2
muGmv = a/cc
sigmaGmv = 1/sqrt(cc)
IR = sigmaGmv*sqrt(d)
# Plot active efficient frontier
sigmaA = seq(0,20,1)
muA = IR*sigmaA
xlab = "Active Volatility (%)"
ylab = "Active Mean Return (%)" 
plot(sigmaA,muA,xlim = c(0,21),ylim = c(0,8.5),type = "l",lwd = 1.5,
     xlab = xlab, ylab = ylab, xaxs = "i", yaxs = "i", cex.lab = 1.3)
text(2,7,pos = 4, "IR = slope of line = .36", cex = 1.5)


##  Table 2.6

wGmv = z1/cc
w1 = z2/a
mu1 = b/a
sigmaA = c(.02,.05,.10)
wA = as.matrix((IR*sigmaA/(mu1 - muGmv))%*%t(w1-wGmv))
rowSum <- apply(wA,1,sum)
wA <- cbind(wA,rowSum)
wA <- round(wA,3)
wA.df = data.frame(wtA1=wA[,1],wtA2=wA[,2],wtA3=wA[,3],
                   wtAsum=wA[,4])
rnames = c("TE  2% ","TE  5% ","TE 10% ")
row.names(wA.df) = rnames
wA.df


## Figure 2.31
# Actively managed frontier dominated by efficient frontier

volB = 0.12
muB = 0.045
varGmv = sigmaGmv^2
muGmv = muGmv
mu1 = mu1
varB = volB^2
muA = seq(-.02,0.06,.001)
sigmaA = muA/IR
muPA = muB + muA
varA = sigmaA^2
const = (2/(mu1-muGmv))*varGmv*(muB/muGmv-1)
varPA = varB + varA + muA*const
sigmaPA = sqrt(varPA)

# Plot using mathEfrontRiskyMuCov for the efficient frontier
mathEfrontRiskyMuCov(mu,vol,corrMat,efront.only = T, display = T)
lines(sigmaPA,muPA, type = "l", lwd = 1.5)
points(volB,muB,pch = 16,cex = 1.3)
text(volB, muB,"B",pos = 4, cex = 1.3)
text(0.163,0.06,"ACTIVELY MANAGED",pos = 4, cex = 1.1)
arrows(0.165,0.06,sigmaPA[50],muPA[50],length = .1,lwd = 1.5)


##  Figure 2.32

# The two plots in this Figure are from Jorion(2003)
# Proper citation will be added.


##  Figure 2.33

# These two plots were created by the PCRA authors


##  Figure 2.34

#  Left-Hand Plot Code

powerUtilityPlots <- function()
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
powerUtilityPlots()

#  Right-Hand Plot Code

quadraticUtilityPlot <- function()
{
  v = seq(0,1.5,.01)
  u = v-v^2
  ylim = c(-0.7,0.4)
  plot(v,u,type = "l",ylim = ylim, xlab = "v", ylab = "U(v)", lwd = 1.5)
  abline(v = .5, lty = "dotted")
  abline(h = .25, lty = "dotted")
}
quadraticUtilityPlot()


##  Figure 2.35

#  Left-Hand Plot Code

rm1	<- 0.18
Beta1  <- c(1.53, 1.36, 1.24, 1.17, 1.06, 0.92, 0.84, 0.76, 0.63, 0.48)
Mu1    <- c(0.26, 0.22, 0.21, 0.21, 0.18, 0.17, 0.16, 0.15, 0.13, 0.12)
Sigma1 <- c(0.49, 0.43, 0.39, 0.37, 0.33, 0.29, 0.27, 0.24, 0.20, 0.17)
SML1   <- rm1 * Beta1

df1  <- data.frame( Beta1, Mu1, Sigma1, SML1)
df1a <- data.frame(x=1, y = rm1)

p1 <- ggplot(df1) + 
  geom_point(aes(x = Beta1, y = Mu1),  color = "black") + 
  geom_line(aes( x = Beta1, y = SML1), color = "gray20") +
  labs(x = "Beta", y = "Mean Excess Return", title = "1931 \u2013 1965") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate(geom="text", x=1.19, y=0.15,  label="CAPM Security Market Line", color="gray20") +
  annotate(geom="text", x=.78,  y=0.185, label="Market Portfolio", color="dodgerblue4")

p1 + geom_point(data = df1a, aes(x = x, y = y),  
                color = "dodgerblue4", shape = 17, size = 3)
#  Rigt-Hand Plot Code

rm2    <- 0.08
Beta2  <- c(1.50, 1.30, 1.17, 1.09, 1.03, 0.95, 0.87, 0.78, 0.67, 0.51)
Mu2    <- c(0.06, 0.08, 0.08, 0.08, 0.08, 0.08, 0.07, 0.07, 0.07, 0.06)
Sigma2 <- c(0.31, 0.26, 0.24, 0.22, 0.21, 0.19, 0.18, 0.16, 0.14, 0.12)
SML2   <- rm2 * Beta2

df2  <- data.frame( Beta2, Mu2, Sigma2, SML2)
df2a <- data.frame(x=1, y = rm2)

p2 <- ggplot(df2) + 
  geom_point(aes(x = Beta2, y = Mu2),  color = "black")  +
  geom_line(aes( x = Beta2, y = SML2), color = "gray20") +
  ylim(0, 0.12) +
  labs(x = "Beta", y = "Mean Excess Return", title = "1965 \u2013 1991") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate(geom="text", x=1.12, y=0.055, label="CAPM Security Market Line", color="gray20") +
  annotate(geom="text", x=.80,  y=0.095, label="Market Portfolio", color="dodgerblue4")

p2 + geom_point(data = df2a, aes(x = x, y = y),  
                color = "dodgerblue4", shape = 17, size = 3)

##  Figure 2.36

#  Left-Hand Plot Code

ggplot(df1, aes(x = Beta1, y = Sigma1)) + 
  geom_point() +    
  geom_smooth(formula = 'y ~ x', method='lm', se = FALSE, linewidth = 0.6, color = "gray20") +
  labs(x = "Beta", y = "Standard Deviation") +
  geom_text(x = 1.05, y = 0.27, label = "sigma %~~% 0.32 ~ beta", parse=TRUE)

#  Right-Hand Plot Code

ggplot(df2, aes(x = Beta2, y = Sigma2)) + 
  geom_point() +    
  geom_smooth(formula = 'y ~ x', method='lm', se = FALSE, linewidth = 0.6, color = "gray20") +
  labs(x = "Beta", y = "Standard Deviation") +
  geom_text(x = 1.05, y = 0.18, label = "sigma %~~% 0.2 ~ beta", parse=TRUE)


##  Table 2.7

df <- data.frame(matrix(" ", nrow = 6, ncol = 4))
df$X1 <- c("1/31--12/39",  "1/40--12/49", "1/50--12/59", "1/60--12/69", "1/70--12/79","1/80--12/91")
df$X2 <- c(-0.05, 0.03, 0.08, 0.03, 0.01, 0.09)
df$X3 <- c(0.17,  0.10, 0.06, 0.07, 0.10, 0.08)
df$X4 <- c(-0.94,  1.06, 4.25, 1.32, 0.18, 3.90)
#Rename rows and columns and reformat the table
# colnames(df) <- c("Period", "$\\mu_{e}$", "$\\sigma_{e}$", "$t( \\mu )$") # In the text
colnames(df) <- c("Period", "Mean(Excess Return)", "Std. Dev(Excess Return)", "t(Mean(Excess Return))")

df


##  Figure 2.37

stocksCRSPweekly <- getPCRAData("stocksCRSPweekly")
dateRange    <- c("2004-01-01", "2005-12-31")
stockItems <- c("Date", "TickerLast", "CapGroupLast", "Return", 
                "MktIndexCRSP", "Ret13WkBill")
returnsAll <- selectCRSPandSPGMI("weekly",
                                 dateRange = dateRange,
                                 stockItems = stockItems, 
                                 factorItems = NULL, 
                                 subsetType = "CapGroupLast",
                                 subsetValues = "SmallCap", 
                                 outputType = "xts")
returns <- returnsAll[ , 1:10]
tsPlotMP(returns, scaleType = "free",layout = c(2,5),stripText.cex = .45,
         axis.cex = 0.4,lwd = 0.5)


##  Figure 2.38

pspec <- portfolio.spec(assets = names(returns))
pspecFI <- add.constraint(pspec, type = "full_investment")
pspecLO <- add.constraint(portfolio = pspecFI, type = "long_only")
pspecESLO5pct <- add.objective(pspecLO, type = "risk", name = "ES",
                               arguments = list(p = 0.050))

# Increase n.portfolios below for more accurate vertical dot-dash line
lty <- c("dashed", "solid",  "dotted", "dotdash")
col <- c("red", "black", "darkgreen", "darkgreen")
chart.EfficientFrontierCompare(returns, pspecESLO5pct, risk_type = "ES", 
                               guideline = TRUE,  cex.axis = 1.2,
                               match.col = c("StdDev", "ES"),
                               n.portfolios = 10,
                               lwd=c(1.3, 1.4, 1.3, 1.0),
                               col = col, lty = lty,
                               xlim = c(0.02, 0.08), ylim = c(0.0, 0.012), 
                               legend.loc = "topleft", main = NULL)


##  Figure 2.39

chart.EfficientFrontierCompare(returns, pspecESLO5pct, risk_type = "StdDev", 
                               guideline = TRUE,  cex.axis = 1.2,
                               match.col = c("ES", "StdDev"),
                               n.portfolios = 10,
                               lwd=c(1.3, 1.4, 1.3, 1.0),
                               col = col, lty = lty,
                               xlim = c(0.01, 0.06), ylim = c(0.0, 0.012), 
                               legend.loc = "topleft", main = NULL)


##  Figure 2.40

x <- seq(-4.9, 4.9, by = 0.001)
ccopt <- computeTuningPsi_modOpt(0.95)
plot(x, wgt_modOpt(x, cc = ccopt), type = "l",
     xlab = "x", ylab = "",cex = 1.5,cex.lab = 1.5)


##  Figure 2.41

data(edhec)
hfnames <- c("CA","CTA","DIS","EM","EMN","ED","FIA","GM","LSE","MA","RV","SS","FOF")
names(edhec) <- hfnames
retLongFIA <- edhec[,"FIA"]
retFIA <- retLongFIA['1998-01-31/1999-12-31',]
index(retFIA) <- as.yearmon(index(retFIA))

# Plot FIA returns with sample mean and robust mean
mu <- 100*mean(retFIA)
se.mu <- 100*sd(retFIA)/sqrt(24)
x <- locScaleM(retFIA,eff = .95)
muRob <- 100*x$mu
se.muRob <- 100*x$std.mu
plot.zoo(retFIA,type ="b",xlab = "",ylab = "FIA Returns")
abline(h = muRob/100, col = "blue")
abline(h = mu/100, lty = "dashed", col ="red")
legend(1999.2,-.03,legend = c("Robust Mean","Sample Mean"),lwd = c(1,2),
       lty = c("solid","dashed"),col = c("blue","red"), bty = "n", cex = 1.3)


##  Table 2.8

tstat.mu <- mu/se.mu
tstat.muRob <- muRob/se.muRob
SR.classic <- tstat.mu/sqrt(24)
SR.Rob <- tstat.muRob/sqrt(24)
row1 <- round(c(mu,se.mu,tstat.mu,SR.classic),2)
row2 <- round(c(muRob,se.muRob,tstat.muRob,SR.Rob),2)
meanEsts <- data.frame(rbind(row1,row2))
names(meanEsts) <- c("Estimate (%)", "Std. Error (%)", "t-Stat","Sharpe Ratio")
row.names(meanEsts) <- c("Sample Mean", "Robust Mean")
meanEsts


##  Figure 2.42

# mOpt 95% Efficiency Tuning Constant
ccModOpt <- computeTuningPsi_modOpt(0.95)

# mOpt M-scale weight function
wgt_mOptScale <- function(x) {
  rho_modOpt(x, cc = ccModOpt)/(x^2)}
wgt_mOptScaleMax <- wgt_mOptScale(0.0001)

# Plot mOptScale Weight Function
x <- seq(-5.5, 5.5, 0.01)
ylim <- c(0, 1.4)
ylab <- "w_scale,mOpt(x)"
plot(x,wgt_mOptScale(x)/wgt_mOptScaleMax, ylim = ylim, 
     ylab = ylab, type = "l", cex.lab = 1.1)
abline(h = 1.0, lty = "dotted")


##  Figure 2.43

data(edhec, package = "PerformanceAnalytics")
hfnames <- c("CA","CTA","DIS","EM","EMN","ED","FIA","GM","LSE","MA","RV","SS","FOF")
names(edhec) <- hfnames
range(index(edhec))
edhec <- edhec[ , 1:12]
returns <- 100*edhec['1998-01-31/1999-12-31']

# Plot edhec returns for 1998-1999
tsPlotMP(returns, type = "l", stripText.cex = 0.7, axis.cex = 0.7)


##  Table 2.9

StdDev <- apply(returns,2,sd)
MADM <- apply(returns,2,mad)
resid <- returns - median(returns)
RobSD <- apply(resid, 2, scaleM, family = "mopt")
SDestsMat <- cbind(StdDev, MADM, RobSD)
SDestsMat <- round(SDestsMat,2)
SDests <- data.frame(hfnames[1:12], SDestsMat)
names(SDests) <- c("HFindex", "StdDev", "MADM", "RobSD")
row.names(SDests) <- NULL
SDests
######
# You first need to install all the R packages listed as arguments to the
# library(packageName) functions below in lines 20 to 32, as follows:
# 
# Install the devtools package using the RStudio Tools/Install Packages
# drop-down menu, or with install.packages("devtools") in the RStudio Console.
#
# Next install PCRA with the devtools function at the RStudio Console with:
# devtools::install_github("robustport/PCRA")
#
# Then install all the other packages below, except the optimalRhoPsi package,
# using the RStudio Tools/Install Packages drop-down menu.
#
# Finally, install the optimalRhoPsi package at the RStudio Console with
# devtools::install_github("kjellpk/optimalRhoPsi")
#
# NOW YOU ARE READY TO RUN THE CH 2 REPRODUCIBILITY CODE BELOW
######

library(PCRA)          ## Install with devtools as above
library(data.table)
library(xts)
library(PerformanceAnalytics) 
library(PortfolioAnalytics)
library(foreach)
library(CVXR)
library(RPESE)
library(RPEIF)
library(ggplot2)
library(dplyr)
library(RobStatTM)
library(optimalRhoPsi)  ## Install with devtools as above


##  Table 2.1

# Largecaps
stockItems <- c("Date","TickerLast","CapGroupLast","Return")
dateRange <- c("1993-01-31","2015-12-31")
returns <- selectCRSPandSPGMI("monthly",dateRange = dateRange, stockItems =
                                stockItems, factorItems = NULL, 
                              subsetType = "CapGroupLast",
                              subsetValues = "LargeCap", outputType = "xts")

ret <- coredata(returns) # Package xts
n <- ncol(ret)
acfLarge <- rep(0,n)
for(i in 1:n){
  acfLarge[i] <- acf(ret[,i], lag.max = 1, 
                     plot = FALSE)$acf[2]
}
muLarge <- mean(acfLarge)
sdLarge <- sd(acfLarge)

# Midcaps
returns <- selectCRSPandSPGMI("monthly",dateRange = dateRange, stockItems =
                                stockItems, factorItems = NULL, subsetType = "CapGroupLast",
                              subsetValues = "MidCap", outputType = "xts")
ret <- coredata(returns) # Package xts
n <- ncol(ret)
acfMid <- rep(0,n)
for(i in 1:n){
  acfMid[i] <- acf(ret[,i], lag.max = 1, 
                   plot = FALSE)$acf[2]
}
muMid <- mean(acfMid)
sdMid <- sd(acfMid)

## Smallcaps
returns <- selectCRSPandSPGMI("monthly",dateRange = dateRange, stockItems =
                                stockItems, factorItems = NULL, subsetType = "CapGroupLast",
                              subsetValues = "SmallCap", outputType = "xts")
ret <- coredata(returns) # Package xts
n <- ncol(ret)
acfSmall <- rep(0,n)
for(i in 1:n){
  acfSmall[i] <- acf(ret[,i], lag.max = 1, 
                     plot = FALSE)$acf[2]
}
muSmall <- mean(acfSmall)
sdSmall <- sd(acfSmall)

## Microcaps
returns <- selectCRSPandSPGMI("monthly",dateRange = dateRange, stockItems =
                                stockItems, factorItems = NULL, subsetType = "CapGroupLast",
                              subsetValues = "MicroCap", outputType = "xts")
ret <- coredata(returns) # Package xts
n <- ncol(ret)
acfMicro <- rep(0,n)
for(i in 1:n){
  acfMicro[i] <- acf(ret[,i], lag.max = 1, 
                     plot = FALSE)$acf[2]
}

muMicro <- mean(acfMicro)
sdMicro <- sd(acfMicro)

dat <- cbind(c(muLarge, sdLarge), c(muMid, sdMid),
             c(muSmall, sdSmall), c(muMicro, sdMicro))
dat <- round(dat,3)
dat <- data.frame(dat)
names(dat) <- c("LargeCap","MidCap","SmallCap","MicroCap")
row.names(dat) <- c("  Mean Lag-1 Acf", 
                    "StdDev Lag-1 Acf")
dat


##  Figure 2.1

dat <- list(acfLarge, acfMid, acfSmall, acfMicro)
names(dat) <- c("87 LargeCaps","67 MidCaps","106 SmallCaps","34 MicroCaps")
boxplot(dat, varwidth = TRUE, col = "cyan")



##  Figure 2.2

returns <- PerformanceAnalytics::edhec
returns <- returns["2002-01-31/2019-12-31", -13]
names(returns) <- c("CA","CTA","DIST","EM","EMN","ED","FIA",
                    "GM","LSE","MA","RV","SS")
PCRA::tsPlotMP(returns)
# range(index(returns))
Ret <- coredata(returns)
n <- ncol(Ret)
acfRet <- rep(0,n)
for(i in 1:n){
  acfRet[i] <- acf(Ret[,i], lag.max = 1, 
                   plot = FALSE)$acf[2]
}
hist(acfRet, main = "EDHEC Hedge Fund Indexes",
     xlab = "Lag-1 ACF Values")


## Figure 2.3

# names(acfRet) <- names(returns)
# (names(sort(acfRet)[1:4]))
# delete "CTA", "GM", "SS", "EMN"

returns8 <- returns[ , c("CA","DIST","EM","ED",
                         "FIA","LSE","MA","RV")]
PCRA::tsPlotMP(returns8, yname = "RETURNS", 
               stripText.cex = 0.7, axis.cex = 0.7)



##  Figure 2.4

acf(returns8$EM, main = "EM", lag.max = 5)



##  Figure 2.5

muVol <- c(.20,.10,.15,.04)
wts <- seq(0,1,.01)
efront2Asset <- function(wts,rho,muVol = c(.20,.10,.15,.04))
{
  sigma1 <- muVol[1]
  mu1 <- muVol[2]
  sigma2 <- muVol[3]
  mu2 <- muVol[4]
  n <- length(wts)
  efront <- data.frame(matrix(rep(0,3*n),ncol = 3))
  names(efront) <- c("SIGMA","MU","WTS")
  w <- wts
  for(i in 1:n){
    mu <- w[i]*mu1 + (1-w[i])*mu2
    var <- w[i]^2*sigma1^2 + 2*w[i]*(1-w[i])*rho*sigma1*sigma2 + (1-w[i])^2*sigma2^2
    sigma <- sqrt(var)
    efront[i,] <- c(sigma,mu,w[i])
  }
  return(efront)
}
ef <- efront2Asset(wts,0,muVol = muVol)
gmv <- ef[ef$SIGMA == min(ef$SIGMA),]
xlab <- expression(sigma [P])
ylab <- expression(mu [P])
par(pty = "s")
plot(ef$SIGMA,ef$MU,type = "l", xlab = xlab, ylab = ylab,
     xlim=c(0,.25),ylim=c(0.03,.11),lwd = 2, cex.lab = 1.5)
points(muVol[c(1,3)], muVol[c(2,4)], pch = 19, cex = 1.3)
points(gmv,pch = 19, cex = 1.3)
text(.04,.10,expression(paste(rho, " = 0")),cex = 1.5)
text(0.12,.0616,adj = c(1,NA),"MinRisk   ",cex = 1.1)
text(0.13,.0616,adj = c(0,NA), "(.12, .0616)",cex = 1.1)
text(0.2,.1,adj = c(0,NA),"  (.20, .10)",cex = 1.1)
text(0.15,.04,adj = c(0,NA),"  (.15, .04)",cex = 1.1)


##  Figure 2.6

muVol <- c(.20,.10,.15,.04)
wts <- seq(0,1,.01)
ef <- efront2Asset(wts,0,muVol = muVol)
ef1 <- efront2Asset(wts,1,muVol = muVol)
ef2 <- efront2Asset(wts,-1,muVol = muVol)
gmv <- ef[ef$SIGMA == min(ef$SIGMA),]
gmv2 <- ef2[ef2$SIGMA == min(ef2$SIGMA),]
xlab <- expression(sigma [P])
ylab <- expression(mu [P])
par(pty = "s")
plot(ef$SIGMA,ef$MU,type = "l", xlab = xlab, ylab = ylab,
     xlim=c(0,.25),ylim=c(0.03,.11),lwd = 2, cex.lab = 1.5)
points(muVol[c(1,3)], muVol[c(2,4)], pch = 19, cex = 1.5)
points(gmv,pch = 19, cex = 1.3)
text(0.2,.1,adj = c(0,NA),"  (.20, .10)",cex = 1.1)
text(0.15,.04,adj = c(0,NA),"  (.15, .04)",cex = 1.1)
lines(ef1$SIGMA,ef1$MU,lty = 2,lwd = 2)
lines(ef2$SIGMA,ef2$MU,lty = 2,lwd = 2)
points(gmv2,pch = 19, cex = 1.3)
text(.12,.07,expression(paste(rho, " = 0 ")),adj = c(1,NA),cex = 1.5)
text(.02,.08,expression(paste(rho, " = -1  ")),adj = c(0,NA),cex = 1.5)
text(.18,.07,expression(paste(rho, " = +1 ")),adj = c(0,NA),cex = 1.5)


##   Figure 2.7

muVol <- c(.20,.10,.15,.04)
wts   <- seq(0,1,.01)
efLO  <- efront2Asset(wts,0,muVol = muVol)
wts   <- seq(1,1.25,.01)
efSS  <- efront2Asset(wts,0,muVol = muVol)
gmv   <- ef[ef$SIGMA == min(ef$SIGMA),]
maxMu <- ef[ef$MU == max(ef$MU),]
maxMuSS <- efSS[efSS$MU == max(efSS$MU),]
xlab  <- expression(sigma [P])
ylab  <- expression(mu [P])
par(pty = "s")
plot(efLO$SIGMA,efLO$MU,type = "l", xlab = xlab, ylab = ylab,
     xlim=c(0,.40),ylim=c(.02,.13),lwd = 2, cex.lab = 1.5)
lines(efSS$SIGMA,efSS$MU,lty = "dashed", lwd = 2)
points(gmv[1:2],pch = 19, cex = 1.3)
points(maxMu[1:2], pch = 19, cex = 1.3)
points(maxMuSS[1:2], pch = 19, cex = 1.3)
text(.04,.12,expression(paste(rho, " = 0")),cex = 1.5)
text(gmv[1:2],adj = c(0,NA),
     paste("  (",toString(round(gmv[1:2],2)),")"),cex = 1.1)
text(maxMu[1:2],adj = c(0,NA),
     paste("  (",toString(maxMu[1:2]),")"),cex = 1.1)
text(maxMuSS[1:2],adj = c(0,NA), 
     paste("  (",toString(round(maxMuSS[1:2],2)),")"), cex = 1.1)


##   Figure 2.8

volMu1 <- c(.20,.10)
volMu2 <- c(.15,.04)
volMu3 <- c(.10,.02)
names(volMu1) <- c("SIGMA","MU")
names(volMu2) <- c("SIGMA","MU")
names(volMu3) <- c("SIGMA","MU")
wts <- seq(0,1,.01)
ef1 <- efront2Asset(wts,0,muVol = c(volMu1,volMu2))
ef2 <- efront2Asset(wts,0,muVol = c(volMu1,volMu3))
ef3 <- efront2Asset(wts,0,muVol = c(volMu2,volMu3))
xlab <- expression(sigma [P])
ylab <- expression(mu [P])
par(pty = "s")
plot(ef1$SIGMA,ef1$MU,type = "l", xlab = xlab, ylab = ylab,
     xlim=c(0,.25),ylim=c(0,.11),lwd = 2, cex.lab = 1.5)
lines(ef2$SIGMA,ef2$MU,lty = 2, lwd = 2.0)
lines(ef3$SIGMA,ef3$MU,lty = 3, lwd = 2.0)
xy <- rbind(volMu1,volMu2,volMu3)
points(xy, pch = 19, cex = 1.3)
text(volMu1[1]+0.01,volMu1[2],adj = c(0,NA),toString(volMu1),cex = 1.1)
text(volMu2[1]+0.01,volMu2[2],adj = c(0,NA),toString(volMu2),cex = 1.1)
text(volMu3[1]+0.01,volMu3[2],adj = c(0,NA),toString(volMu3),cex = 1.1)
text(.04,.10,expression(paste(rho, " = 0")),cex = 1.5)


##  Figure 2.9

volMu1 <- c(.20,.10)
volMu2 <- c(.15,.04)
volMu3 <- c(.10,.02)
vol <- c(volMu1[1],volMu2[1],volMu3[1])
mu <- c(volMu1[2],volMu2[2],volMu3[2])
corrMat0 <- matrix(rep(0,9),nrow = 3)+ diag(rep(1,3))
covMat0 <- diag(vol)%*%corrMat0%*%diag(vol)
n <- 500
port <- matrix(rep(0,2*n),ncol = 2)
dimnames(port)[[2]] = c("SIG.P","MU.P")
wts = hitandrun::simplex.sample(3,n)$samples
for(i in 1:n) {
  x <- wts[i,] 
  port[i,1] <- sqrt(x%*%covMat0%*%x)
  port[i,1]
  port[i,2] <- x%*%mu
}
xlab <- expression(sigma [P])
ylab <- expression(mu [P])
plot(port[,1],port[,2],xlim = c(0,.25),ylim = c(0,.11),
     xlab = xlab, ylab = ylab, pch = 20, cex = .7, cex.lab = 1.5)
points(vol,mu, pch = 19, cex = 1.3)
text(volMu1[1]+0.01,volMu1[2],adj = c(0,NA),toString(volMu1),cex = 1.1)
text(volMu2[1]+0.01,volMu2[2],adj = c(0,NA),toString(volMu2),cex = 1.1)
text(volMu3[1]+0.01,volMu3[2],adj = c(0,NA),toString(volMu3),cex = 1.1)
text(.04,.10,expression(paste(rho, " = 0")),cex = 1.5)


##  Example 2.3

muRet = c(.10,.04,.02)
volRet = c(.20,.15,.10)
corrRet = diag(c(1,1,1))
PCRA::mathGmvMuCov(muRet,volRet,corrRet,digits = 3)


##  Figure 2.10

# There is currently no code for this Figure, and while
# IBM and XOM are in our CRSP data, GE is not.
# Will consider replacing GE with a stock in the CRSP data.


## Example 2.6 Figures 2.8 - 2.10 with the following steps
# Get xts object of 106 smallcap stocks, and the Market ("MktIndexCRSP") in
# stocksCRSP for 1997 - 2010, and use the third group of 10 of these
# to compute Gmv portfolios. Change name "MktIndexCRSP" to "Market".


## Figure 2.11

library(data.table)
stockItems <- c("Date","TickerLast","CapGroupLast","Return","MktIndexCRSP")
dateRange <- c("1997-01-31","2010-12-31")
stocksDat <- PCRA::selectCRSPandSPGMI("monthly",dateRange = dateRange, stockItems =
                                        stockItems, factorItems = NULL, subsetType = "CapGroupLast",
                                      subsetValues = "SmallCap", outputType = "xts")

returns10Mkt <- stocksDat[, c(21:30,107)]
names(returns10Mkt)[11] <- "Market"
tsPlotMP(returns10Mkt,scaleType = "free",layout = c(2,6),stripText.cex = .45,
         axis.cex = 0.4,lwd = 0.5)



##  Figures 2.12 and 2.13

# Load PortfolioAnalytics and related packages, and compute time
# series of GmvLO and GmvLS portfolio weights wtsGmvLO and wtsGmvLS

# Create GmvLS portfolio specs
returns <- returns10Mkt[,1:10]
Market  <- returns10Mkt[,11]
funds   <- colnames(returns)

pspec       <- portfolio.spec(assets=funds)
pspec.fi    <- add.constraint(pspec, type="full_investment")
pspec.gmvLS <- add.objective(pspec.fi, type="risk", name="var")

# Optimize Portfolio at Monthly Rebalancing and 5-Year Training
bt.gmvLS <- optimize.portfolio.rebalancing(returns, pspec.gmvLS,
                                           optimize_method="CVXR",
                                           rebalance_on="months",
                                           training_period=60,
                                           rolling_window=60,
                                           trace = TRUE)
# Extract time series of portfolio weights
wtsGmvLS <- extractWeights(bt.gmvLS)

# Compute rebalancing GmvLS arithmetic returns
GmvLS <- Return.rebalancing(returns,wtsGmvLS)

# Combine GmvLS and Market returns and plot their time series
ret.comb <- na.omit(merge.xts(GmvLS, Market, all=F))
names(ret.comb) <- c("GmvLS","Market")


# Figure 2.12
tsPlotMP(wtsGmvLS,layout = c(2,5),scaleType = "same",
         stripText.cex = 0.7, axis.cex = .7)


# Figure 2.13
tsPlotMP(ret.comb,scaleType = "same",stripText.cex = .7, axis.cex = .7)


##  Figure 2.14
# Compute cumulative gross portfolio returns

R <- ret.comb
geometric <- TRUE
c.xts <- if ( geometric ) {
  cumprod(1+R)
} else {
  1 + cumsum(R)
}

# Plot cumulative gross returns of GmvLS and Market portfolios
# Original code contributed by Peter Carl

p <- plot.xts(c.xts[,1], col="black", main = "Cumulative Returns",
              grid.ticks.lwd=1, grid.ticks.lty = "dotted", grid.ticks.on = "years",
              labels.col="grey20", cex.axis=0.8, format.labels = "%b\n%Y",
              ylim = c(min(c.xts), max(c.xts)))
p <- addSeries(c.xts[,2], on=1, lwd=2, col="darkred", lty="dashed")
p <- addLegend("topleft", on = 1,
               legend.names = names(c.xts),
               lty = c(1,2), lwd = rep(2, NCOL(c.xts)),
               col = c("black", "darkred"),
               bty = "o", box.col = "white",
               bg=rgb(t(col2rgb("white")), alpha = 200,
                      maxColorValue = 255) )
d.xts <- PerformanceAnalytics::Drawdowns(R)
p <- xts::addSeries(d.xts[,1], col="darkblue", lwd=2, main="Drawdown",
                    ylim = c(min(d.xts), 0) )
p <- xts::addSeries(d.xts[,2], on=2, lwd=2, col="darkred", lty="dashed")

# panel 1 and 2 ylim
## ylim1 <- c(p$Env$ylim[[2]][1], p$Env$ylim[[2]][2]) No longer works
## ylim2 <- c(p$Env$ylim[[4]][1], p$Env$ylim[[4]][2]) No longer works

ylim1 <- p$Env$panels[[1]]$ylim
ylim2 <- p$Env$panels[[2]]$ylim
ylim <- c(ylim1, ylim2)

# get longest drawdown dates for xts object
dt <- table.Drawdowns(R, top = 1) # just want to find the worst drawdown
dt2 <- t(dt[,c("From", "To")])
x <- as.vector(dt2[,NCOL(dt2)])
y <- as.xts(matrix(rep(ylim, length(x)),ncol=length(ylim), byrow=TRUE), 			
            order.by=as.Date(x))
i=1
p <- xts::addPolygon(y[i:(i+1),1:2], on=-1, col="lightgrey") # top panel
p <- xts::addPolygon(y[i:(i+1),3:4], on=-2, col="lightgrey") # lower panel
p


##  Table 2.2

dt2mat <- table.Drawdowns(R, top = 2) # find the worst two drawdowns
dt2mat[,4] <- round(dt2mat[,4],2)
dt2 <- data.frame(dt2mat)[,1:5]
names(dt2) <- c("Begin","Minimum","End","Depth","Months")
dt2 <- dt2[,c(1:3,5,4)]
dt2


## Figure 2.15

levg <- levgLongShort(wtsGmvLS)
plot.zoo(levg,ylim = c(0.0,1.5),ylab = "Leverage")
abline(h = 1.0,lty = "dotted")



##  Figure 2.16 Left-Hand Plot

GmvLS.TO <- 100*turnOver(wtsGmvLS)
plot.zoo(GmvLS.TO,ylim = c(0,60), ylab = "TURNOVER (%)",
         xlab = "", cex.axis = 1.5, cex.lab = 1.5)
abline(h = mean(GmvLS.TO),lty = "dashed")
text(as.Date("2004-01-31"),50,"Mean Turnover = 13.4 (%)", cex = 1.5)


## Figure 2.16 Right-Hand Plot

GmvLS.DIV <- 100*divHHI(wtsGmvLS)
plot.zoo(GmvLS.DIV,ylim = c(0,100),lwd = 1.5, ylab = "DIV(%)",
         xlab = "", cex.axis = 1.5, cex.lab = 1.5)
abline(h = mean(GmvLS.DIV),lty = "dashed")
text(as.Date("2006-01-31"),90,"Mean Diversification = 63.5 (%)",
     cex = 1.5)


##  Table 2.3
# Risk & Performance Estimator Standard Errors package

SD12 <- SD.SE(ret.comb, se.method = "IFiid")
SD12 <- printSE(SD12, round.digit = 4)
SSD12 <- SemiSD.SE(ret.comb, se.method = "IFiid")
SSD12 <- printSE(SSD12, round.digit = 4)
ES12 <- ES.SE(ret.comb, se.method = "IFiid")
ES12 <- printSE(ES12, round.digit = 4)
VaR12 <- VaR.SE(ret.comb, se.method = "IFiid")
VaR12 <- printSE(VaR12, round.digit = 4)
# VaR12[,1] <- -VaR12[,1]
RM <- 100*rbind(SD12,SSD12,ES12,VaR12)
colnames(RM)  <- c("Estimate (%)","StdError (%)")
rownames(RM) <- c("GmvLS SD","Market SD",
                  "GmvLS SSD","Market SSD",
                  "GmvLS ES","Market ES",
                  "GmvLS VaR","Market VaR")
RM <- as.data.frame(RM)
RM

##  Figure 2.17
# Risk-free rates were not negligible before 2009

stockItems <- c("Date", "TickerLast", "Return", "Ret13WkBill")
returnsAll <- selectCRSPandSPGMI("monthly", stockItems = stockItems,  
                                 factorItems = NULL, outputType = "xts")
riskFree <- returnsAll[ , "Ret13WkBill"]
tsPlotMP(riskFree, yname = "RISK-FREE RATE")


##  Figure 2.18
# Variation of risk-free rate for 1995 through 2000

returns <- returnsAll["1995-01-31/2000-12-31"]
x <- sort(apply(returns,2,mean))
x0 <- x[x <= 0.007 & x >= 0.005] # Results in 21 stocks & choose FMC
ret <- returns[, c("FMC", "Ret13WkBill")]
names(ret)[2] <- "Risk-Free"
tsPlotMP(ret, yname = "RETURNS", scaleType = "free")


##  Table 2.4

SR12 <- SR.SE(ret.comb, se.method = "IFiid")
SR12 <- printSE(SR12, round.digit = 2)
DSR12 <- DSR.SE(ret.comb, se.method = "IFiid")
DSR12 <- printSE(DSR12, round.digit = 2)
SoR12 <- SoR.SE(ret.comb, se.method = "IFiid")
SoR12 <- printSE(SoR12, round.digit = 2)
ESratio12 <- ESratio.SE(ret.comb, se.method = "IFiid")
ESratio12 <- printSE(ESratio12, round.digit = 2)
Ratios <- rbind(SR12,DSR12,SoR12,ESratio12)
colnames(Ratios)  <- c("Estimate","StdError")
rownames(Ratios) <- c("GmvLS SR","Market SR",
                      "GmvLS DSR","Market DSR",
                      "GmvLS SOR","Market SOR",
                      "GmvLS ESR","Market ESR")
Ratios <- as.data.frame(Ratios)
Ratios

##  Figure 2.19

data(edhec, package = "PerformanceAnalytics")
colnames(edhec) <- c("CA", "CTAG", "DIS", "EM", "EMN", "ED", "FIA", 
                     "GM", "LS", "MA", "RV", "SS", "FoF")
par(mfrow = c(1, 2))
outSD <- IF.SD(returns = edhec$CA, evalShape = T, IFplot = T)
outSR <- IF.SemiSD(returns = edhec$CA, evalShape = T, IFplot = T)
par(mfrow = c(1, 1))


##  Figure 2.20

muRet = c(.10,.04,.02)
volRet = c(.20,.15,.10)
corrRet = diag(c(1,1,1))
mathEfrontRiskyMuCov(muRet,volRet,corrRet,efront.only = F)
text(0.07, 0.095, "EFFICIENT FRONTIER", cex = 1.2)
arrows(0.07, 0.09, .10, .06, length = 0.1, lwd= 1.0)



##  Table 2.5

muRet = c(.10,.04,.02)
volRet = c(.20,.15,.10)
corrRet = diag(c(1,1,1))
efront = mathEfrontRiskyMuCov(muRet,volRet,corrRet, npoints = 5, values = T, display = F)
mu.efront = efront$mu.efront
wtsEfront <- mathWtsEfrontRiskyMuCov(muRet,volRet,corrRet,mu.efront,digits = 3)
wtsEfront


##  Figure 2.21

nColor <- 4
barplotWts(as.matrix(wtsEfront), legend.text = T, ylab = "WEIGHTS",
           col = topo.colors(nColor), bar.ylim = c(-1, 2),cex.lab = 1.2,
           cex.axis = 1.3)


##  Figure 2.22

returns10 <- returns10Mkt[,-11]
efront <- mathEfrontRisky(returns10, display = T, cexGmv = 1.0,
                          cexPoints = 1.1, cexText = 0.9)



##  Figure 2.23

efront10 <- mathEfrontRisky(returns10,npoints = 5, display = F, values = TRUE)
mu.efront <- efront10$mu.efront
wtsEfront <- mathWtsEfrontRisky(returns10, mu.efront,digits = 3)
barplotWts(as.matrix(wtsEfront), legend.text = T, ylab = "WEIGHTS",
           col = topo.colors(10), bar.ylim = c(-1.5,3.0),cex.lab = 1.2,
           cex.axis = 1.3)


##  Figure 2.24

stockItems <- c("Date","TickerLast","CapGroupLast","Return","Ret13WkBill")
dateRange <- c("1997-01-31","2010-12-31")
stocksDat <- selectCRSPandSPGMI("monthly",dateRange = dateRange, 
                                stockItems = stockItems, factorItems = NULL, 
                                subsetType = "CapGroupLast",
                                subsetValues = "SmallCap", outputType = "xts")
returns10andRF <- stocksDat[, c(21:30,107)]
names(returns10andRF)[11] <- "RiskFree"
tsPlotMP(returns10andRF,scaleType = "free",layout = c(2,6),stripText.cex = .45,
         axis.cex = 0.4,lwd = 0.5)


##  Figure 2.25

rf <- mean(returns10andRF[,11])
returns10 <- returns10andRF[,-11]
mathEfrontCashRisky(returns10, rf = rf, cexPoints = 1.0)


##  Figure 2.26

rf <- mean(returns10andRF[,11])
returns10 <- returns10andRF[,-11]
wtsEfront = mathEfrontCashRisky(returns10, plot.efront = FALSE, values = TRUE)
barplotWts(as.matrix(wtsEfront),legend.text = T,col = topo.colors(3),
           ylab = "Weights",xlab = "VOL", bar.ylim = c(-0.5,1.5))



##  Calculation of risk aversion and risk tolerance values for Figure 2.25,
##  reported at end of paragraph below (2.154)

rf = .005
C = var(returns10)
mu.stocks = apply(returns10, 2, mean)
mue = mu.stocks - rf
a = solve(C, mue)
lambda = sum(a)
lambda      # Risk aversion value
1/lambda    # Risk tolerance value


##  Figure 2.27

plot(FRBinterestRates, xaxt = "n", xlab = "", ylab = "InterestRates (%)")
axis(side = 1,at = seq(1930,2020,by=10), labels = seq(1930, 2020, by=10))
grid()


##  Figure 2.28

rf <- mean(returns10andRF[,11])
returns10 <- returns10andRF[,-11]
mathEfront(returns10, rf = rf, mu.max = .035, sigma.max = .19, cexText = 0.8, npoints = 100)


##  Figure 2.29

rf        <- 0.03
rf_lend   <- 0.04
rf_borrow <- 0.06
er_port   <- 0.07
leverage  <- seq(0, 2, .1)

er_rf <- rf + leverage * (er_port - rf)
er_1  <- rf_lend   + leverage * (er_port - rf_lend)
er_2  <- rf_borrow + leverage * (er_port - rf_borrow)

df <- data.frame("Leverage" = leverage,
                 "Single Risk Free Rate for Borrowing and Lending"  	= er_rf,
                 "Different Risk Free Rates for Borrowing and Lending"  = pmin(er_1, er_2))

df_melt <- reshape2::melt(df, id.vars =  "Leverage", variable.name = "Risk_Free_Rate")
df_melt[["Risk_Free_Rate"]] <- gsub("\\.", " ", df_melt[["Risk_Free_Rate"]])

ggplot(df_melt, aes(x = Leverage, y = value )) +
  geom_line(aes(color = Risk_Free_Rate, linetype = Risk_Free_Rate), linewidth = 1) +
  labs(x = "Leverage", y = "Expected Return", 
       color = "Risk Free Rate", linetype = "Risk Free Rate") +
  theme_bw() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.4, 0.9),
        legend.title = element_text(size = 16),
        legend.text  = element_text(size = 14),
        axis.text    = element_text(size = 12),
        axis.title   = element_text(size = 14))


##  Figure 2.30

volMu1 = c(.20,.10)
volMu2 = c(.15,.04)
volMu3 = c(.10,.02)
vol = c(volMu1[1],volMu2[1],volMu3[1])
mu = c(volMu1[2],volMu2[2],volMu3[2])
corrMat = matrix(rep(0,9),nrow = 3)+ diag(rep(1,3))
V = diag(vol)%*%corrMat%*%diag(vol)
# Compute IR for the three stocks
one = rep(1, nrow(V))
z1 = solve(V, one) # Vinv*one
z2 = solve(V, mu)  # Vinv*mu
a = as.numeric(t(mu) %*% z1)   # a = mu*Vinv*one
b = as.numeric(t(mu) %*% z2)   # b = mu*Vinv*mu
cc = as.numeric(t(one) %*% z1) # c = one*Vinv*one
d = b * cc - a^2
muGmv = a/cc
sigmaGmv = 1/sqrt(cc)
IR = sigmaGmv*sqrt(d)
# Plot active efficient frontier
sigmaA = seq(0,20,1)
muA = IR*sigmaA
xlab = "Active Volatility (%)"
ylab = "Active Mean Return (%)" 
plot(sigmaA,muA,xlim = c(0,21),ylim = c(0,8.5),type = "l",lwd = 1.5,
     xlab = xlab, ylab = ylab, xaxs = "i", yaxs = "i", cex.lab = 1.3)
text(2,7,pos = 4, "IR = slope of line = .36", cex = 1.5)


##  Table 2.6

wGmv = z1/cc
w1 = z2/a
mu1 = b/a
sigmaA = c(.02,.05,.10)
wA = as.matrix((IR*sigmaA/(mu1 - muGmv))%*%t(w1-wGmv))
rowSum <- apply(wA,1,sum)
wA <- cbind(wA,rowSum)
wA <- round(wA,3)
wA.df = data.frame(wtA1=wA[,1],wtA2=wA[,2],wtA3=wA[,3],
                   wtAsum=wA[,4])
rnames = c("TE  2% ","TE  5% ","TE 10% ")
row.names(wA.df) = rnames
wA.df


## Figure 2.31
# Actively managed frontier dominated by efficient frontier

volB = 0.12
muB = 0.045
varGmv = sigmaGmv^2
muGmv = muGmv
mu1 = mu1
varB = volB^2
muA = seq(-.02,0.06,.001)
sigmaA = muA/IR
muPA = muB + muA
varA = sigmaA^2
const = (2/(mu1-muGmv))*varGmv*(muB/muGmv-1)
varPA = varB + varA + muA*const
sigmaPA = sqrt(varPA)

# Plot using mathEfrontRiskyMuCov for the efficient frontier
mathEfrontRiskyMuCov(mu,vol,corrMat,efront.only = T, display = T)
lines(sigmaPA,muPA, type = "l", lwd = 1.5)
points(volB,muB,pch = 16,cex = 1.3)
text(volB, muB,"B",pos = 4, cex = 1.3)
text(0.163,0.06,"ACTIVELY MANAGED",pos = 4, cex = 1.1)
arrows(0.165,0.06,sigmaPA[50],muPA[50],length = .1,lwd = 1.5)


##  Figure 2.32

# The two plots in this Figure are from Jorion(2003)
# Proper citation will be added.


##  Figure 2.33

# These two plots were created by the PCRA authors


##  Figure 2.34

#  Left-Hand Plot Code

powerUtilityPlots <- function()
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
powerUtilityPlots()

#  Right-Hand Plot Code

quadraticUtilityPlot <- function()
{
  v = seq(0,1.5,.01)
  u = v-v^2
  ylim = c(-0.7,0.4)
  plot(v,u,type = "l",ylim = ylim, xlab = "v", ylab = "U(v)", lwd = 1.5)
  abline(v = .5, lty = "dotted")
  abline(h = .25, lty = "dotted")
}
quadraticUtilityPlot()


##  Figure 2.35

#  Left-Hand Plot Code

rm1	<- 0.18
Beta1  <- c(1.53, 1.36, 1.24, 1.17, 1.06, 0.92, 0.84, 0.76, 0.63, 0.48)
Mu1    <- c(0.26, 0.22, 0.21, 0.21, 0.18, 0.17, 0.16, 0.15, 0.13, 0.12)
Sigma1 <- c(0.49, 0.43, 0.39, 0.37, 0.33, 0.29, 0.27, 0.24, 0.20, 0.17)
SML1   <- rm1 * Beta1

df1  <- data.frame( Beta1, Mu1, Sigma1, SML1)
df1a <- data.frame(x=1, y = rm1)

p1 <- ggplot(df1) + 
  geom_point(aes(x = Beta1, y = Mu1),  color = "black") + 
  geom_line(aes( x = Beta1, y = SML1), color = "gray20") +
  labs(x = "Beta", y = "Mean Excess Return", title = "1931 \u2013 1965") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate(geom="text", x=1.19, y=0.15,  label="CAPM Security Market Line", color="gray20") +
  annotate(geom="text", x=.78,  y=0.185, label="Market Portfolio", color="dodgerblue4")

p1 + geom_point(data = df1a, aes(x = x, y = y),  
                color = "dodgerblue4", shape = 17, size = 3)
#  Rigt-Hand Plot Code

rm2    <- 0.08
Beta2  <- c(1.50, 1.30, 1.17, 1.09, 1.03, 0.95, 0.87, 0.78, 0.67, 0.51)
Mu2    <- c(0.06, 0.08, 0.08, 0.08, 0.08, 0.08, 0.07, 0.07, 0.07, 0.06)
Sigma2 <- c(0.31, 0.26, 0.24, 0.22, 0.21, 0.19, 0.18, 0.16, 0.14, 0.12)
SML2   <- rm2 * Beta2

df2  <- data.frame( Beta2, Mu2, Sigma2, SML2)
df2a <- data.frame(x=1, y = rm2)

p2 <- ggplot(df2) + 
  geom_point(aes(x = Beta2, y = Mu2),  color = "black")  +
  geom_line(aes( x = Beta2, y = SML2), color = "gray20") +
  ylim(0, 0.12) +
  labs(x = "Beta", y = "Mean Excess Return", title = "1965 \u2013 1991") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate(geom="text", x=1.12, y=0.055, label="CAPM Security Market Line", color="gray20") +
  annotate(geom="text", x=.80,  y=0.095, label="Market Portfolio", color="dodgerblue4")

p2 + geom_point(data = df2a, aes(x = x, y = y),  
                color = "dodgerblue4", shape = 17, size = 3)

##  Figure 2.36

#  Left-Hand Plot Code

ggplot(df1, aes(x = Beta1, y = Sigma1)) + 
  geom_point() +    
  geom_smooth(formula = 'y ~ x', method='lm', se = FALSE, linewidth = 0.6, color = "gray20") +
  labs(x = "Beta", y = "Standard Deviation") +
  geom_text(x = 1.05, y = 0.27, label = "sigma %~~% 0.32 ~ beta", parse=TRUE)

#  Right-Hand Plot Code

ggplot(df2, aes(x = Beta2, y = Sigma2)) + 
  geom_point() +    
  geom_smooth(formula = 'y ~ x', method='lm', se = FALSE, linewidth = 0.6, color = "gray20") +
  labs(x = "Beta", y = "Standard Deviation") +
  geom_text(x = 1.05, y = 0.18, label = "sigma %~~% 0.2 ~ beta", parse=TRUE)


##  Table 2.7

df <- data.frame(matrix(" ", nrow = 6, ncol = 4))
df$X1 <- c("1/31--12/39",  "1/40--12/49", "1/50--12/59", "1/60--12/69", "1/70--12/79","1/80--12/91")
df$X2 <- c(-0.05, 0.03, 0.08, 0.03, 0.01, 0.09)
df$X3 <- c(0.17,  0.10, 0.06, 0.07, 0.10, 0.08)
df$X4 <- c(-0.94,  1.06, 4.25, 1.32, 0.18, 3.90)
#Rename rows and columns and reformat the table
# colnames(df) <- c("Period", "$\\mu_{e}$", "$\\sigma_{e}$", "$t( \\mu )$") # In the text
colnames(df) <- c("Period", "Mean(Excess Return)", "Std. Dev(Excess Return)", "t(Mean(Excess Return))")

df


##  Figure 2.37

stocksCRSPweekly <- getPCRAData("stocksCRSPweekly")
dateRange    <- c("2004-01-01", "2005-12-31")
stockItems <- c("Date", "TickerLast", "CapGroupLast", "Return", 
                "MktIndexCRSP", "Ret13WkBill")
returnsAll <- selectCRSPandSPGMI("weekly",
                                 dateRange = dateRange,
                                 stockItems = stockItems, 
                                 factorItems = NULL, 
                                 subsetType = "CapGroupLast",
                                 subsetValues = "SmallCap", 
                                 outputType = "xts")
returns <- returnsAll[ , 1:10]
tsPlotMP(returns, scaleType = "free",layout = c(2,5),stripText.cex = .45,
         axis.cex = 0.4,lwd = 0.5)


##  Figure 2.38

pspec <- portfolio.spec(assets = names(returns))
pspecFI <- add.constraint(pspec, type = "full_investment")
pspecLO <- add.constraint(portfolio = pspecFI, type = "long_only")
pspecESLO5pct <- add.objective(pspecLO, type = "risk", name = "ES",
                               arguments = list(p = 0.050))

# Increase n.portfolios below for more accurate vertical dot-dash line
lty <- c("dashed", "solid",  "dotted", "dotdash")
col <- c("red", "black", "darkgreen", "darkgreen")
chart.EfficientFrontierCompare(returns, pspecESLO5pct, risk_type = "ES", 
                               guideline = TRUE,  cex.axis = 1.2,
                               match.col = c("StdDev", "ES"),
                               n.portfolios = 10,
                               lwd=c(1.3, 1.4, 1.3, 1.0),
                               col = col, lty = lty,
                               xlim = c(0.02, 0.08), ylim = c(0.0, 0.012), 
                               legend.loc = "topleft", main = NULL)


##  Figure 2.39

chart.EfficientFrontierCompare(returns, pspecESLO5pct, risk_type = "StdDev", 
                               guideline = TRUE,  cex.axis = 1.2,
                               match.col = c("ES", "StdDev"),
                               n.portfolios = 10,
                               lwd=c(1.3, 1.4, 1.3, 1.0),
                               col = col, lty = lty,
                               xlim = c(0.01, 0.06), ylim = c(0.0, 0.012), 
                               legend.loc = "topleft", main = NULL)


##  Figure 2.40

x <- seq(-4.9, 4.9, by = 0.001)
ccopt <- computeTuningPsi_modOpt(0.95)
plot(x, wgt_modOpt(x, cc = ccopt), type = "l",
     xlab = "x", ylab = "",cex = 1.5,cex.lab = 1.5)


##  Figure 2.41

data(edhec)
hfnames <- c("CA","CTA","DIS","EM","EMN","ED","FIA","GM","LSE","MA","RV","SS","FOF")
names(edhec) <- hfnames
retLongFIA <- edhec[,"FIA"]
retFIA <- retLongFIA['1998-01-31/1999-12-31',]
index(retFIA) <- as.yearmon(index(retFIA))

# Plot FIA returns with sample mean and robust mean
mu <- 100*mean(retFIA)
se.mu <- 100*sd(retFIA)/sqrt(24)
x <- locScaleM(retFIA,eff = .95)
muRob <- 100*x$mu
se.muRob <- 100*x$std.mu
plot.zoo(retFIA,type ="b",xlab = "",ylab = "FIA Returns")
abline(h = muRob/100, col = "blue")
abline(h = mu/100, lty = "dashed", col ="red")
legend(1999.2,-.03,legend = c("Robust Mean","Sample Mean"),lwd = c(1,2),
       lty = c("solid","dashed"),col = c("blue","red"), bty = "n", cex = 1.3)


##  Table 2.8

tstat.mu <- mu/se.mu
tstat.muRob <- muRob/se.muRob
SR.classic <- tstat.mu/sqrt(24)
SR.Rob <- tstat.muRob/sqrt(24)
row1 <- round(c(mu,se.mu,tstat.mu,SR.classic),2)
row2 <- round(c(muRob,se.muRob,tstat.muRob,SR.Rob),2)
meanEsts <- data.frame(rbind(row1,row2))
names(meanEsts) <- c("Estimate (%)", "Std. Error (%)", "t-Stat","Sharpe Ratio")
row.names(meanEsts) <- c("Sample Mean", "Robust Mean")
meanEsts


##  Figure 2.42

# mOpt 95% Efficiency Tuning Constant
ccModOpt <- computeTuningPsi_modOpt(0.95)

# mOpt M-scale weight function
wgt_mOptScale <- function(x) {
  rho_modOpt(x, cc = ccModOpt)/(x^2)}
wgt_mOptScaleMax <- wgt_mOptScale(0.0001)

# Plot mOptScale Weight Function
x <- seq(-5.5, 5.5, 0.01)
ylim <- c(0, 1.4)
ylab <- "w_scale,mOpt(x)"
plot(x,wgt_mOptScale(x)/wgt_mOptScaleMax, ylim = ylim, 
     ylab = ylab, type = "l", cex.lab = 1.1)
abline(h = 1.0, lty = "dotted")


##  Figure 2.43

data(edhec, package = "PerformanceAnalytics")
hfnames <- c("CA","CTA","DIS","EM","EMN","ED","FIA","GM","LSE","MA","RV","SS","FOF")
names(edhec) <- hfnames
range(index(edhec))
edhec <- edhec[ , 1:12]
returns <- 100*edhec['1998-01-31/1999-12-31']

# Plot edhec returns for 1998-1999
tsPlotMP(returns, type = "l", stripText.cex = 0.7, axis.cex = 0.7)


##  Table 2.9

StdDev <- apply(returns,2,sd)
MADM <- apply(returns,2,mad)
resid <- returns - median(returns)
RobSD <- apply(resid, 2, scaleM, family = "mopt")
SDestsMat <- cbind(StdDev, MADM, RobSD)
SDestsMat <- round(SDestsMat,2)
SDests <- data.frame(hfnames[1:12], SDestsMat)
names(SDests) <- c("HFindex", "StdDev", "MADM", "RobSD")
row.names(SDests) <- NULL
SDests
