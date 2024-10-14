## DATE OF THIS VERSION:  10/13/24
##
## Copy/paste this entire script into your own blank R script file.
## Then run the code for the Figures and Tables of your choice
##
library(PCRA)
library(RobStatTM)

## Figure 2.1
muVol <- c(.20, .10, .15, .04)
wts   <- seq(0, 1, .01)
efront2Asset <- function(wts, rho, muVol = c(.20, .10, .15, .04))
{
  sigma1 <- muVol[1]
  mu1    <- muVol[2]
  sigma2 <- muVol[3]
  mu2    <- muVol[4]
  n <- length(wts)
  efront <- data.frame(matrix(rep(0, 3*n), ncol = 3))
  names(efront) <- c("SIGMA", "MU", "WTS")
  w <- wts
  for(i in 1:n){
    mu <- w[i]*mu1 + (1-w[i])*mu2
    var <- w[i]^2*sigma1^2 + 2*w[i]*(1-w[i])*rho*sigma1*sigma2 + (1-w[i])^2*sigma2^2
    sigma <- sqrt(var)
    efront[i,] <- c(sigma,mu,w[i])
  }
  return(efront)
}
ef   <- efront2Asset(wts,0,muVol = muVol)
gmv  <- ef[ef$SIGMA == min(ef$SIGMA),]
xlab <- expression(sigma [P])
ylab <- expression(mu [P])
par(pty = "s")
plot(ef$SIGMA,ef$MU,type = "l", xlab = xlab, ylab = ylab,
     xlim=c(0, .25), ylim=c(0.03, .11), lwd = 2, cex.lab = 1.5)
points(muVol[c(1,3)], muVol[c(2,4)], pch = 19, cex = 1.3)
points(gmv,pch = 19, cex = 1.3)
text(.04,.10,expression(paste(rho, " = 0")),cex = 1.5)
text(0.12,.0616,adj = c(1,NA),"MinRisk   ",cex = 1.1)
text(0.13,.0616,adj = c(0,NA), "(.12, .0616)",cex = 1.1)
text(0.2,.1,adj = c(0,NA),"  (.20, .10)",cex = 1.1)
text(0.15,.04,adj = c(0,NA),"  (.15, .04)",cex = 1.1)


## Figure 2.2
muVol <- c(.20, .10, .15, .04)
wts   <- seq(0, 1, .01)
ef    <- efront2Asset(wts,0,muVol = muVol)
ef1   <- efront2Asset(wts,1,muVol = muVol)
ef2   <- efront2Asset(wts,-1,muVol = muVol)
gmv   <- ef[ef$SIGMA == min(ef$SIGMA),]
gmv2  <- ef2[ef2$SIGMA == min(ef2$SIGMA),]
xlab  <- expression(sigma [P])
ylab  <- expression(mu [P])
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
text(.12, .07, expression(paste(rho, " = 0 ")), adj = c(1,NA), cex = 1.5)
text(.02, .08, expression(paste(rho, " = -1  ")), adj = c(0,NA), cex = 1.5)
text(.18, .07, expression(paste(rho, " = +1 ")), adj = c(0,NA), cex = 1.5)


## Figure 2.3
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


## Figure 2.4
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


## Figure 2.5
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


## Example 2.3
library(PCRA)
muRet = c(.10,.04,.02)
volRet = c(.20,.15,.10)
corrRet = diag(c(1,1,1))
mathGmvMuCov(muRet,volRet,corrRet,digits = 3)


## Figure 2.6
# This code for this Figure 2.6 does not work because the returns
# data WeeklyReturns1989 is not contained in stocksCRSP data set
# If you want to create this Figure 2.6 and the following Example
# 2.5, you need to have access to CRSP stocks data and create
# a WeeklyReturns1980 xts object from November 7, 1980 to October 30,
# which contains stocks with tickers IBM, GE and XOM 

# ret <- WeeklyReturns1980[,1:3]
# tsPlotMP(ret,layout = c(1,3),type = "l",lwd = 1, color ="blue",
#         scaleType = "same", stripText.cex = .7, axis.cex = .7)


## Example 2.5
# This code does not work for the reasons stated for Figure 2.6 above

# retMat <- zoo::coredata(ret)
# gmv = mathGmv(retMat)
# round(gmv$wts,3)
# meanGMV = round(100*52*gmv$mu,1)
# volGMV = round(100*sqrt(52)*gmv$vol,1)
# data.frame(cbind(meanGMV,volGMV),row.names = "")


## Example 2.6 Figures 2.7 - 2.10
# Get data.table of 106 smallcap stocks in stocksCRSP for 1997 - 2010
# and choose the third group of 10 of these stocks as the Gmv portfolio
library(PCRA)
library(data.table)
stockItems <- c("Date", "TickerLast", "CapGroupLast", "Return","MktIndexCRSP")
dateRange  <- c("1997-01-31", "2010-12-31")
stocksDat  <- selectCRSPandSPGMI("monthly",
                                 dateRange    = dateRange, 
                                 stockItems   = stockItems, 
                                 factorItems  = NULL, 
                                 subsetType   = "CapGroupLast",
                                 subsetValues = "SmallCap", 
                                 outputType   = "xts")
returns10Mkt <- stocksDat[, c(21:30,107)]
names(returns10Mkt)[11] <- "Market"
tsPlotMP(returns10Mkt,scaleType = "free",layout = c(2,6),stripText.cex = .45,
         axis.cex = 0.4,lwd = 0.5)


## Load PortfolioAnalytics and related packages, and compute time
## series of GmvLO and GmvLS portfolio weights wtsGmvLO and wtsGmvLS
library(PortfolioAnalytics)
library(ROI)
library(quadprog)
library(ROI.plugin.quadprog)
library(PerformanceAnalytics)

# Create GmvLS portfolio specs
returns <- returns10Mkt[,1:10]
Market  <- returns10Mkt[,11]
funds   <- colnames(returns)

pspec       <- portfolio.spec(assets=funds)
pspec.fi    <- add.constraint(pspec, type="full_investment")
pspec.gmvLS <- add.objective(pspec.fi, type="risk", name="var")

# Optimize Portfolio at Monthly Rebalancing and 5-Year Training
bt.gmvLS <- optimize.portfolio.rebalancing(returns, pspec.gmvLS,
                                           optimize_method="quadprog",
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


## Figure 2.8
tsPlotMP(wtsGmvLS,layout = c(2,5),scaleType = "same",
         stripText.cex = 0.7, axis.cex = .7)


## Figure 2.9
tsPlotMP(ret.comb,scaleType = "same",stripText.cex = .7, axis.cex = .7)

## Compute cumulative gross portfolio returns
R <- ret.comb
geometric <- TRUE
c.xts <- if ( geometric ) {
  cumprod(1+R)
} else {
  1 + cumsum(R)
}

## Figure 2.10
# Cumulative gross returns of GmvLS and Market portfolios
# Code contributed by Peter Carl
p <- xts::plot.xts(c.xts[,1], col="black", main = "Cumulative Returns",
                   grid.ticks.lwd=1, grid.ticks.lty = "dotted", grid.ticks.on = "years",
                   labels.col="grey20", cex.axis=0.8, format.labels = "%b\n%Y",
                   ylim = c(min(c.xts), max(c.xts)))
p <- xts::addSeries(c.xts[,2], on=1, lwd=2, col="darkred", lty="dashed")
p <- xts::addLegend("topleft", on = 1,
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
# ylim1 <- c(p$Env$ylim[[2]][1], p$Env$ylim[[2]][2])
# ylim2 <- c(p$Env$ylim[[4]][1], p$Env$ylim[[4]][2])
ylim1 <- p$Env$panels[[1]]$ylim
ylim2 <- p$Env$panels[[2]]$ylim

ylim <- c(ylim1, ylim2)
# get longest drawdown dates for xts object
dt <- table.Drawdowns(R, top = 1) # just want to find the worst drawdown
dt2 <- t(dt[,c("From", "To")])
x <- as.vector(dt2[,NCOL(dt2)])
y <- as.xts(matrix(rep(ylim, length(x)),ncol=length(ylim), byrow=TRUE), 			order.by=as.Date(x))
i=1
p <- xts::addPolygon(y[i:(i+1),1:2], on=-1, col="lightgrey") # top panel
p <- xts::addPolygon(y[i:(i+1),3:4], on=-2, col="lightgrey") # lower panel
p


## Table 2.1
dt2mat <- table.Drawdowns(R, top = 2) # find the worst two drawdowns
dt2mat[,4] <- round(dt2mat[,4],2)
dt2 <- data.frame(dt2mat)[,1:5]
names(dt2) <- c("Begin","Minimum","End","Depth","Months")
dt2 <- dt2[,c(1:3,5,4)]
dt2


# Table 2.2
library(RPESE)
SD12 <- SD.SE(ret.comb, se.method = "IFiid")
SD12 <- printSE(SD12)
SemiSD12 <- SemiSD.SE(ret.comb, se.method = "IFiid")
SemiSD12 <- printSE(SemiSD12)
ES12 <- ES.SE(ret.comb, se.method = "IFiid")
ES12 <- printSE(ES12)
VaR12 <- VaR.SE(ret.comb, se.method = "IFiid")
VaR12 <- printSE(VaR12)
# VaR12[,1] <- -VaR12[,1]
RM <- 100*rbind(SD12,SemiSD12,ES12,VaR12)
colnames(RM)  <- c("Estimate (%)","StdError (%)")
rownames(RM) <- c("GmvLS SD","Market SD",
                  "GmvLS SSD","Market SSD",
                  "GmvLS ES","Market ES",
                  "GmvLS VaR","Market VaR")
RM <- as.data.frame(RM)
RM


## Figure 2.11
library(PCRA)
library(data.table)
stockItems <- c("Date", "TickerLast", "Return", "Ret13WkBill")
returnsAll <- selectCRSPandSPGMI("monthly", stockItems = stockItems,  
                                 factorItems = NULL, outputType = "xts")
riskFree <- returnsAll[ , "Ret13WkBill"]
tsPlotMP(riskFree, yname = "RISK-FREE RATE")


## Figure 2.12
returns <- returnsAll["1995-01-31/2000-12-31"]
x <- sort(apply(returns,2,mean))
x0 <- x[x <= 0.007 & x >= 0.005] # Results in 21 stocks & choose FMC
ret <- returns[, c("FMC", "Ret13WkBill")]
names(ret)[2] <- "Risk-Free"
tsPlotMP(ret, yname = "RETURNS", scaleType = "free")


## Table 2.3
library(RPESE)
SR12 <- SR.SE(ret.comb, se.method = "IFiid")
SR12 <- printSE(SR12)
DSR12 <- DSR.SE(ret.comb, se.method = "IFiid")
DSR12 <- printSE(DSR12)
SoR12 <- SoR.SE(ret.comb, se.method = "IFiid")
SoR12 <- printSE(SoR12)
ESratio12 <- ESratio.SE(ret.comb, se.method = "IFiid")
ESratio12 <- printSE(ESratio12)
Ratios <- rbind(SR12,DSR12,SoR12,ESratio12)
colnames(Ratios)  <- c("Estimate","StdError")
rownames(Ratios) <- c("GmvLS SR","Market SR",
                      "GmvLS DSR","Market DSR",
                      "GmvLS SOR","Market SOR",
                      "GmvLS ESR","Market ESR")
Ratios <- as.data.frame(Ratios)
Ratios


## Figure 2.13
levg <- levgLongShort(wtsGmvLS)
plot.zoo(levg,ylim = c(0.0,1.5),ylab = "Leverage")
abline(h = 1.0,lty = "dotted")


## Figure 2.14 Left-Hand Plot
GmvLS.TO <- 100*turnOver(wtsGmvLS)
plot.zoo(GmvLS.TO,ylim = c(0,60), ylab = "TURNOVER (%)",
         xlab = "", cex.axis = 1.5, cex.lab = 1.5)
abline(h = mean(GmvLS.TO),lty = "dashed")
text(as.Date("2004-01-31"),50,"Mean Turnover = 13.4 (%)", cex = 1.5)


## Figure 2.14 Right-Hand Plot
GmvLS.DIV <- 100*divHHI(wtsGmvLS)
plot.zoo(GmvLS.DIV,ylim = c(0,100),lwd = 1.5, ylab = "DIV(%)",
         xlab = "", cex.axis = 1.5, cex.lab = 1.5)
abline(h = mean(GmvLS.DIV),lty = "dashed")
text(as.Date("2006-01-31"),90,"Mean Diversification = 63.5 (%)",
     cex = 1.5)


## Figure 2.15
muRet = c(.10,.04,.02)
volRet = c(.20,.15,.10)
corrRet = diag(c(1,1,1))
mathEfrontRiskyMuCov(muRet,volRet,corrRet,efront.only = F)
text(0.07, 0.095, "EFFICIENT FRONTIER", cex = 1.2)
arrows(0.07, 0.09, .10, .06, length = 0.1, lwd= 1.0)


## Table 2.4
muRet = c(.10,.04,.02)
volRet = c(.20,.15,.10)
corrRet = diag(c(1,1,1))
efront = mathEfrontRiskyMuCov(muRet,volRet,corrRet, npoints = 5, values = T, display = F)
mu.efront = efront$mu.efront
wtsEfront <- mathWtsEfrontRiskyMuCov(muRet,volRet,corrRet,mu.efront,digits = 3)
wtsEfront


## Figure 2.16
nColor <- 4
barplotWts(as.matrix(wtsEfront), legend.text = T, ylab = "WEIGHTS",
           col = topo.colors(nColor), bar.ylim = c(-1, 2),cex.lab = 1.2,
           cex.axis = 1.3)


## Figure 2.17
returns10 <- returns10Mkt[,-11]
efront <- mathEfrontRisky(returns10, display = T, cexGmv = 1.0,
                          cexText = 0.8)


## Figure 2.18
efront10 <- mathEfrontRisky(returns10,npoints = 5, display = F, values = TRUE)
mu.efront <- efront10$mu.efront
wtsEfront <- mathWtsEfrontRisky(returns10, mu.efront,digits = 3)
barplotWts(as.matrix(wtsEfront), legend.text = T, ylab = "WEIGHTS",
           col = topo.colors(10), bar.ylim = c(-1.5,3.0),cex.lab = 1.2,
           cex.axis = 1.3)


## Figure 2.19
library(PCRA)
library(data.table)
stockItems <- c("Date","TickerLast","CapGroupLast","Return","Ret13WkBill")
dateRange <- c("1997-01-31","2010-12-31")
stocksDat <- selectCRSPandSPGMI("monthly",dateRange = dateRange, stockItems =
                                  stockItems, factorItems = NULL, subsetType = "CapGroupLast",
                                subsetValues = "SmallCap", outputType = "xts")
returns10andRF <- stocksDat[, c(21:30,107)]
names(returns10andRF)[11] <- "Risk-Free"
tsPlotMP(returns10andRF)
rf <- mean(returns10andRF[,11])


## Figure 2.20
returns10 <- returns10andRF[,-11]
mathEfrontCashRisky(returns10, rf = rf)


## Figure 2.21
wtsEfront = mathEfrontCashRisky(returns10, plot.efront = FALSE, values = TRUE)
barplotWts(as.matrix(wtsEfront),legend.text = T,col = topo.colors(2),ylab = "Weights",
           xlab = "VOL", bar.ylim = c(-0.5,1.5))



## Risk Aversion and Risk Tolerance Values in Text Below Equation (2.142)
rf = .005
C = var(returns10)
mu.stocks = apply(returns10, 2, mean)
mue = mu.stocks - rf
a = solve(C, mue)
lambda = sum(a)
lambda      # Risk aversion value
1/lambda    # Risk tolerance value


## Figure 2.22
plot(FRBinterestRates,xaxt="n",xlab="",ylab="InterestRates (%)")
axis(side=1,at=seq(1930,2020,by=10),labels=seq(1930,2020,by=10))
grid()



## Risk-Free Rate and Mean Return of GMV Portfolio in Text Below Figure 2.22
# This code is not provided because it uses CRSP data 
# not contained in the stocksCRSP data set
# library(PCRA)
# rf = round(mean(WeeklyReturns1980[,"TBill90"]),1)
# ret = WeeklyReturns1980[,1:10]
# names(ret)
# muGMV = round(100*52*mathGmv(ret)$mu,1)
# c(rf,muGMV)


## Figure 2.23
# Use returns10 and rf from code chunk for Figure 2.19
rf <- mean(returns10andRF[,11])
returns10 <- returns10andRF[,-11]
mathEfront(returns10, rf = rf, mu.max = .05, sigma.max = .17,npoints = 100)


## Figure 2.24
library(ggplot2)
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
  theme(legend.position=c(0.4, 0.9),
        legend.title = element_text(size = 16),
        legend.text  = element_text(size = 14),
        axis.text    = element_text(size = 12),
        axis.title   = element_text(size = 14))


## Figure 2.25
# Three stocks means, volatilities and covmat
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


## Table 2.5
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


## Figure 2.26
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
library(PCRA)
mathEfrontRiskyMuCov(mu,vol,corrMat,efront.only = T, display = T)
lines(sigmaPA,muPA, type = "l", lwd = 1.5)
points(volB,muB,pch = 16,cex = 1.3)
text(volB, muB,"B",pos = 4, cex = 1.3)
text(0.163,0.06,"ACTIVELY MANAGED",pos = 4, cex = 1.1)
arrows(0.165,0.06,sigmaPA[50],muPA[50],length = .1,lwd = 1.5)


## Figure 2.27
# Copied from Jorion paper, need to get permission


## Figure 2.28
# Made by hand, should create with code

## Figure 2.29 Left-Hand Plot
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


##  Figure 2.29 Right-Hand Plot
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



## Figure 2.30 Left Hand Plot
library(ggplot2)
rm1	   <- 0.18
Beta1  <- c(1.53, 1.36, 1.24, 1.17, 1.06, 0.92, 0.84, 0.76, 0.63, 0.48)
Mu1    <- c(0.26, 0.22, 0.21, 0.21, 0.18, 0.17, 0.16, 0.15, 0.13, 0.12)
Sigma1 <- c(0.49, 0.43, 0.39, 0.37, 0.33, 0.29, 0.27, 0.24, 0.20, 0.17)
SML1   <- rm1 * Beta1

df1 <- data.frame( Beta1, Mu1, Sigma1, SML1)

ggplot(df1) + 
  geom_point(aes(x = Beta1, y = Mu1),  color = "black") + 
  geom_line(aes( x = Beta1, y = SML1), color = "gray20") +
  geom_point(aes(x = 1,     y = rm1),  color = "dodgerblue4", shape = 17, size = 3) +
  labs(x = "Beta", y = "Mean Excess Return", title = "1931 \u2013 1965") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate(geom="text", x=1.19, y=0.15,  label="CAPM Security Market Line", color="gray20") +
  annotate(geom="text", x=.78,  y=0.185, label="Market Portfolio", color="dodgerblue4")


## Figure 2.30 Right Hand Plot
rm2    <- 0.08
Beta2  <- c(1.50, 1.30, 1.17, 1.09, 1.03, 0.95, 0.87, 0.78, 0.67, 0.51)
Mu2    <- c(0.06, 0.08, 0.08, 0.08, 0.08, 0.08, 0.07, 0.07, 0.07, 0.06)
Sigma2 <- c(0.31, 0.26, 0.24, 0.22, 0.21, 0.19, 0.18, 0.16, 0.14, 0.12)
SML2   <- rm2 * Beta2

df2 <- data.frame( Beta2, Mu2, Sigma2, SML2)

ggplot(df2) + 
  geom_point(aes(x = Beta2, y = Mu2),  color = "black")  +
  geom_line(aes( x = Beta2, y = SML2), color = "gray20") +
  geom_point(aes(x = 1,     y = rm2),  color = "dodgerblue4", shape = 17, size = 3) +
  ylim(0, 0.12) +
  labs(x = "Beta", y = "Mean Excess Return", title = "1965 \u2013 1991") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate(geom="text", x=1.12, y=0.055, label="CAPM Security Market Line", color="gray20") +
  annotate(geom="text", x=.80,  y=0.095, label="Market Portfolio", color="dodgerblue4")


## Figure 2.31 Left-Hand Plot
ggplot(df1, aes(x = Beta1, y = Sigma1)) + 
  geom_point() +    
  geom_smooth(method='lm', se = FALSE, linewidth = 0.6, color = "gray20") +
  labs(x = "Beta", y = "Standard Deviation") +
  geom_text(x = 1.05, y = 0.27, label = "sigma %~~% 0.32 ~ beta", parse=TRUE)


## Figure 2.31 Right-Hand Plot
ggplot(df2, aes(x = Beta2, y = Sigma2)) + 
  geom_point() +    
  geom_smooth(method='lm', se = FALSE, linewidth = 0.6, color = "gray20") +
  labs(x = "Beta", y = "Standard Deviation") +
  geom_text(x = 1.05, y = 0.18, label = "sigma %~~% 0.2 ~ beta", parse=TRUE)


## Table 2.6
library(dplyr)
######## CREATE A TABLE FOR THE BOOK ###########
df <- data.frame(matrix(" ", nrow = 6, ncol = 4))
df$X1 <- c("1/31--12/39",  "1/40--12/49", "1/50--12/59", "1/60--12/69", "1/70--12/79","1/80--12/91")
df$X2 <- c(-0.05, 0.03, 0.08, 0.03, 0.01, 0.09)
df$X3 <- c(0.17,  0.10, 0.06, 0.07, 0.10, 0.08)
df$X4 <- c(-0.94,  1.06, 4.25, 1.32, 0.18, 3.90)
#Rename rows and columns and reformat the table
colnames(df) <- c("Period", "$\\mu_{e}$", "$\\sigma_{e}$", "$t( \\mu )$")
df


## Figure 2.32
library(PCRA)
library(xts)
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


## Figure 2.33
library(PortfolioAnalytics)
library(CVXR)
library(data.table)
pspec <- portfolio.spec(assets = names(returns))
pspecFI <- add.constraint(pspec, type = "full_investment")
pspecLO <- add.constraint(portfolio = pspecFI, type = "long_only")
pspecESLO5pct <- add.objective(pspecLO, type = "risk", name = "ES",
                               arguments = list(p = 0.050))

# Increase n.portfolios below for more vertical line accuracy
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


## Figure 2.34
# Use devtools::install_github("kjellpk/optimalRhoPsi") to install optimalRhoPsi
library(optimalRhoPsi)
x <- seq(-4.9, 4.9, by = 0.001)
ccopt <- computeTuningPsi_modOpt(0.95)
plot(x, wgt_modOpt(x, cc = ccopt), type = "l",
     xlab = "x", ylab = "",cex = 1.5,cex.lab = 1.5)


## Figure 2.35
library(PerformanceAnalytics)
library(RobStatTM)
data(edhec)
hfnames <- c("CA","CTA","DIS","EM","EMN","ED","FIA","GM","LSE","MA","RV","SS","FOF")
names(edhec) <- hfnames
retLong <- edhec[,"FIA"]
ret <- retLong['1998-01-31/1999-12-31',]
index(ret) <- as.yearmon(index(ret))
# plot.zoo(ret,type ="b")
mu <- 100*mean(ret)
se.mu <- 100*sd(ret)/sqrt(24)
x <- locScaleM(ret,eff = .95)
muRob <- 100*x$mu
se.muRob <- 100*x$std.mu
plot.zoo(ret,type ="b",xlab = "",ylab = "FIA Returns")
abline(h = muRob/100, col = "blue")
abline(h = mu/100, lty = "dashed", col ="red")
legend(1999.2,-.03,legend = c("Robust Mean","Sample Mean"),lwd = c(1,2),
       lty = c("solid","dashed"),col = c("blue","red"), bty = "n", cex = 1.3)


## Table 2.7
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


## Table 2.8 
# This table does not currently exist, and should add
stdDev <- sd(ret)
madm <- mad(ret)
library(robustbase)
scaleRob <- scaleTau2(ret)
dat <- round(100*rbind(stdDev,madm,scaleRob),2)
scaleEsts <- data.frame(dat)
names(scaleEsts) <- "Estimate (%)"
row.names(scaleEsts) <- c("Sample StdDev","MADM Scale","Robust Scale")
scaleEsts
