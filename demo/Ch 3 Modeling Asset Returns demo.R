
## 1. First, install the devtools package using the drop-down menu:
## RStudio > Tools > Install Packages > devtools

## 2. Next, install the PCRA package FROM THE RStudio CONSOLE using 
## devtools::install_github("robustport/PCRA"), and load it with:
library(PCRA)
# NOTE: This PCRA is the latest version. DO NOT INSTALL PCRA FROM CRAN !

## 3. Install all packages that are arguments of library(pkgName) below with:
## RStudio > Tools > Install Packages > PackageName, and load them with:

## The Ch 2 notes in the lines below indicate that these packages will have
## been installed to run the Ch 2 demo code, and needn't be installed again here

library(data.table) # Ch2    
library(xts) # Ch2 
library(PerformanceAnalytics) # Ch2  
library(PortfolioAnalytics) # Ch2 
library(foreach) # Ch2 
library(CVXR) # Ch2 
library(nortest)
library(nor1mix)
library(QRM)
library(sn)


# NOTE:  When loading packages, you can safely ignore the various comments 
# that appear in the console in red after each package is loaded, including, 
# but not limited to:
# "method overwritten", "using 6 threads", "object is masked", etc. etc.


# You are now ready to run Ch 3 Modeling Asset Returns demo.R


## Table 3.1

colNames <- c("Jan","Feb","Mar","AvgRet","Q1Ret","CumRet")
rowNames <- c("Port1 ","Port2 ","Port3")
p1  <- c(0.1,  0.2,  0.3)
p2  <- c(0.1, -0.2,  0.7)
p3  <- c(0.1,  1.4, -0.9)
ret <- data.frame(rbind(p1, p2, p3))
avgret <- apply(ret, 1, mean)
q1ret  <- apply(ret, 1, PerformanceAnalytics::Return.cumulative, geometric = F)  
cumret <- apply(ret, 1, PerformanceAnalytics::Return.cumulative)
ret    <- 100*cbind(ret, avgret, q1ret, cumret)
names(ret) <- colNames
row.names(ret) <- rowNames
ret


## Table 3.2

colNames <- c("Jan", "Feb", "Mar", "AvgRet", "GeomRet")
rowNames <- c("Port1", "Port2", "Port3")
p1 <- c(0.1,  0.2, 0.3)
p2 <- c(0.1, -0.2, 0.7)
p3 <- c(0.1,  1.4,-0.9)
k  <- 3 # Number of periods
ret <- data.frame(rbind(p1, p2, p3))
avgret <- apply(ret, 1, mean)  
cumret <- apply(ret, 1, PerformanceAnalytics::Return.cumulative)
geomret <- (cumret + 1)^(1/k)-1
ret <- 100*cbind(ret, avgret, round(geomret,4))
names(ret) <- colNames
ret

## Table 3.3

df <- data.frame(matrix(" ", nrow = 4, ncol = 12))

df$X1 <- c("Port1", "Port2", "Port3", "Benchmark")
df$X2 <- c(10, 10,  10, 5)
df$X3 <- c(20,-20, 140, 15)
df$X4 <- c(30, 70, -90, 25)
df$X5 <- c(9.53, 9.53, 9.53, 4.88)
df$X6 <- c(18.23, -22.31, 87.55, 13.98)
df$X7 <- c(26.24, 53.06, -230.26, 22.31)
df$X8 <- c(20, 20, 20, 15)
df$X9 <- c(18.00, 13.43, -44.39, 13.72)
df$X10 <- c(19.72, 14.37, -35.85, 14.71)
df$X11 <- c(0.01, 0.21, 1.33, 0.01)
df$X12 <- c(19.58, 11.25, -35.42, 14.57)
df$X13 <- c(19.50, 9.50, -46.50, 14.50)

#Rename columns
colnames(df) <-c("Portfolio", "r1", "r2", "r3", 
                 "ln(1+r1)", "ln(1+r2)", "ln(1+r3)", 
                 "Mean(r)", "Mean(ln(1+r))", "g", "var(r)", 
                 "Mean(r) - var(r)/2(1 + Mean(r))",
                 "Mean(r) - var(r)/2")

df


## Table 3.4

#  This is taken from a Berkshire Hathaway report.


## Table 3.5

df <- data.frame(matrix(" ", nrow = 2, ncol = 7))
df$X1  <- c("Berkshire Hathaway Inc.", "S&P 500 Index")
df$X2 <- c(21.9,   13.1)
df$X3 <- c(17.4,   11.2)
df$X4 <- c("19.0", 11.8)
df$X5 <- c(0.073, 0.027)
df$X6 <- c(18.9,   11.9)
df$X7 <- c(18.2,   11.7)

#Rename columns
colnames(df) <-c("Portfolio", 
                 "Mean(r)", "Mean(ln(1+r))", "g", "var(r)", 
                 "Mean(r) - var(r)/2(1 + Mean(r))",
                 "Mean(r) - var(r)/2")

df


## Figure 3.1

# Get data.table of returns of 69 midcap stocks in stocksCRSP for 1997 - 2015

stockItems <- c("Date", "TickerLast", "CapGroupLast", "Return", "MktIndexCRSP") 
dateRange  <- c("1997-01-31", "2015-12-31")
returnsAll <- PCRA::selectCRSPandSPGMI("monthly", dateRange = dateRange,  
                                       stockItems = stockItems, factorItems = NULL,
                                       subsetType = "CapGroupLast",
                                       subsetValues = "MidCap",
                                       outputType= "xts")

# Use first 20 stocks and the Market (MktIndexCRSP)
returns20Mkt <- returnsAll[,c(1:20, 68)] # Keep the Market 
names(returns20Mkt)[21] <- "Market"
PCRA::tsPlotMP(returns20Mkt, scaleType = "free", layout = c(2,11), 
               stripText.cex = .4, axis.cex = 0.5, lwd = 0.5)


## Figure 3.2

# Create GmvLS portfolio specs
returns <- returns20Mkt[, -21]
funds <- colnames(returns)
pspec <- PortfolioAnalytics::portfolio.spec(assets = funds)
pspec.fi <- PortfolioAnalytics::add.constraint(pspec, type = "full_investment")
pspec.gmvLS <- PortfolioAnalytics::add.objective(pspec.fi, type = "risk", name = "var")

# Optimize Portfolio at Monthly Rebalancing and 5-Year Training
# It takes 13 seconds on my fairly loaded Dell XPS 17
bt.gmvLS <- PortfolioAnalytics::optimize.portfolio.rebalancing(returns, 
                                                               pspec.gmvLS,
                                                               optimize_method="CVXR",
                                                               rebalance_on="months",
                                                               training_period=60,
                                                               rolling_window=60,
                                                               trace = TRUE)

# Extract time series of portfolio weights
wtsGmvLS <- PortfolioAnalytics::extractWeights(bt.gmvLS)

# Compute rebalancing GmvLO and GmvLS arithmetic returns
GmvLS <- PerformanceAnalytics::Return.rebalancing(returns, wtsGmvLS)

# Combine GmvLS amd Market returns and plot their time series
ret.comb <- na.omit(xts::merge.xts(GmvLS, returns20Mkt[,21], all=F))
names(ret.comb) <- c("GmvLS", "Market")
PCRA::tsPlotMP(ret.comb, scaleType = "free", stripText.cex = .45,
               axis.cex = 0.4, lwd = 0.5)

#### For some reason the above plot becomes a blank plot when running
#### all the code from the beginning through line 203, but if the code
#### is run just through line 174, it the plot is fine.

#### Code lines 195-207 may be over-writing the plot with an empty plot

## Figure 3.3

## Compute cumulative gross portfolio returns
R <- ret.comb
geometric <- TRUE
c.xts <- if ( geometric ) {
  cumprod(1+R)
} else {
  1 + cumsum(R)
}
## Plot cumulative gross returns of Midcap GmvLS and Market portfolios
## Code contributed by Peter Carl

p <- xts::plot.xts(c.xts[,1], col="black", main = "Cumulative Returns",
                   grid.ticks.lwd=1, grid.ticks.lty = "dotted", grid.ticks.on = "years",
                   labels.col="grey20", cex.axis=0.8, format.labels = "%b\n%Y",
                   ylim = c(min(c.xts), max(c.xts)))
p <- xts::addSeries(c.xts[,2], on=1, lwd=2, col="darkblue", lty="dashed")
p <- xts::addLegend("topleft", on = 1,
                    legend.names = names(c.xts),
                    lty = c(1,2,3), lwd = rep(2, NCOL(c.xts)),
                    col = c("black","darkblue", "dareKRed"),
                    bty = "o", box.col = "white",
                    bg=rgb(t(col2rgb("white")), alpha = 200, maxColorValue = 255) 
)
p


## Table 3.6

# Classic GmvLS mean returns4
MeansGmvLS <- round(100*meanReturns4Types(GmvLS), 3)

# Robust GmvLS mean returns4
RobMeansGmvLS <- round(100*meanReturns4Types(GmvLS, robust = TRUE), 3)

# Classic Market mean returns4
Market   <- returns20Mkt[, 21]
MeansMkt <- round(100*meanReturns4Types(Market), 3)

# Robust Market mean returns4
RobMeansMkt <- round(100*meanReturns4Types(Market, robust = TRUE), 3)
dat <- data.frame(rbind(MeansGmvLS, RobMeansGmvLS, 
                        MeansMkt,   RobMeansMkt))
names(dat) <- c("Arithmetic", "Logarithm", "Geometric", "ApproxGeom")
row.names(dat) <- c("Means GmvLS",  "RobMeans GmvLS",
                    "Means Market", "RobMeans Market")
dat

## Table 3.7

GmvLS1YR  <- GmvLS["2015-01-31/2015-12-31"]
GmvLS2YR  <- GmvLS["2014-01-31/2015-12-31"]
GmvLS5YR  <- GmvLS["2011-01-31/2015-12-31"]
GmvLS10YR <- GmvLS["2006-01-31/2015-12-31"]
GmvLS14YR <- GmvLS["2002-01-31/2015-12-31"]
means1YR <- round(100*meanReturns4Types(GmvLS1YR, robust = FALSE)[-4], 3)
means1YRrob <-round(100*meanReturns4Types(GmvLS1YR, robust = TRUE)[-4], 3)
means2YR <- round(100*meanReturns4Types(GmvLS2YR, robust = FALSE)[-4], 3)
means2YRrob <- round(100*meanReturns4Types(GmvLS2YR, robust = TRUE)[-4], 3)
means5YR <- round(100*meanReturns4Types(GmvLS5YR, robust = FALSE)[-4], 3)
means5YRrob <- round(100*meanReturns4Types(GmvLS5YR, robust = TRUE)[-4], 3)
means10YR <- round(100*meanReturns4Types(GmvLS10YR, robust = FALSE)[-4], 3)
means10YRrob <- round(100*meanReturns4Types(GmvLS10YR, robust = TRUE)[-4], 3)
tmp <- round(rbind(means1YR, means1YRrob, means2YR,  means2YRrob,
                   means5YR, means5YRrob, means10YR, means10YRrob), 3)
means3Periods5 <- data.frame(tmp)
names(means3Periods5) <- c("Arithmetic Mean", "Logarithmic Mean", "Geometric Mean")
namesRows <- c("1YR Standard",  "1YR Robust", 
               "2YR Standard",  "2YR Robust",
               "5YR Standard",  "5YR Robust",
               "10YR Standard", "10YR Robust")
row.names(means3Periods5) <- namesRows
means3Periods5


## Table 3.8

df <- data.frame(matrix(" ", nrow = 3, ncol = 13))
df$X1 <- c("Port1", "Port2", "Port3")
df$X2 <- c(5,   5,  5)
df$X3 <- c(5,   -35,  125)
df$X4 <- c(5,   45,  -115)
df$X5 <- c(4.65, 4.65, 4.65)
df$X6 <- c(4.26,   -36.29,  73.57)
df$X7 <- c(3.92,   30.75,  -252.57)
df$X8 <- c(4.76, 4.76, 4.76)
df$X9 <- c(4.35,   -30.43,  108.70)
df$X10 <- c(4.00,   36.00,  -92.00)
df$X11 <- c(5,   5,  5)
df$X12 <- c(4.28,   -0.30,  -58.12)
df$X13 <- c(4.37,   3.44,  7.15)
df$X14 <- c(5.01,  -0.34,  -50.56)
#Rename columns and reformat the table
colnames(df) <-c("Portfolio", 
                 "Arith. Act. Ret 1", "Arith. Act. Ret 2", "Arith. Act. Ret 3", 
                 "Log. Act. Ret 1",   "Log. Act. Ret 2",   "Log. Act. Ret 3", 
                 "Geom. Act. Ret 1",  "Geom. Act. Ret 2",  "Geom. Act. Ret 3", 
                 "Mean(Arith. Act. Ret)", "Mean(Log. Act. Ret)", "Mean(Geom. Act. Ret)", 
                 "g(Port)-g(Bmk)")

df


## Table 3.9

df <- data.frame(matrix(" ", nrow = 3, ncol = 6))
df$X1 <- c("12/31/1979 to 12/31/2019", 
           "12/31/1979 to 12/31/2009", 
           "12/31/2009 to 12/31/2019")
df$X2 <- c(8.79, 11.78, -0.16)
df$X3 <- c(6.26, 8.48,  -0.41)
df$X4 <- c(7.22, 9.85,  -0.46)
df$X5 <- c(22.0, 24.1,  10.9)
df$X6 <- c(17.9, 19.5,  9.7)
#Rename rows and columns and reformat the table

colnames(df) <-c("Period", 
                 "Mean(Arith. Act. Ret)", "Mean(Log. Act. Ret)", 
                 "g(BRK) - g(S&P 500)", 
                 "Std. Dev(Arith. Act. Ret)", "Std. Dev(Log. Act. Ret)")
df


## Figure 3.4

# Get stocksCRSP data for 6 time periods
ret1  <- PCRA::stocksCRSPxts(stocksCRSP,dateRange = c("1993-01-31", "1995-12-31"))
ret2  <- PCRA::stocksCRSPxts(stocksCRSP,dateRange = c("1996-01-31", "1999-12-31"))
ret3  <- PCRA::stocksCRSPxts(stocksCRSP,dateRange = c("2000-01-31", "2003-12-31"))
ret4  <- PCRA::stocksCRSPxts(stocksCRSP,dateRange = c("2004-01-31", "2007-12-31"))
ret5  <- PCRA::stocksCRSPxts(stocksCRSP,dateRange = c("2008-01-31", "2011-12-31"))
ret6  <- PCRA::stocksCRSPxts(stocksCRSP,dateRange = c("2012-01-31", "2015-12-31"))
logret1 <- log(1 + ret1)
logret2 <- log(1 + ret2)
logret3 <- log(1 + ret3)
logret4 <- log(1 + ret4)
logret5 <- log(1 + ret5)
logret6 <- log(1 + ret6)

# Compute skewness for stocksCRSP returns for each of 6 time periods
sk1 <- apply(zoo::coredata(ret1), 2, SKest)
sk2 <- apply(zoo::coredata(ret2), 2, SKest)
sk3 <- apply(zoo::coredata(ret3), 2, SKest)
sk4 <- apply(zoo::coredata(ret4), 2, SKest)
sk5 <- apply(zoo::coredata(ret5), 2, SKest)
sk6 <- apply(zoo::coredata(ret6), 2, SKest)
sk  <- cbind(sk1, sk2, sk3, sk4, sk5, sk6)
times <- c("1993-1995", "1996-1999", "2000-2003",
           "2004-2007", "2008-2011", "2012-2015")

# Compute skewness for stocksCRSP log returns for each of 6 time periods
logsk1 <- apply(zoo::coredata(logret1), 2, SKest)
logsk2 <- apply(zoo::coredata(logret2), 2, SKest)
logsk3 <- apply(zoo::coredata(logret3), 2, SKest)
logsk4 <- apply(zoo::coredata(logret4), 2, SKest)
logsk5 <- apply(zoo::coredata(logret5), 2, SKest)
logsk6 <- apply(zoo::coredata(logret6), 2, SKest)
logsk  <- cbind(logsk1, logsk2, logsk3,
                logsk4, logsk5, logsk6)

# Boxplots of stocksCRSP returns skewness for each of 6 time periods
col <- "cyan"
boxplot(sk, xaxt = "n", main = " Skewness \n Monthly Returns stocksCRSP", 
        cex.main = 1.5, ylim = c(-3.2, 4.5), col = col)
axis(1, at=1:6, labels=times, cex.axis = 1.1)
abline(h=0, lty = "dotted")


## Figure 3.5

# Left-hand plot
# Boxplots of stocksCRSP log returns skewness for 6 time periods
col <- "cyan"
boxplot(logsk, xaxt = "n", 
        main = " Skewness \n Monthly Log Returns stocksCRSP", 
        cex.main = 1.5, ylim = c(-3.2, 4.5), col = col)
axis(1, at=1:6, labels=times, cex.axis = 1.1)
abline(h=0, lty = "dotted")

# Compute stocksCRSP returns excess kurtosis
eKR1 <- apply(zoo::coredata(ret1), 2, KRest)
eKR2 <- apply(zoo::coredata(ret2), 2, KRest)
eKR3 <- apply(zoo::coredata(ret3), 2, KRest)
eKR4 <- apply(zoo::coredata(ret4), 2, KRest)
eKR5 <- apply(zoo::coredata(ret5), 2, KRest)
eKR6 <- apply(zoo::coredata(ret6), 2, KRest)
eKR  <- cbind(eKR1, eKR2, eKR3, eKR4, eKR5, eKR6)

# Compute stocksCRSP log returns excess kurtosis
logeKR1 <- apply(zoo::coredata(logret1), 2, KRest)
logeKR2 <- apply(zoo::coredata(logret2), 2, KRest)
logeKR3 <- apply(zoo::coredata(logret3), 2, KRest)
logeKR4 <- apply(zoo::coredata(logret4), 2, KRest)
logeKR5 <- apply(zoo::coredata(logret5), 2, KRest)
logeKR6 <- apply(zoo::coredata(logret6), 2, KRest)
logeKR  <- cbind(logeKR1, logeKR2, logeKR3, logeKR4, logeKR5, logeKR6)

col <- "cyan"
boxplot(eKR, xaxt = "n",ylim = c(-2, 12), 
        main = "Excess Kutosis \n Monthly Returns stocksCRSP",
        cex.main = 1.5, col = col)
axis(1, at=1:6,labels=times, cex.axis = 1.1)
abline(h=0, lty = "dotted")


# Right-hand plot
col <- "cyan"
boxplot(logeKR, xaxt = "n",ylim = c(-2, 12), 
        main = "Excess Kurtosis \n Monthly Log Returns stocksCRSP",
        cex.main = 1.5,col = col)
axis(1, at=1:6,labels=times, cex.axis = 1.1)
abline(h=0, lty = "dotted")


## Figure 3.6

stocksCRSPweekly <- PCRA::getPCRAData(dataset = "stocksCRSPweekly")
stocksCRSPdaily  <- PCRA::getPCRAData(dataset = "stocksCRSPdaily")
dateRange4 <- c("2003-01-31", "2005-12-31")
dateRange5 <- c("2006-01-31", "2009-12-31")
dateRange6 <- c("2010-01-31", "2012-12-31")
ret4M  <- PCRA::stocksCRSPxts(stocksCRSP, dateRange = dateRange4)
ret5M  <- PCRA::stocksCRSPxts(stocksCRSP, dateRange = dateRange5)
ret6M  <- PCRA::stocksCRSPxts(stocksCRSP, dateRange = dateRange6)
logret4M <- log(1 + ret4M)
logret5M <- log(1 + ret5M)
logret6M <- log(1 + ret6M)

ret4W  <- PCRA::stocksCRSPxts(stocksCRSPweekly, dateRange = dateRange4)
ret5W  <- PCRA::stocksCRSPxts(stocksCRSPweekly, dateRange = dateRange5)
ret6W  <- PCRA::stocksCRSPxts(stocksCRSPweekly, dateRange = dateRange6)
logret4W <- log(1 + ret4W)
logret5W <- log(1 + ret5W)
logret6W <- log(1 + ret6W)

ret4D  <- PCRA::stocksCRSPxts(stocksCRSPdaily, dateRange = dateRange4)
ret5D  <- PCRA::stocksCRSPxts(stocksCRSPdaily, dateRange = dateRange5)
ret6D  <- PCRA::stocksCRSPxts(stocksCRSPdaily, dateRange = dateRange6)
logret4D <- log(1 + ret4D)
logret5D <- log(1 + ret5D)
logret6D <- log(1 + ret6D)

# Compute excess kurtosis for stocksCRSP using log returns
logeKR4M <- apply(zoo::coredata(logret4M), 2, KRest)
logeKR5M <- apply(zoo::coredata(logret5M), 2, KRest)
logeKR6M <- apply(zoo::coredata(logret6M), 2, KRest)
logeKR456M <- cbind(logeKR4M, logeKR5M, logeKR6M)

logeKR4W <- apply(zoo::coredata(logret4W), 2, KRest)
logeKR5W <- apply(zoo::coredata(logret5W), 2, KRest)
logeKR6W <- apply(zoo::coredata(logret6W), 2,KRest)
logeKR456W <- cbind(logeKR4W, logeKR5W, logeKR6W)

logeKR4D <- apply(zoo::coredata(logret4D), 2, KRest)
logeKR5D <- apply(zoo::coredata(logret5D), 2, KRest)
logeKR6D <- apply(zoo::coredata(logret6D), 2, KRest)
logeKR456D <- cbind(logeKR4D, logeKR5D, logeKR6D)

# Set time labels for boxplots
times456 <- c("2003-2005", "2006-2009", "2010-2012")


ylim <- c(-2, 25)
par(mfrow = c(1, 3))
col <- "cyan"
boxplot(logeKR456M, xaxt = "n", ylim = ylim, col = col,
        main = "Excess Kurtosis \n Monthly Log Returns stocksCRSP",
        cex.main = 0.95)
axis(1, at=1:3, labels=times456, cex.axis = 0.9)
abline(h=0, lty = "dotted")

boxplot(logeKR456W, xaxt = "n", ylim = ylim, col = col,
        main = "Excess Kurtosis \n Weekly Log Returns stocksCRSP",
        cex.main = 0.95)
axis(1, at=1:3, labels = times456, cex.axis = 0.9)
abline(h=0, lty = "dotted")

boxplot(logeKR456D, xaxt = "n",ylim = ylim, col = col, 
        main = "Excess Kurtosis \n Daily Log Returns stocksCRSP",
        cex.main = 0.95)
axis(1, at=1:3, labels = times456, cex.axis = 0.9)
abline(h=0, lty = "dotted")
par(mfrow = c(1, 1))


## Figure 3.7

outliers5D   <- logeKR5D[logeKR5D > 25]
tickersOut5D <- names(outliers5D)
PCRA::tsPlotMP(100*logret5D[,tickersOut5D],
               layout = c(2, 6),
               stripText.cex = 0.5, 
               axis.cex = 0.5)


## Figure 3.8

# Functions for asymptotic variance of sample skewness and kurtosis
# T-distribution standardized moment function

smt_t <- function(df, n) {
  if (n >= min(df)) {
    return('No moments')
  } else if (n%%2 == 1) {
    return(0)
  } else {
    moment <- (df^(n/2))/((df/(df-2))^(n/2))
    sumprod <- 1
    for (i in 1:(n/2)) {
      sumprod <- sumprod*(2*i-1)/(df-2*i)
    }
    result <- moment*sumprod
    return(result)
  }
}
# Asymptotic variance of skewness
avar_sk_t <- function(df) {
  if (min(df)<=6) {
    return("Not Applicable")
  } else {
    result<- smt_t(df,6) + 9*(df-2)/df - 6*smt_t(df,4)/sqrt(df/(df-2))
    return(result)
  }
}
# Asymptotic variance of kurtosis
avar_kr_t <- function(df) {
  if (min(df)<=8) {
    return("Not Applicable")
  } else {
    result<- -smt_t(df,4)^2 + 4*smt_t(df,4)^3 - 4*smt_t(df,4)*smt_t(df,6) + smt_t(df,8)
    return(result)
  }
}

par(mfrow = c(1,2))
# Skewness variance plot

nuMin <- 7
plot(seq(nuMin,15,1), avar_sk_t(seq(nuMin,15,1)), 
     type = 'l', col = 'blue', lwd = 1,
     xlab = 'Degrees of Freedom', ylab = 'SK Asymptotic Variance',
     ylim = c(0,110))
abline(h = 6, lty = "dotted")
text(9.5, 10.5, "Normal Dist. Variance = 6", cex = 0.9)
points(seq(nuMin, 20, 1), avar_sk_t(seq(nuMin, 20, 1)), pch = 16)

# Kurtosis variance plot
nuMin <- 9
plot(seq(nuMin,15,1), avar_kr_t(seq(nuMin,15,1)), 
     type = 'l', col = 'blue', lwd = 1,
     xlab = 'Degrees of Freedom', ylab = 'eKR Asymptotic Variance',
     ylim = c(0, 2000))
abline(h = 24, lty = "dotted")
text(11, 120, "Normal Dist. Variance = 24", cex = 0.9) 
points(seq(nuMin, 20, 1), avar_kr_t(seq(nuMin, 20, 1)), pch = 16)
par(mfrow = c(1, 1))



## Figure 3.9

dateRange5 <- c("2006-01-31", "2009-12-31")
ret  <- stocksCRSPxts(stocksCRSPdaily, dateRange = dateRange5)
logret <- log(1 + ret)
logret <- logret[, "CMTL"]
dim(logret)
Mat <- zoo::coredata(logret)
apply(Mat, 2, KRest)
clnMat <- apply(Mat, 2, cleanOutliers, shrink = TRUE, k = 4)
apply(clnMat, 2, KRest)
cln.logret <- xts::xts(clnMat, order.by = index(logret))
logretBoth <- cbind(logret, cln.logret)
colnames(logretBoth) <- c("CMTL", "cleanCMTL")
PCRA::tsPlotMP(100*logretBoth, scaleType = "same")



## Figure 3.10

# Returns for four stocks from 2011 - 2015
tickers4 <- c("DHR", "CSL", "AVP", "AMWD")
dateRange = c("2011-01-31", "2015-12-31")
returns4M <- PCRA::stocksCRSPxts(stocksCRSP, dateRange = dateRange,
                                 tickerSet = tickers4)
returns4W <- PCRA::stocksCRSPxts(stocksCRSPweekly, dateRange = dateRange,
                                 tickerSet = tickers4)
returns4D <- PCRA::stocksCRSPxts(stocksCRSPdaily, dateRange = dateRange,
                                 tickerSet = tickers4)
PCRA::tsPlotMP(returns4M, yname = "MONTHLY RETURNS",
               stripText.cex = 0.5, axis.cex = 0.5)
PCRA::tsPlotMP(returns4W, yname = "WEEKLY RETURNS", 
               stripText.cex = 0.5, axis.cex = 0.5)
PCRA::tsPlotMP(returns4D, yname = "DAILY RETURNS", 
               stripText.cex = 0.5, axis.cex = 0.5)


## Figure 3.11

par(mfrow = c(2,2))
y = "Ordered Returns"
PerformanceAnalytics::chart.QQPlot(returns4M[,"DHR"], ylab = y, pch = 20, 
                                   envelope = 0.95,
                                   main = "DHR (LargeCap)", lwd = 1)
PerformanceAnalytics::chart.QQPlot(returns4M[,"CSL"], ylab = y, pch = 20, 
                                   envelope = 0.95,
                                   main = "CSL (MidCap)", lwd = 1)
PerformanceAnalytics::chart.QQPlot(returns4M[,"AVP"], ylab = y, pch = 20, 
                                   envelope = 0.95,
                                   main = "AVP (SmallCap)", lwd = 1)
PerformanceAnalytics::chart.QQPlot(returns4M[,"AMWD"], ylab = y, pch = 20, 
                                   envelope = 0.95,
                                   main = "AMWD (MicroCap)", lwd = 1)
par(mfrow = c(1, 1))


## Figure 3.12

par(mfrow = c(2,2))
y = "Ordered Returns"
PerformanceAnalytics::chart.QQPlot(returns4W[,"DHR"], ylab = y, pch = 20, 
                                   envelope = 0.95, 
                                   main = "DHR (LargeCap)", lwd = 1)
PerformanceAnalytics::chart.QQPlot(returns4W[,"CSL"], ylab = y, pch = 20, 
                                   envelope = 0.95,
                                   main = "CSL (MidCap)", lwd = 1)
PerformanceAnalytics::chart.QQPlot(returns4W[,"AVP"], ylab = y, pch = 20, 
                                   envelope = 0.95,
                                   main = "AVP (SmallCap)", lwd = 1)
PerformanceAnalytics::chart.QQPlot(returns4W[,"AMWD"], ylab = y, pch = 20, 
                                   envelope = 0.95,
                                   main = "AMWD (MicroCap)", lwd = 1)
par(mfrow = c(1, 1))


## Figure 3.13

par(mfrow = c(2,2))
y = "Ordered Returns"
PerformanceAnalytics::chart.QQPlot(returns4D[,"DHR"], ylab = y, pch = 20, 
                                   envelope = 0.95,
                                   main = "DHR (LargeCap)", lwd = 1)
PerformanceAnalytics::chart.QQPlot(returns4D[,"CSL"], ylab = y, pch = 20, 
                                   envelope = 0.95,
                                   main = "CSL (MidCap)", lwd = 1)
PerformanceAnalytics::chart.QQPlot(returns4D[,"AVP"], ylab = y, pch = 20, 
                                   envelope = 0.95,
                                   main = "AVP (SmallCap)", lwd = 1)
PerformanceAnalytics::chart.QQPlot(returns4D[,"AMWD"], ylab = y, pch = 20, 
                                   envelope = 0.95,
                                   main = "AMWD (MicroCap)", lwd = 1)
par(mfrow = c(1,1))



## Table 3.10

## CRSP Stocks Arithmetic and Log Returns Selection for 2 Time Periods
dateRange1 = c("2006-01-31", "2009-12-31")
dateRange2 = c("2010-01-31", "2013-12-31")
stockItems = c("Date", "Return", "TickerLast", "CapGroupLast")  
ret1 <- PCRA::selectCRSPandSPGMI("monthly", dateRange = dateRange1,  
                                 stockItems = stockItems, factorItems = NULL,
                                 subsetType = "CapGroupLast", subsetValues = "MidCap")

ret2 <- PCRA::selectCRSPandSPGMI("monthly",dateRange = dateRange2,
                                 stockItems = stockItems, factorItems = NULL,
                                 subsetType = "CapGroupLast", subsetValues = "MidCap")
logret1 <- log(1 + ret1)
logret2 <- log(1 + ret2)
n <- length(colnames(ret1))

## Period 1

# SF Test
SFtest <- function(x) SFtest <- sf.test(x)[[2]]

ret <- ret1
sf1 <- apply(zoo::coredata(ret), 2, SFtest)
sf1.001 <- length(sf1[sf1 < 0.001])
sf1.01  <- length(sf1[sf1 >= 0.001 & sf1 < 0.01])
sf1.05  <- length(sf1[sf1 >= 0.01  & sf1 < 0.05])
sf1vec  <- rbind(sf1.001, sf1.01, sf1.05)

ret <- logret1
sf1Log <- apply(zoo::coredata(ret),2,SFtest)
sf1Log.001 <- length(sf1Log[sf1Log < 0.001])
sf1Log.01  <- length(sf1Log[sf1Log >= 0.001 & sf1Log < 0.01])
sf1Log.05  <- length(sf1Log[sf1Log >= 0.01  & sf1Log < 0.05])
sf1Logvec  <- rbind(sf1Log.001, sf1Log.01, sf1Log.05)


# AD Test
ADtest <- function(x) adtest <- ad.test(x)[[2]]

ret <- ret1
ad1 <- apply(zoo::coredata(ret), 2, ADtest)
ad1.001 <- length(ad1[ad1 < 0.001])
ad1.01  <- length(ad1[ad1 >= 0.001 & ad1 < 0.01])
ad1.05  <- length(ad1[ad1 >= 0.01 & ad1 < 0.05])
ad1vec  <- rbind(ad1.001, ad1.01, ad1.05)

ret <- logret1
ad1Log <- apply(zoo::coredata(ret),2,ADtest)
ad1Log.001 <- length(ad1Log[ad1Log < 0.001])
ad1Log.01  <- length(ad1Log[ad1Log >= 0.001 & ad1Log < 0.01])
ad1Log.05  <- length(ad1Log[ad1Log >= 0.01  & ad1Log < 0.05])
ad1Logvec  <- rbind(ad1Log.001, ad1Log.01, ad1Log.05)

## CVM Test
CVMtest <- function(x) adtest <- cvm.test(x)[[2]]

ret <- ret1
cvm1 <- apply(zoo::coredata(ret), 2, CVMtest)
cvm1.001 <- length(cvm1[cvm1 < 0.001])
cvm1.01  <- length(cvm1[cvm1 >= 0.001 & cvm1 < 0.01])
cvm1.05  <- length(cvm1[cvm1 >= 0.01  & cvm1 < 0.05])
cvm1vec  <- rbind(cvm1.001, cvm1.01, cvm1.05)

ret <- logret1
cvm1Log <- apply(zoo::coredata(ret), 2, CVMtest)
cvm1Log.001 <- length(cvm1Log[cvm1Log < 0.001])
cvm1Log.01  <- length(cvm1Log[cvm1Log >= 0.001 & cvm1Log < 0.01])
cvm1Log.05  <- length(cvm1Log[cvm1Log >= 0.01  & cvm1Log < 0.05])
cvm1Logvec  <- rbind(cvm1Log.001, cvm1Log.01, cvm1Log.05)

datP1raw <- data.frame(sf1vec,    sf1Logvec, ad1vec, 
                       ad1Logvec, cvm1vec,   cvm1Logvec)
pctFun <- function(x,n) {round(100*x/n)}
datP1pct <- apply(datP1raw, MARGIN = 2, FUN = pctFun, n = n)
pvals <- c("     p <= .001", ".001 < p <= 0.01", ".01 < p <= 0.05")
datP1 <- data.frame(pvals, datP1pct)
names(datP1) <- c("p-Values", rep(c("Return", "LogRet"), 3))
rownames(datP1) <- NULL

## Period 2 

# SF Test
ret <- ret2
sf2 <- apply(zoo::coredata(ret), 2, SFtest)
sf2.001 <- length(sf2[sf2 < 0.001])
sf2.01  <- length(sf2[sf2 >= 0.001 & sf2 < 0.01])
sf2.05  <- length(sf2[sf2 >= 0.01  & sf2 < 0.05])
sf2vec  <- rbind(sf2.001, sf2.01, sf2.05)

ret <- logret2
sf2Log <- apply(zoo::coredata(ret), 2, SFtest)
sf2Log.001 <- length(sf2Log[sf2Log <= 0.001])
sf2Log.01  <- length(sf2Log[sf2Log >= 0.001 & sf2Log < 0.01])
sf2Log.05  <- length(sf2Log[sf2Log >= 0.01 & sf2Log < 0.05])
sf2Logvec  <- rbind(sf2Log.001, sf2Log.01, sf2Log.05)

# AD Test
ret <- ret2
ad2 <- apply(zoo::coredata(ret), 2, ADtest)
ad2.001 <- length(ad2[ad2 < 0.001])
ad2.01  <- length(ad2[ad2 >= 0.001 & ad2 < 0.01])
ad2.05  <- length(ad2[ad2 >= 0.01  & ad2 < 0.05])
ad2vec  <- rbind(ad2.001, ad2.01, ad2.05)

ret <- logret2
ad2Log <- apply(zoo::coredata(ret), 2, ADtest)
ad2Log.001 <- length(ad2Log[ad2Log < 0.001])
ad2Log.01  <- length(ad2Log[ad2Log >= 0.001 & ad2Log < 0.01])
ad2Log.05  <- length(ad2Log[ad2Log >= 0.01  & ad2Log < 0.05])
ad2Logvec  <- rbind(ad2Log.001, ad2Log.01, ad2Log.05)

## CVM Test
ret <- ret2
cvm2 <- apply(zoo::coredata(ret), 2, CVMtest)
cvm2.001 <- length(cvm2[cvm2 < 0.001])
cvm2.01  <- length(cvm2[cvm2 >= 0.001 & cvm2 < 0.01])
cvm2.05  <- length(cvm2[cvm2 >= 0.01  & cvm2 < 0.05])
cvm2vec  <- rbind(cvm2.001, cvm2.01, cvm2.05)

ret <- logret2
cvm2Log <- apply(zoo::coredata(ret), 2, CVMtest)
cvm2Log.001 <- length(cvm2Log[cvm2Log < 0.001])
cvm2Log.01  <- length(cvm2Log[cvm2Log >= 0.001 & cvm2Log < 0.01])
cvm2Log.05  <- length(cvm1Log[cvm2Log >= 0.01 & cvm2Log < 0.05])
cvm2Logvec  <- rbind(cvm2Log.001, cvm2Log.01, cvm2Log.05)

datP2raw <- data.frame(sf2vec,    sf2Logvec, ad2vec,
                       ad2Logvec, cvm2vec,   cvm2Logvec)
pctFun <- function(x,n) {round(100*x/n)}
datP2pct <- apply(datP2raw, MARGIN = 2, FUN = pctFun, n = n)
pvals <- c("     p <= .001", ".001 < p <= 0.01", ".01 < p <= 0.05")
datP2 <- data.frame(pvals, datP2pct)
names(datP2) <- c("p-Values", rep(c("Return","LogRet"), 3))
rownames(datP2) <- NULL
datP1P2 <- rbind(datP1, datP2)
datP1P2


## Figure 3.14

# x = seq(-1.2, 1.2, .01)
# One could use dnmix = .7*dnorm(x, .1, .15) + .3*dnorm(x, .1, .45) to create the
# above x, but instead we use the following nor1mix package code to do it, and
# the following plot

normix2Pars = nor1mix::norMix(mu = c(.1, .1), sigma = c(.15, .45), w = c(.7, .3))
x = seq(-1.2, 1.2, .01)
dnmix = nor1mix::dnorMix(x, normix2Pars)
dnormref = dnorm(x, .1, .15,)

plot(x, dnormref, type = "l", ylim = c(0, 3), lty = 2, xlab = "Returns", ylab = "")
lines(x, dnmix, lwd = 1.2)
leg.txt =c("NORMAL MIXTURE DENSITY", "STANDARD NORMAL DENSITY")
legend("topleft", legend = leg.txt, lty = 1:2, bty = "n", cex = .7)


## Figure 3.15

n = 104
set.seed(95)
r1 = rnorm(n, 0.1, 0.15)
r2 = rnorm(n, 0.1, 0.45)
u = runif(n)
nmix2ret = ifelse(u<.7, r1, r2)
PerformanceAnalytics::chart.QQPlot(nmix2ret, envelope = 0.95, pch = 20, 
                                   main="",
                                   xlab = "N(0,1) Quantiles", 
                                   ylab = "Ordered Returns")


## Figure 3.16

# Following code uses the nor1mix package function norMixEM EM algorithm for
# the above nmix2ret data set to compute the estimates of the mean, standard
# deviation and probability for each of the two normal distribution components,
# as reported in the text in the second paragraph below equation (45).

out = nor1mix::norMixEM(nmix2ret, 2)
parEst = out[,]
round(parEst, 2)

# Convert variances to standard deviations and fix names
parEst[, 2] = sqrt(parEst[, 2])
dimnames(parEst)[[2]] = c("mu", "sigma", "pi")
round(parEst, 2)

distPar <- nor1mix::norMixEM(nmix2ret, m = 2)
PerformanceAnalytics::chart.QQPlot(nmix2ret, distribution = 'norMix', 
                                   envelope = 0.95, main = "", 
                                   pch = 20,  line = c("quartiles"),
                                   distributionParameter = 'distPar', 
                                   xlab = "Quantiles of Fitted Normal Mixture Distribution",
                                   ylab = "Ordered Returns")


## Figure 3.17

stocksCRSPweekly <- PCRA::getPCRAData("stocksCRSPweekly")
tickers4 <- c("DHR", "CSL", "AVP", "AMWD")
dateRange <- c("2011-01-31", "2015-12-31")
returns4W <- PCRA::stocksCRSPxts(stocksCRSPweekly, 
                                 dateRange = dateRange,
                                 tickerSet = tickers4)
ret <- returns4W[, "DHR"] 
ret <- as.numeric(zoo::coredata(ret))

fit.tdist <- QRM::fit.st(ret)  # Uses package QRM
fitpars <- round(fit.tdist$par.ests[c(2,3,1)], 4)
names(fitpars) <- c("location","scale","dof")

par(mfrow = c(1,2))
PerformanceAnalytics::chart.QQPlot(ret, envelope = .95, 
                                   xlab = "Quantiles of N(0,1) Distribution",
                                   ylab = "DHR Ordered Returns", 
                                   main = "", pch = 20)
PerformanceAnalytics::chart.QQPlot(ret,  envelope = .95,  
                                   xlab = "Quantiles of t(0,1) Distribution", 
                                   ylab = "DHR Ordered Returns", 
                                   main = "",  distribution = 't', 
                                   distributionParameter = fitpars, pch = 20)
par(mfrow = c(1,1))

## Parameter estimates
# fitpars
# fit.tdist
# cov2cor(fit.tdist$asymp.cov)

## Formula for volatility in terms of s and nu
# sigmaTdist <- function(s,nu) s*sqrt(nu/(nu - 2))
# s <- 0.020; nu <- 4.44 # 0.027
# sigmaTdist(s,nu)

## Formula for excess kurtosis (eKR) in terms of s and nu
# First the formula for the 4th central moment m4
# m4Tdist <- function(s,nu) 3*s^4*(nu - 2)/(nu - 4)
# m4Tdist(s,nu)/(sigmaTdist(s,nu))^4 - 2


## Figure 3.18

ret <- returns4W[, "AMWD"] 
ret <- as.numeric(zoo::coredata(ret))
## PerformanceAnalytics::chart.QQPlot(ret, envelope = .95, pch = 20) # Reference normal QQPlot

fit.tdist <- QRM::fit.st(ret)  # Uses package QRM
fitpars <- round(fit.tdist$par.ests[c(2,3,1)], 4)
names(fitpars) <- c("location","scale","dof")

n = length(ret)
fit.st = sn::st.mple(as.matrix(rep(1, n)), ret)
names(fit.st$dp) = c("location", "scale", "alpha", "dof")
fit.st$dp <- round(fit.st$dp, 4)

par(mfrow = c(1, 2), pty = "s")
PerformanceAnalytics::chart.QQPlot(ret,  envelope = .95,  
                                   xlab = "Quantiles of T(0,1)", 
                                   ylab = "Ordered Returns", 
                                   main = "",  distribution = 't', 
                                   distributionParameter = fitpars, pch = 20)
PerformanceAnalytics::chart.QQPlot(ret, envelope = .95, 
                                   xlab = "Quantiles of Fitted SlewT(loc,scale,skew,df)", 
                                   ylab = "Ordered Returns", distribution = 'st', 
                                   main = "", 
                                   distributionParameter = 'xi = fit.st$dp[1],
				                           omega = fit.st$dp[2],alpha = fit.st$dp[3],
			    	                       nu = fit.st$dp[4]', pch = 20)
par(mfrow = c(1,1), pty = "m")