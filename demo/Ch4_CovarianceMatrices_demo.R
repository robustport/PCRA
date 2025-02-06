
## 1. First, install the devtools package using the drop-down menu:
## RStudio > Tools > Install Packages > devtools

## 2. Next, install the PCRA package FROM THE RStudio CONSOLE using 
## devtools::install_github("robustport/PCRA"), and load it with:
library(PCRA)
# NOTE: This PCRA is the latest version. DO NOT INSTALL PCRA FROM CRAN !

## 3. Install all packages that are arguments of library(pkgName) below with:
## RStudio > Tools > Install Packages > PackageName, and load them with:

## The Ch 2 comments in the lines below indicate that these packages will have
## been installed to run the Ch 2 demo code, and needn't be installed again here

library(data.table) # Ch2    
library(xts) # Ch2
library(PortfolioAnalytics) # Ch2
library(CVXR) # Ch2
library(RobStatTM) # Ch2
library(foreach)
library(ellipse)
library(Matrix)
library(fit.models)
library(MASS)

## 4. Finally, instal the FactorAnalytics and covmat packages
## from the RStudio Console using
## devtools::install_github("braverock/FactorAnalytics") # Needed only for now
## devtools::install_github("robustport/covmat")

# NOTE:  When loading packages, you can safely ignore the various comments 
# that appear in the console in red after each package is loaded, including, 
# but not limited to:
# "method overwritten", "using 6 threads", "object is masked", etc. etc.

# You are now ready to run Ch 4 Covariance Matrices demo.R


##  Figure 4.1

rho <- 0.8
col1 <- c(1.0, 0.8)
col2 <- c(0.8, 1.0)
cormat <- cbind(col1,col2)
vol1 <- 0.8
vol2 <- 1.0
dsig <- diag(c(vol1,vol2))
covmat <- dsig%*%cormat%*%dsig
mu <- c(0,0)
evalvec <- eigen(covmat, symmetric = T)
evals <- evalvec$values
sqrt(evals)
p1 <- evalvec$vectors[,1]
p2 <- evalvec$vectors[,2]

par(pty = "s")
xlim <- c(-2.5, 2.5)
ylim <- c(-2.5, 2.5)
fig <- ellipse::ellipse(covmat, centre = mu, level = 0.95)
plot(fig, type = "l", xlim = xlim, ylim = ylim, cex = 2.0,
     xlab = "R1", ylab = "R2", cex.axis = 1.5, cex.lab = 1.4,
     lwd = 1.3, axes = F)
axis(side = 2)
axis(side = 1)
arrows(0, 0, p1[1], p1[2], length = 0.15, angle = 20, lwd = 2)
arrows(0, 0, p2[1], p2[2], length = 0.15, angle = 20, lwd = 2)
abline(0,p1[2]/p1[1], lty = "dotted")
abline(0,p2[2]/p2[1], lty = "dotted")
text(p1[1], p1[2], labels = "p1", pos = 4, cex = 1.4)
text(p2[1], p2[2], labels = "p2", pos = 3, cex = 1.4)
par(pty = "m")


##  Table 4.1

stockItems <- c("Date","TickerLast","CapGroupLast","Return")
dateRange <- c("2003-01-31","2003-12-31")
stocksDat <- PCRA::selectCRSPandSPGMI("monthly",dateRange = dateRange, 
                                      stockItems = stockItems, 
                                      factorItems = NULL, 
                                      subsetType = "CapGroupLast",
                                      subsetValues = "SmallCap", 
                                      outputType = "xts")

returns10 <- stocksDat[, c(21:30)]
covmat8 <- cov(returns10[1:8,])
covmat9 <- cov(returns10[1:9,])
covmat10 <- cov(returns10[1:10,])
covmat11 <- cov(returns10[1:11,])
covmat12 <- cov(returns10[1:12,])

rk8 <- Matrix::rankMatrix(covmat8)[1]
rk9 <- Matrix::rankMatrix(covmat9)[1]
rk10 <- Matrix::rankMatrix(covmat10)[1]
rk11 <- Matrix::rankMatrix(covmat11)[1]
rk12 <- Matrix::rankMatrix(covmat12)[1]
# ranks <- data.frame(rbind(c(8,9,10,11,12),c(rk8,rk9,rk10,rk11,rk12)))
ranks <- data.frame(t(c(rk8,rk9,rk10,rk11,rk12)))
row.names(ranks) <- "Sample CovMat Rank"
names(ranks) <- c("8","9","10","11","12")
ranks


##  Table 4.2

stockItems <- c("Date","TickerLast","CapGroupLast","Return")
dateRange <- c("2003-01-31","2003-06-30")
stocksDat <- PCRA::selectCRSPandSPGMI("monthly",dateRange = dateRange, 
                                      stockItems =
                                        stockItems, factorItems = NULL, 
                                      subsetType = "CapGroupLast",
                                      subsetValues = "SmallCap", 
                                      outputType = "xts")
digits <- 5
ret <- stocksDat[, 21:24]
cov3 <- cov(ret[1:3, ])
evs3 <- round(eigen(cov3,symmetric=T,only.values=T)$values,digits)
cov4 <- cov(ret[1:4, ])
evs4 <- round(eigen(cov4,symmetric=T,only.values=T)$values,digits)
cov5 <- cov(ret[1:5, ])
evs5 <- round(eigen(cov5,symmetric=T,only.values=T)$values,digits)
cov6 <- cov(ret[1:6, ])
evs6 <- round(eigen(cov6,symmetric=T,only.values=T)$values,digits)
evsMat <- rbind(evs3,evs4,evs5,evs6)
x <- 3:6
evs <- data.frame(cbind(x,evsMat))
names(evs) <- c("T","EV1","EV2","EV3","EV4")
row.names(evs) <- NULL
evs


##  Figure 4.2

stocksCRSPweekly <- PCRA::getPCRAData("stocksCRSPweekly")
stockItems <- c("Date","TickerLast","CapGroupLast","Return")
dateRange <- c("2007-01-01", "2010-12-31")

largeCapAll <- PCRA::selectCRSPandSPGMI("weekly",dateRange = dateRange, 
                                        stockItems = stockItems, 
                                        factorItems = NULL, 
                                        subsetType = "CapGroupLast",
                                        subsetValues = "LargeCap", 
                                        outputType = "xts")

midCapAll <- PCRA::selectCRSPandSPGMI("weekly",dateRange = dateRange, 
                                      stockItems = stockItems, 
                                      factorItems = NULL, 
                                      subsetType = "CapGroupLast",
                                      subsetValues = "MidCap", 
                                      outputType = "xts")

smallCapAll <- PCRA::selectCRSPandSPGMI("weekly",dateRange = dateRange, 
                                        stockItems = stockItems, 
                                        factorItems = NULL, 
                                        subsetType = "CapGroupLast",
                                        subsetValues = "SmallCap", 
                                        outputType = "xts")

microCapAll <- PCRA::selectCRSPandSPGMI("weekly",dateRange = dateRange, 
                                        stockItems = stockItems, 
                                        factorItems = NULL, 
                                        subsetType = "CapGroupLast",
                                        subsetValues = "MicroCap", 
                                        outputType = "xts")

largeCov <- cov(zoo::coredata(largeCapAll[ , 1:20]))
midCov <- cov(zoo::coredata(midCapAll[ , 1:20]))
smallCov <- cov(zoo::coredata(smallCapAll[ , 1:20]))
microCov <- cov(zoo::coredata(microCapAll[ , 1:20]))

evsLarge <- eigen(largeCov,symmetric=T,only.values=T)$values
evsMid <- eigen(midCov,symmetric=T,only.values=T)$values
evsSmall <- eigen(smallCov,symmetric=T,only.values=T)$values
evsMicro <- eigen(microCov,symmetric=T,only.values=T)$values

par(mfrow = c(2,2))
barplot(sqrt(evsLarge), ylim = c(0,0.30), col = "cyan",
        main = "Square Root Eigenvalues Largecap Covmat")
barplot(sqrt(evsMid), ylim = c(0,0.30), col = "cyan",
        main = "Square Root Eigenvalues Midcap Covmat")
barplot(sqrt(evsSmall), ylim = c(0,0.30), col = "cyan",
        main = "Square Root Eigenvalues Smallcap Covmat")
barplot(sqrt(evsMicro), ylim = c(0,0.30), col = "cyan",
        main = "Square Root Eigenvalues Microcap Covmat")
par(mfrow = c(1,1))


##  Figure 4.3

largeCor <- cor(zoo::coredata(largeCapAll[ , 1:20]))
midCor <- cor(zoo::coredata(midCapAll[ , 1:20]))
smallCor <- cor(zoo::coredata(smallCapAll[ , 1:20]))
microCor <- cor(zoo::coredata(microCapAll[ , 1:20]))

evsLarge <- eigen(largeCor,symmetric=T,only.values=T)$values
evsMid <- eigen(midCor,symmetric=T,only.values=T)$values
evsSmall <- eigen(smallCor,symmetric=T,only.values=T)$values
evsMicro <- eigen(microCor,symmetric=T,only.values=T)$values

par(mfrow = c(2,2))
ymax <- 10.0
barplot(evsLarge, ylim = c(0,ymax), col = "cyan",
        main = "Eigenvalues Largecap CorMat")
barplot(evsMid, ylim = c(0,ymax), col = "cyan",
        main = "Eigenvalues Midcap CorMat")
barplot(evsSmall, ylim = c(0,ymax), col = "cyan",
        main = "Eigenvalues Smallcap CorMat")
barplot(evsMicro, ylim = c(0,ymax), col = "cyan",
        main = "Eigenvalues Microcap CorMat")
par(mfrow = c(1,1))


##  Figure 4.4

data(gfunds5, package = "PCRA")
gfunds5 <- gfunds5[-(1:81),]
gfunds4 <- gfunds5[,-1]
gfunds124 <- gfunds4[,c(1,2,4)]

winsorize2D = function(x,frac = 0.10)
{
  # x is a matrix with two columns
  mat = x
  mat1 = mat[order(mat[,1]),]
  mat1[,1] = winsorize(mat1[,1], fraction = frac)
  mat2 = mat1[order(mat1[,2]),]
  mat2[,2] = winsorize(mat2[,2], fraction = frac)
  matW = mat2[order(row.names(mat2)),]
  matW
}

gfundsBndHY <- gfunds124[,c(2,3)]
ret = zoo::coredata(gfundsBndHY)
ret.df = data.frame(ret)
retW = winsorize2D(ret.df, frac = .05)

# NOTE: 3 points Winsorized, so 4 equal maximum and minimum values 
# in each dimension for a total of 16, but only 15 are unique because
# 1 data point is Winsorized with respect to HY and BND variables

retWxmin = retW[retW[,1] == min(retW[,1]),]
retWxmax = retW[retW[,1] == max(retW[,1]),]
retWymin = retW[retW[,2] == min(retW[,2]),]
retWymax = retW[retW[,2] == max(retW[,2]),]
retWset = rbind(retWxmin,retWxmax,retWymin,retWymax)
retOutRows = as.numeric(row.names(retWset))
retWinside = ret.df[-retOutRows,]

par(mfrow = c(1,2), pty = "s")
xlim = c(-.06,.06)
ylim = c(-.022,.016)
plot(ret.df,xlim = xlim, ylim = ylim, pch = 16, cex = 0.8,
     sub = "(a)  Original Returns")
plot(retWinside, xlim = xlim, ylim = ylim, pch = 16, cex = 0.8,
     sub = "(b)  5% Winsorized Returns")
points(retWset, xlim = xlim, ylim = ylim, pch = 21, cex = 1.1)
par(mfrow = c(1,1))


##  Table 4.3

gfundsBndHY <- gfunds124[,c(2,3)]
ret = zoo::coredata(gfundsBndHY)
ret.df = data.frame(ret)
retWin0 = ret.df
retWin1 = winsorize2D(ret.df,frac = .01)  # Winsorize 1% each end
retWin3 = winsorize2D(ret.df,frac = .03)  # Winsorize 2% each end
retWin5 = winsorize2D(ret.df,frac = .05)  # Winsorize 3% each end

winPct = c("0% ","1% ","3% ","5% ")
cor0 = cor(ret.df)[1,2]
cor1 = cor(retWin1)[1,2]
cor3 = cor(retWin3)[1,2]
cor5 = cor(retWin5)[1,2]
corrSetOut = round(cbind(cor0,cor1,cor3,cor5),3)
outWin = data.frame(corrSetOut)
names(outWin) = winPct
row.names(outWin) = "Correlation"
outWin


##  Table 4.4

# Get trimmed data correlations

gfundsBndHY <- gfunds124[,c(2,3)]
ret = zoo::coredata(gfundsBndHY)
ret.df = data.frame(ret)
trimFrac = c(0.01, 0.03, 0.05)
corrSet = rep(0,3)

## The following method of using winsorization2D to get trimmed returns
## correlations is a kludge, and it trims one too many returns values.  This
## needs to be ## replaced with proper trimming, likely by writing a simple
## function trimData, or else using the function Trim in the DescTools package.

for(i in 1:3) {
  retW = winsorize2D(ret.df, frac = trimFrac[i])
  retWxmin = retW[retW[,1] == min(retW[,1]),]
  retWxmax = retW[retW[,1] == max(retW[,1]),]
  retWymin = retW[retW[,2] == min(retW[,2]),]
  retWymax = retW[retW[,2] == max(retW[,2]),]
  retWset = rbind(retWxmin,retWxmax,retWymin,retWymax)
  retWset = unique(retWset)
  retOutRows = as.numeric(row.names(retWset))
  retWinside = ret.df[-retOutRows,]
  corrSet[i] = cor(retWinside)[1,2]
}
corrSetOut = round(c(cor(ret.df)[1,2],corrSet),3)
outTrim = data.frame(t(corrSetOut))
names(outTrim) = c("0% ","1% ","3% ","5% ")
row.names(outTrim) = "Correlation"
outTrim


##  Figure 4.5

# The following function should be added to PCRA

wgtSHR <- function(x)
{
  q <- -1.944 + 1.728*x - 0.312*x^2 + 0.016*x^3
  if(x <= 4) {out <- 1}
  else
  {out <- ifelse(x <= 9, q, 0)}
  out
}

x <- seq(0, 11, 0.1)
nx <- length(x)
y <- rep(0,nx)
for(i in 1:nx) 
{y[i] <- wgtSHR(x[i])}

plot(x,y, type = "l", ylab = "W-SHR(x)", lwd = 1.3)
abline(h = 0, lty = "dotted")


##  Figure 4.6

data(gfunds5, package = "PCRA")
gfunds5 <- gfunds5[-(1:81),]
gfunds4 <- gfunds5[,-1]
gfunds124 <- gfunds4[,c(1,2,4)]

gfunds124.df <- data.frame(zoo::coredata(gfunds124))
set.seed(1024)
robFit <- RobStatTM::covRob(gfunds124.df)
wtsVec <- robFit$wts
# Scale wtsVec to have max value of 1
wtsVec <- wtsVec/max(wtsVec)
index124 <- index(gfunds124)
wtsCovRob <- xts(wtsVec, order.by = index124)
gfunds124andWtsTseries <- cbind(gfunds124, wtsCovRob)

PCRA::tsPlotMP(gfunds124andWtsTseries, scaleType = "free", type ="l", yname = NULL, 
               stripText.cex = .5, axis.cex = .5, color = "black")


## Figure 4.7 Left

gfunds124.df <- data.frame(zoo::coredata(gfunds124))
pairs(100*gfunds124.df, pch = 20, cex = 2, cex.axis = 2, cex.labels = 3)


## Figure 4.7 Right

classicFit <- RobStatTM::covClassic(gfunds124.df)
set.seed(1024)
robFit <- RobStatTM::covRob(gfunds124.df)
covClassicRob <- fit.models::fit.models(classicFit, robFit)

PCRA::ellipsesPlotPCRA.covfm(covClassicRob, which = 2, cex.axis = 1.5, cex.lab = 1.5)


##  Figure 4.8

set.seed(1024)
robFit <- RobStatTM::covRob(gfunds124.df)
wtsVec <- robFit$wts
gfundsAndWtsVec <- cbind(gfunds124.df, wtsVec)
datWtsZero <- subset(gfundsAndWtsVec, wtsVec <= 0.1, select = c("HY", "BND"))
datWtsNonZero <- subset(gfundsAndWtsVec, wtsVec > 0.1 , select = c("HY", "BND"))

# Make ellipses plots for BND versus HY with data display
gfunds <- gfunds124.df[,c(2,3)]
set.seed(1024)

# Need to check why the above set.seed is needed

classicFit <- RobStatTM::covClassic(gfunds)
RobFit <- RobStatTM::covRob(gfunds)
# cov2Cor(RobFit$V)
classicAndRob <- fit.models::fit.models(classicFit,RobFit)

par(mfrow = c(1,2))
par(pty = "s")
{plot(datWtsZero, ylim = c(-0.05, 0.05), cex = 1.2, 
      main = "Rejected Outliers")
  points(datWtsNonZero, pch = 20, cex = 1.5)
  legend("topleft", legend = c("Rejected Outliers", "Original Returns"),
         pch = c(1, 16), bty = "n")}

PCRA::ellipsesPlotPCRA.covfm(classicAndRob)
par(pty = "m")
par(mfrow = c(1,1))


##  Figure 4.9

p1 <- 10
p2 <- 50
n1 <- 100
n2 <- 500

# NOTE: consRocke.R, WRoTru.R are non-exported RobStatTM functions
# So use ":::" instead of "::"

gamma1 <- RobStatTM:::consRocke(p1, n1, initial = "Rocke")$gamma
gamma2 <- RobStatTM:::consRocke(p2, n2, initial = "Rocke")$gamma

u <- seq(0,2,by = 0.05)
y1 <- RobStatTM:::WRoTru(u, gamma1, q = 2)
y2 <- RobStatTM:::WRoTru(u,gamma2, q = 2)

plot(u, y1, type = "l", xlab = "dsq/N", ylab = "W(dsq/N)", 
     cex.lab = 2.0, cex.axis = 2.0)
lines(u, y2, lty = "dashed")
legend(0.5, 0.25, legend = c("K = 10, T = 50", "K = 50, T = 500"),
       lty = c("solid", "dashed"), bty = "n", cex = 2.5)


##  Figure 4.10

stocksCRSPweekly <- PCRA::getPCRAData("stocksCRSPweekly")
dateRange    <- c("2008-01-01", "2009-12-31")
stockItems <- c("Date", "TickerLast", "CapGroupLast", "Return", "MktIndexCRSP")
returnsAll <- PCRA::selectCRSPandSPGMI("weekly",
                                       dateRange = dateRange,
                                       stockItems = stockItems, 
                                       factorItems = NULL, 
                                       subsetType = "CapGroupLast",
                                       subsetValues = "SmallCap", 
                                       outputType = "xts")

returns <- returnsAll[ , 1:20]
# PCRA::tsPlotMP(returns, layout = c(2,10)) # Optional for user

range(index(returns))
ret.df <- data.frame(zoo::coredata(returns))
covClassic <- RobStatTM::covClassic(ret.df)
set.seed(1024) # This is to avoid a bug in covRobRocke.R
covRobust <- RobStatTM::covRobRocke(ret.df)
covClassicRob <- fit.models::fit.models(covClassic, covRobust)
plot(covClassicRob, which.plots = 3, npcs = 5, cex = 1.0, 
     cex.axis = 0.7, col = c("darkred", "black"),
     lty = c("dashed","solid"), lwd = c(1.0, 1.5))

# Legend needs fixing in the fit.models function screePlot.covfm
# See ls("packages:fit.models")


#  Figure 4.11

muT <- c(1,1)
sdT <- c(1.5,3.0)
datCorT <- c(1,0.9,0.9,1)
rhoCorT <- matrix(datCorT,nrow = 2)
sigMatT <- diag(sdT)%*%rhoCorT%*%diag(sdT)
set.seed(60)
set.seed(570000) 
retN <- MASS::mvrnorm(100, muT, sigMatT)

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
plot(retN, xlab = "Return 1", ylab = "Return 2", xlim = c(-3.0,5.0), ylim = c(-7.0,8.0),
     cex.lab = 1.3, pch = 16)
abline(h = 0, v = 0, lty="dotted")
plot(z, xlab ="Z1", ylab="Z2", xlim=c(-3,3), ylim=c(-3,3), cex.lab = 1.3, pch = 16)
abline(h=0, v=0, lty="dotted")
par(mfrow = c(1,1))


##  Figure 4.12

# Create bivariate normal data plus outliers

muT <- c(0,0)
sdT <- c(1,1)
datCorT <- c(1,0.9,0.9,1)
rhoCorT <- matrix(datCorT, nrow = 2)
covMatT <- diag(sdT)%*%rhoCorT%*%diag(sdT)
set.seed(8182)
retN <- MASS::mvrnorm(50,muT,covMatT)
retOut1 <- MASS::mvrnorm(5, c(1.5,-1.5), diag(c(.2^2,.2^2)))
retOut2 <- MASS::mvrnorm(5, c(-1.5,1.5), diag(c(.2^2,.2^2)))
ret <- rbind(retN, retOut1, retOut2)

# Sample mean and covariance
mu <- apply(ret,2,mean)
covMat <- cov(ret)
cov2cor(covMat)

# Compute inverse square-root of sample sigMat
covMatInv <- solve(covMat)
covMatInvSqrt <- tensr::mhalf(covMatInv)

# Transform returns 
z <- (ret-mu)%*%covMatInvSqrt
cor(z)

## Make plots
par(mfrow = c(1,2))
par(pty = "s")
plot(ret, xlab = "Return 1", ylab = "Return 2", xlim=c(-3,3), ylim=c(-3,3), cex.lab = 1.3, 
     cex = 1.1, cex.axis = 1.2, pch = 16)
abline(h=0, v=0, lty="dotted")
plot(z, xlab="Z1", ylab="Z2", xlim = c(-3,3), ylim = c(-3,3), cex.lab = 1.3, 
     cex = 1.1, cex.axis = 1.2, pch = 16)
abline(h=0,v=0,lty="dotted")
par(mfrow = c(1,1))


##  Figure 4.13

# Robust mean and covariance
muCovRob <- RobStatTM::covRob(ret) 
muRob <- muCovRob$center
covMatRob <- muCovRob$cov
cov2cor(covMatRob)

# Compute inverse square-root of robust sigMat
covMatInvRob <- solve(covMatRob)
covMatInvSqrtRob <- tensr::mhalf(covMatInvRob)

# Transform returns 
zRob <- (ret-muRob)%*%covMatInvSqrtRob
cor(zRob)
RobStatTM::covRobRocke(zRob)$cov 

## Make plots
plot(zRob, xlab= "Zrob 1", ylab="Zrob 2", xlim = c(-6,6), ylim = c(-6,6), cex = 1.1, 
     cex.lab = 1.3, pch = 16)
abline(h=0,v=0,lty="dotted")


##  Figure 4.14

MD <- RobStatTM::covClassic(ret)
set.seed(1024)
RD <- RobStatTM::covRobRocke(ret)
covClassicRobNorMix <- fit.models::fit.models(MD, RD)
plot(covClassicRobNorMix, which = 1)


##  Figure 4.15

# covClassicRob argument below is from the Figure 4.10 code

plot(covClassicRob, which.plots = 1, level = 0.99, id.n = 7, cex = 0.5)


####  Figure 4.16. 

####  NOTE: The code below takes about 3.5 minutes

stocksCRSPweekly <- PCRA::getPCRAData("stocksCRSPweekly")
dateRange    <- c("2006-01-01", "2012-12-31")
stockItems <- c("Date", "TickerLast", "CapGroupLast", "Return", "MktIndexCRSP")
returnsAll <- PCRA::selectCRSPandSPGMI("weekly",
                                       dateRange = dateRange,
                                       stockItems = stockItems, 
                                       factorItems = NULL, 
                                       subsetType = "CapGroupLast",
                                       subsetValues = "SmallCap", 
                                       outputType = "xts")
returns <- returnsAll[,1:30]
# PCRA::tsPlotMP(returns, layout = c(2,15))  # User optional plot
Market <- returnsAll[, 107]

# Except for Return.rebalancing and base R functions, the functions
# below up to an including line 629 are in the PortfolioAnalytics package.
# Return.rebalancing is in the PerformnaceAnalytics package.

funds <- colnames(returns)
pspec <- PortfolioAnalytics::portfolio.spec(funds)
pspec <- add.constraint(pspec, type="full_investment")
pspec <- add.constraint(pspec, type="long_only")
pspec <- add.objective(pspec, type="risk", name="var")

bt.gmv <- optimize.portfolio.rebalancing(returns, pspec,
                                         optimize_method = "CVXR",
                                         rebalance_on="weeks",
                                         training_period = 100)
wts.gmv <- extractWeights(bt.gmv)
GMV <- Return.rebalancing(returns, wts.gmv)

momentEstFun <- 'custom.covRob.Rocke'
name <- "GmvLOcovRobRocke"
bt.gmv.rob <- optimize.portfolio.rebalancing(returns, pspec,
                                             optimize_method = "CVXR",
                                             rebalance_on = "weeks",
                                             training_period = 100, 
                                             momentFUN = momentEstFun)
wts.gmv.rob <- extractWeights(bt.gmv.rob)
GMV.rob <- Return.rebalancing(returns, wts.gmv.rob)
ret.comb <- na.omit(merge(GMV.rob, GMV, Market, all=F))
names(ret.comb) <- c(name, "GmvLO", "Market")

backtest.plot(ret.comb, colorSet = c("darkgreen", "black", "red"), 
              plotType = "cumRet", drawdown_on = NULL, ltySet = c(1,2,3))



####  Figure 4.17

####  NOTE: The code below takes about 3.5 minutes

stocksCRSPweekly <- getPCRAData("stocksCRSPweekly")
dateRange    <- c("2006-01-01", "2012-12-31")
stockItems <- c("Date", "TickerLast", "CapGroupLast", "Return", "MktIndexCRSP")
returnsAll <- selectCRSPandSPGMI("weekly",
                                 dateRange = dateRange,
                                 stockItems = stockItems, 
                                 factorItems = NULL, 
                                 subsetType = "CapGroupLast",
                                 subsetValues = "SmallCap", 
                                 outputType = "xts")
returns <- returnsAll[,31:60]
# PCRA::tsPlotMP(returns, layout = c(2,15))  # User optional plot
MARKET <- returnsAll[, 107]

# Except for Return.rebalancing and base R functions, the functions
# below up to an including line 681 are in the PortfolioAnalytics package.
# Return.rebalancing is in the PerformnaceAnalytics package.

funds <- colnames(returns)
pspec <- portfolio.spec(funds)
pspec <- add.constraint(pspec, type="full_investment")
pspec <- add.constraint(pspec, type="long_only")
pspec <- add.objective(pspec, type="risk", name="var")

bt.gmv <- optimize.portfolio.rebalancing(returns, pspec,
                                         optimize_method = "CVXR",
                                         rebalance_on="weeks",
                                         training_period = 100)
wts.gmv <- extractWeights(bt.gmv)
GMV <- Return.rebalancing(returns, wts.gmv)

momentEstFun <- 'custom.covRob.Rocke'
name <- "GmvLOcovRobRocke"
bt.gmv.rob <- optimize.portfolio.rebalancing(returns, pspec,
                                             optimize_method = "CVXR",
                                             rebalance_on = "weeks",
                                             training_period = 100, 
                                             momentFUN = momentEstFun)
wts.gmv.rob <- extractWeights(bt.gmv.rob)
GMV.rob <- Return.rebalancing(returns, wts.gmv.rob)
ret.comb <- na.omit(merge(GMV.rob, GMV, MARKET, all=F))
names(ret.comb) <- c(name, "GmvLO", "MARKET")

backtest.plot(ret.comb, colorSet = c("darkgreen", "black", "red"), 
              plotType = "cumRet", drawdown_on = NULL, ltySet = c(1,2,3))


# Figure  4.18

covClassic <- c(0.97, 0.79, 0.59, 0.37, 0.20)
covShrinkCC <- c(1.24, 1.14, 0.91, 0.54, 0.30)
covShrinkSFM <- c(1.18, 1.08, 0.89, 0.57, 0.33)
df <- data.frame(x = c("25", "50", "100", "225", "500"),
                 y1 = covShrinkCC,
                 y2 = covShrinkSFM,
                 y3 = covClassic)

# require(zoo)
plot(df$x, df$y1, type = "b", col = "black", ylim = c(0, 1.5), lwd = 1.3,
     lty = "solid", ylab = "Information Ratio (IR)", cex.axis = 1.3,
     xlab = "N = Number of Stocks in Benchmark", cex.lab = 1.3)
lines(df$x, df$y2, type = "b", col = "cyan", lwd = 1.5, lty = "dashed")
lines(df$x, df$y3, type = "b", col = "red", lwd = 1.5, lty = "dotted")
text <- c("CovShrinkCC", "CovShrinkSFM", "CovSample")
col <- c("black", "cyan", "red")
lty = c("solid", "dashed", "dotted")
lwd = rep(1.5,3)
legend("topright", bty = "n", 1.3, legend = text, col = col, lwd = lwd, cex = 1.3)


#### Figure 4.19

#### NOTE: The code below takes about 1.5 minutes

stocksCRSPweekly <- getPCRAData("stocksCRSPweekly")
dateRange    <- c("2006-01-01", "2012-12-31")
stockItems <- c("Date", "TickerLast", "CapGroupLast", "Return", "MktIndexCRSP")
returnsAll <- selectCRSPandSPGMI("weekly",
                                 dateRange = dateRange,
                                 stockItems = stockItems, 
                                 factorItems = NULL, 
                                 subsetType = "CapGroupLast",
                                 subsetValues = "SmallCap", 
                                 outputType = "xts")
returns <- returnsAll[,1:30]
# PCRA::tsPlotMP(returns, layout = c(2,15))  # User optional plot
Market <- returnsAll[, 107]

# Except for Return.rebalancing, lw_linear_shrinkage, and base R functions, the
# functions below, up to an including line 766, are in the PortfolioAnalytics 
# package.
# Return.rebalancing is in the PerformnaceAnalytics package.
# lw_linear_shrinkage is in the covmat package

funds <- colnames(returns)
pspec <- portfolio.spec(funds)
pspec <- add.constraint(pspec, type="full_investment")
pspec <- add.constraint(pspec, type="long_only")
pspec <- add.objective(pspec, type="risk", name="var")

bt.gmv <- optimize.portfolio.rebalancing(returns, pspec,
                                         optimize_method = "CVXR",
                                         rebalance_on="weeks",
                                         training_period = 100)
wts.gmv <- extractWeights(bt.gmv)
GMV <- Return.rebalancing(returns, wts.gmv)

library(FactorAnalytics)
library(covmat)

covShrink <- function(R) {
  dat <- zoo::coredata(R)
  out <- list()
  out$sigma <- lw_linear_shrinkage(dat)$cov
  out$mu <- apply(dat, 2, mean)
  return(out)
}

name <- "GmvLOshrinkCovCC"

bt.gmv.shrinkCovCC <- optimize.portfolio.rebalancing(returns, pspec,
                                                     optimize_method = "CVXR",
                                                     rebalance_on = "weeks",
                                                     training_period = 100, 
                                                     momentFUN = covShrink)
wts.gmv.shrinkCovCC <- extractWeights(bt.gmv.shrinkCovCC)
GMV.shrinkCovCC <- Return.rebalancing(returns, wts.gmv.shrinkCovCC)
ret.comb <- na.omit(merge(GMV.shrinkCovCC, GMV, Market, all=F))
names(ret.comb) <- c(name, "GmvLO", "Market")

backtest.plot(ret.comb, colorSet = c("darkgreen", "black", "red"), 
              plotType = "cumRet", drawdown_on = NULL, ltySet = c(1,2,3))



