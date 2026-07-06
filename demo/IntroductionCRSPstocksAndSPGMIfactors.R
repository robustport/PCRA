### SECTION 1 Introduction

library(PCRA)
library(data.table)
class(stocksCRSPmonthly)
dim(stocksCRSPmonthly)   # Number of rows and columns
names(stocksCRSPmonthly) # Names of items in each column

class(factorsSPGMI)
dim(factorsSPGMI) 
names(factorsSPGMI)

# help(stocksCRSPmonthly)
# help(factorsSPGMI)

stocksCRSPmonthly[,1:4]

dat.df <- data.frame(stocksCRSPmonthly)
class(dat.df)
names(dat.df)

dat.Return <- dat.df[ , "Return"]
class(dat.Return)
dat.Return <- dat.df[ , "Return", drop = FALSE]
class(dat.Return)
dat.DateAndReturn <- dat.df[, c("Date","Return")]
class(dat.DateAndReturn)

dat <- stocksCRSPmonthly # This is a data.table
dat.Return <- dat[, Return]
class(dat.Return)
dat.Return <- dat[, list(Return)]
class(dat.Return)
dat.Return <- dat[, .(Return)]
class(dat.Return)
dat.DateAndReturn <- dat[, .(Date, Return)]
class(dat.DateAndReturn)

nMonths <- length(unique(stocksCRSPmonthly[,Date]))
nStocks <- length(unique(stocksCRSPmonthly[,TickerLast])) 
nMonths*nStocks # Number of rows
range(stocksCRSPmonthly[,Date]) # First and last date


### SECTION 2 Selecting stocksCRSP and factorsSPGMI

args(selectCRSPandSPGMI)

# help(selectCRSPandSPGMI)

## Example 1

stockItems1 <- c("Date","TickerLast","CapGroupLast","Return","MktIndexCRSP",
                 "Ret13WkBill")
dateRange <- c("1997-01-31","2010-12-31")
stocksSmall <- selectCRSPandSPGMI("monthly", dateRange = dateRange, 
                                  stockItems = stockItems1, factorItems = NULL, 
                                  subsetType = "CapGroupLast",
                                  subsetValues = "SmallCap",
                                  outputType = "data.table")
length(unique(stocksSmall[, TickerLast]))
dim(stocksSmall)
names(stocksSmall)
range(stocksSmall[, Date])

## Example 2

stockItems2 <- c("Date","TickerLast","CapGroupLast","Return")
factorItems <- c("LogMktCap","Beta60M","EP")
dateRange <- c("1997-01-31","2010-12-31")
stocksSmall3Fac <- selectCRSPandSPGMI("monthly", 
                                      dateRange = dateRange, 
                                      stockItems = stockItems2, 
                                      factorItems = factorItems, 
                                      subsetType = "CapGroupLast",
                                      subsetValues = "SmallCap",
                                      outputType = "data.table")
length(unique(stocksSmall3Fac[, TickerLast]))
dim(stocksSmall3Fac)
names(stocksSmall3Fac)

## Example 3

dateRange <- c("1997-01-31","2010-12-31")
stocksMicro <- selectCRSPandSPGMI("monthly", 
                                  dateRange = dateRange, 
                                  stockItems = stockItems1, 
                                  factorItems = NULL, 
                                  subsetType = "CapGroupLast",
                                  subsetValues = "MicroCap",
                                  outputType = "xts")
class(stocksMicro)
dim(stocksMicro)
names(stocksMicro)

names(stocksMicro)[c(35,36)] <- c("Market","Risk-Free")
tsPlotMP(stocksMicro[, c(1:4,35,36)], scaleType = "free", stripText.cex = .45, axis.cex = 0.4, lwd = 0.5)


## Section 2.1 Manipulation of xts Objects

library(xts)
datesIndex <- index(stocksMicro)
class(datesIndex)
length(datesIndex) # Recall 14 years x 12 months
head(datesIndex, 3)
tail(datesIndex, 3)
range(datesIndex)


## Section 2.2 A Simple Selection Function for stocksCRSP Data

args(stocksCRSPxts)

stocksAll <- stocksCRSPxts(stocksCRSPmonthly)
class(stocksAll)
dim(stocksAll)
library(xts)
range(index(stocksAll))

# Extract cross-sections of stocksCRSP monthly returns on 6 time intervals

dates1 <- c("1993-01-31","1995-12-31")
dates2 <- c("1996-01-31","1999-12-31")
dates3 <- c("2000-01-31","2003-12-31")
dates4 <- c("2004-01-31","2007-12-31")
dates5 <- c("2008-01-31","2011-12-31")
dates6 <- c("2012-01-31","2015-12-31")
ret1  <- stocksCRSPxts(stocksCRSPmonthly,dateRange = dates1)
ret2  <- stocksCRSPxts(stocksCRSPmonthly,dateRange = dates2)
ret3  <- stocksCRSPxts(stocksCRSPmonthly,dateRange = dates3)
ret4  <- stocksCRSPxts(stocksCRSPmonthly,dateRange = dates4)
ret5  <- stocksCRSPxts(stocksCRSPmonthly,dateRange = dates5)
ret6  <- stocksCRSPxts(stocksCRSP,dateRange = dates6)

# Compute cross-section of excess kurtosis values on those time intervals
eKR1 <- apply(coredata(ret1),2,KRest)
eKR2 <- apply(coredata(ret2),2,KRest)
eKR3 <- apply(coredata(ret3),2,KRest)
eKR4 <- apply(coredata(ret4),2,KRest)
eKR5 <- apply(coredata(ret5),2,KRest)
eKR6 <- apply(coredata(ret6),2,KRest)
eKR <- cbind(eKR1,eKR2,eKR3,eKR4,eKR5,eKR6)
times <- c("1993-1995","1996-1999","2000-2003","2004-2007","2008-2011","2012-2015")

boxplot(eKR, xaxt = "n", ylim = c(-2,12), main = "Excess Kutosis stocksCRSP",
        cex.main = 1.5, col = "cyan")
axis(1, at=1:6, labels=times, cex.axis = 1.1)
abline(h=0, lty = "dotted")


## Section 2.3 Using the Weekly and Daily stocksCRSP Data

stocksCRSPweekly <- getPCRAData(dataset = "stocksCRSPweekly")
dateRange <- c("2000-01-7","2000-12-31")
stocksMicroWeekly <- selectCRSPandSPGMI("weekly", 
                                        dateRange = dateRange, 
                                        stockItems = stockItems1, 
                                        factorItems = NULL, 
                                        subsetType = "CapGroupLast",
                                        subsetValues = "MicroCap",
                                        outputType = "xts")

dim(stocksMicroWeekly)
names(stocksMicroWeekly)[35] <- "Market"

tsPlotMP(stocksMicroWeekly[, c(1:5,35)], scaleType = "free", stripText.cex = .45,
         yname = "RETURNS", layout = c(1,6), type = "l", axis.cex = 0.7, lwd = 0.6)


### SECTION 3 Basic Data.Table Manipulations

## Section 3.1 Sector and Capitalization Group Counts

lastDate <- tail(factorsSPGMI[,Date],1)
factorsSPGMILast <- factorsSPGMI[Date == lastDate]
factorsSPGMILast[ , .N, by = Sector]
factorsSPGMILast[ , .N, by = CapGroup]


## Section 3.2 Extracting a Subset of the Variables

colNames <- c("Date","TickerLast","Return","Beta60M","EP")                      
stocksSmall3Fac <- stocksSmall3Fac[, .SD , .SDcols = colNames]
names(stocksSmall3Fac)


## Section 3.3 Renaming Variables

setnames(stocksSmall, old = c("MktIndexCRSP", "Ret13WkBill"),
         new = c("Market","RiskFree"))
names(stocksSmall)


## Section 3.4 Extracting a Sector of SmallCap Stocks

stockItems <- c("Date","TickerLast","CapGroupLast","Return", "Sector")
stocksSmallCap <- selectCRSPandSPGMI("monthly", 
                                     stockItems = stockItems, factorItems = NULL,
                                     subsetType = "CapGroupLast",
                                     subsetValues = "SmallCap",
                                     outputType = "data.table")
length(unique(stocksSmallCap[,TickerLast])) # Verifying that there are 106 smallcap stocks
stocksSmallCapIT <- stocksSmallCap[Sector == "Information Technology"]
unique(stocksSmallCapIT[,TickerLast])


## Section 3.5 Selecting a Time Range of a Set of Stocks and Factors

stocksSmallCapIT5Year <- stocksSmallCapIT[stocksSmallCapIT[,Date] >= as.Date("2010-01-31") & 
                                            stocksSmallCapIT[,Date] <= as.Date("2014-12-31"), ]
range(stocksSmallCapIT5Year[,Date])
length(unique(stocksSmallCapIT5Year[,Date]))

