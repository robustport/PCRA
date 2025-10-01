#' crsp.returns8
#' 
#' Monthly returns of 8 stocks with tickers GHI, PBCI, MODI, MGJ, MAT,
#' EMN, AMAT, AMGN from 1997 to 2001
#'
#' @docType data
#'
#' @usage data(crsp.returns8)
#'
#' @format A multivariate xts object
#' 
#' @source Center for Research in Security Prices, LLC (CRSP), an Affiliate of
#' the University of Chicago Booth School of Business. NOTE: CRSP data is not
#' covered by the GPL. Redistribution of the data in any form is not permitted,
#' and use of the data in derivative works is not permitted without the written
#' permission of CRSP.
#' 
#' @examples
#' library(PCRA)
#' library(zoo)
#' data(crsp.returns8)
#' names(crsp.returns8)
#' dim(crsp.returns8)
#' range(index(crsp.returns8))
"crsp.returns8"


#' @title Hedge Fund Strategies Returns
#' 
#' @description Monthly returns of 9 hedge fund strategies from 1994 to 2004
#'
#' @docType data
#'
#' @usage data(strategies)
#'
#' @format A multivariate xts object
#' 
#' @source Unknown
#' 
#' @examples
#' library(PCRA)
#' library(zoo)
#' data(strategies)
#' names(strategies)
#' dim(strategies)
#' range(index(strategies))
"strategies"


#' @title Federal Reserve Board Interest Rates
#' 
#' @description Federal Reserve Board monthly interest rates of 90 day Bill from 1934
#' to 2014.
#'
#' @docType data
#'
#' @usage data(FRBinterestRates)
#'
#' @format A time series zoo object
#' 
#' @source Federal Reserve Board
#' 
#' @examples
#' library(PCRA)
#' library(zoo)
#' data(FRBinterestRates)
#' class(FRBinterestRates)
#' range(index(FRBinterestRates))
"FRBinterestRates"


#' @title Weekly Returns 1980
#' 
#' @description Weekly returns of 10 stocks and 90 day T-Bill from November
#' 1980 through Octobber 1981.
#'
#' @docType data
#'
#' @usage data(WeeklyReturns1980)
#'
#' @format Multivariate time series zoo object
#' 
#' @source Center for Research in Security Prices, LLC (CRSP), an Affiliate of
#' the University of Chicago Booth School of Business.
#' 
#' @examples
#' library(PCRA)
#' library(zoo)
#' data(WeeklyReturns1980)
#' class(WeeklyReturns1980)
#' names(WeeklyReturns1980)
#' head(WeeklyReturns1980,2)
#' range(index(retDD))
"WeeklyReturns1980"


#' @title gfunds5
#' 
#' @description Monthly returns of 5 German investment funds November 1989
#' to July 2001: EM (emerging markets), PE (private equity), HY (high yield),
#' ALT (alternatives), and BND (fixed income)
#'
#' @docType data
#'
#' @usage data(gfunds5)
#'
#' @format Multivariate xts object
#' 
#' @source Unknown
#' 
#' @examples
#' library(PCRA)
#' library(zoo)
#' data(gfunds5)
#' class(gfunds5)
#' names(gfunds5)
#' range(index(gfunds5))
"gfunds5"


#' @title Stock with Ticker DD
#' 
#' @description Weekly returns (RET) of stock with ticker DD for 1986
#' and 1987, along with market returns (MKT) and risk-free rate (RF).
#'  
#' @docType data
#'
#' @usage data(retDD)
#'
#' @format Multivariate time series xts object
#' 
#' @source Center for Research in Security Prices, LLC (CRSP), an Affiliate
#' of the University of Chicago Booth School of Business.
#' 
#' @examples
#' library(PCRA)
#' library(zoo)
#' data(retDD)
#' head(retDD)
#' range(index(retDD))
"retDD"


#' @title Stock with Ticker EDS
#' 
#' @description Weekly returns (RET) of stock with ticker EDS for 2002
#' and 2003, along with market returns (MKT) and risk-free rate (RF).
#'  
#' @docType data
#'
#' @usage data(retEDS)
#'
#' @format Multivariate time series xts object
#' 
#' @source Center for Research in Security Prices, LLC (CRSP), an Affiliate of
#' the University of Chicago Booth School of Business.
#' 
#' @examples
#' library(PCRA)
#' library(zoo)
#' data(retEDS)
#' head(retEDS)
#' range(index(retEDS))
"retEDS"


#' @title Stock with Ticker MER
#' 
#' @description Weekly returns (RET) of stock with ticker MER for 2002
#' and 2003, along with market returns (MKT) and risk-free rate (RF).
#'  
#' @docType data
#'
#' @usage data(retMER)
#'
#' @format Multivariate time series xts object
#' 
#' @source Center for Research in Security Prices, LLC (CRSP), an Affiliate of
#' the University of Chicago Booth School of Business.
#' 
#' @examples
#' library(PCRA)
#' library(zoo)
#' data(retMER)
#' head(retMER)
#' range(index(retMER))
"retMER"


#' @title Stock with Ticker VHI
#' 
#' @description Weekly returns (RET) of stock with ticker VHI for 1990
#' and 1991, along with market returns (MKT) and risk-free rate (RF).
#'  
#' @docType data
#'
#' @usage data(retVHI)
#'
#' @format Multivariate time series xts object
#' 
#' @source Center for Research in Security Prices, LLC (CRSP), an Affiliate of
#' the University of Chicago Booth School of Business.
#' 
#' @examples
#' library(PCRA)
#' library(zoo)
#' data(retVHI)
#' head(retVHI)
#' range(index(retVHI))
"retVHI"


#' @title Earnings per Share of Invensys
#' 
#' @description Yearly earnings-per-share of company Invensys for 17 years.
#' The company's name was invensys prior to 2004.
#'  
#' @docType data
#'
#' @usage data(invensysEPS)
#'
#' @format A numeric vector
#' 
#' @source Corporate Finance Department of Dupont
#' 
#' @examples
#' library(PCRA)
#' data(invensysEPS)
#' invensysEPS
"invensysEPS"
