#' @title CRSP Liquid Stocks Market Cap Group Counts
#' 
#' @description Biennial counts of bigcap, smallcap, microcap stocks among
#' liquid CRSP database stocks from 1964 to 2018 using weekly returns.  Bigcap
#' stocks consist of midcap, largecap, and megacap stocks.  For each contiguous
#' two-year interval, liquid stocks are those with no missing returns and at
#' most 4 returns with value 0. 
#'
#' @docType data
#'
#' @usage data(CRSPLiquidMktCapGrpsCnts)
#'
#' @format A multivariate xts object
#' 
#' @source The microcap, smallcap and bigcap groups were defined using the
#' using the 20th and 50th percentiles of the NYSE capitalization data as
#' breakpoints to separate these three market cap groups.  
#' 
#' @examples
#' library(PCRA)
#' library(zoo)
#' data(CRSPLiquidMktCapGrpsCnts)
#' names(CRSPLiquidMktCapGrpsCnts)
#' dim(CRSPLiquidMktCapGrpsCnts)
#' range(index(CRSPLiquidMktCapGrpsCnts))


#' @title crsp.returns8
#' 
#' @description Monthly returns of 8 stocks with tickers GHI, PBCI, MODI, MGJ, MAT,
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


#' @title Stock with Ticker FNB
#' 
#' @description Weekly returns (RET) of stock with ticker FNB for 2008
#'  
#' @docType data
#'
#' @usage data(retFNB)
#'
#' @format Univariate time series xts object
#' 
#' @source Center for Research in Security Prices, LLC (CRSP), an Affiliate of
#' the University of Chicago Booth School of Business.
#' 
#' @examples
#' library(PCRA)
#' library(zoo)
#' data(retFNB)
#' head(retFNB)
#' range(index(retFNB))
"retFNB"


#' @title Fama-French Weekly 3-Factor Model
#' 
#' @description Weekly values of the 3 factors MKT, SMB and HML
#' 
#' @docType data
#'
#' @usage data(datFF3W)
#'
#' @format Multivariate time series xts object
#' 
#' @source \url{https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html}
#' 
#' @examples
#' library(PCRA)
#' library(zoo)
#' data(datFF3W)
#' head(datFF3W)
#' range(index(datFF3W))
"datFF3W"


#' @title Fama-French-Carhart Weekly 4-Factor Model
#' 
#' @description Weekly values of the 4 factors MKT, SMB, HML and MOM
#' 
#' @docType data
#'
#' @usage data(datFF4W)
#'
#' @format Multivariate time series xts object
#' 
#' @source \url{https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html}
#' 
#' @examples
#' library(PCRA)
#' library(zoo)
#' data(datFF4W)
#' head(datFF4W)
#' range(index(datFF4W))
"datFF4W"


#' @title Stock with Ticker KBH
#' 
#' @description Weekly returns (RET) of stock with ticker KBH for 2007
#' and 2008, along with market returns (MKT) and risk-free rate (RF).
#'  
#' @docType data
#'
#' @usage data(retKBH)
#'
#' @format Multivariate time series xts object
#' 
#' @source Center for Research in Security Prices, LLC (CRSP), an Affiliate of
#' the University of Chicago Booth School of Business.
#' 
#' @examples
#' library(PCRA)
#' library(zoo)
#' data(retKBH)
#' head(retKBH)
#' range(index(retKBH))
"retKBH"


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


#' @title Stock with Ticker OFG
#' 
#' @description Weekly returns (RET) of stock with ticker OFG for 2007
#' and 2008, along with market returns (MKT) and risk-free rate (RF).
#'  
#' @docType data
#'
#' @usage data(retOFG)
#'
#' @format Multivariate time series xts object
#' 
#' @source Center for Research in Security Prices, LLC (CRSP), an Affiliate of
#' the University of Chicago Booth School of Business.
#' 
#' @examples
#' library(PCRA)
#' library(zoo)
#' data(retOFG)
#' head(retOFG)
#' range(index(retOFG))
"retOFG"

#' @title Stock with Ticker PSC
#' 
#' @description Weekly returns (RET) of stock with ticker PSC for 1987
#' and 1088, along with market returns (MKT) and risk-free rate (RF).
#'  
#' @docType data
#'
#' @usage data(retPSC)
#'
#' @format Multivariate time series xts object
#' 
#' @source Center for Research in Security Prices, LLC (CRSP), an Affiliate of
#' the University of Chicago Booth School of Business.
#' 
#' @examples
#' library(PCRA)
#' library(zoo)
#' data(retPSC)
#' head(retPSC)
#' range(index(retPSC))
"retPSC"


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


#' @title Stock with Ticker WTS
#' 
#' @description Weekly returns (RET) of stock with ticker WTS for 2009
#' and 2010, along with market returns (MKT) and risk-free rate (RF).
#'  
#' @docType data
#'
#' @usage data(retWTS)
#'
#' @format Multivariate time series xts object
#' 
#' @source Center for Research in Security Prices, LLC (CRSP), an Affiliate of
#' the University of Chicago Booth School of Business.
#' 
#' @examples
#' library(PCRA)
#' library(zoo)
#' data(retWTS)
#' head(retWTS)
#' range(index(retWTS))
"retWTS"


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