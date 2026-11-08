#' @title Random Stocks Selection
#'
#' @description Random selection without replacement of a specified number of
#' stocks from the stocksCRSPmonthly data set for a specified date range, seed
#' and capitalization group
#'   
#' @param nStocks An integer numbers of stocks
#' @param dateRange A character vector in "year-month-day" format
#' @param seed An integer seed for random subset selection without replacement
#' @param capGroup A character value, one of MicroCap, SmallCap, MidCap, LargeCap
#' 
#' @details The integer nStocks needs to be less than the number of stocks in
#' the specified capGroup, specifically less than 34, 106, 67, 87 for
#' MicroCap, SmallCap, MidCap, LargeCap, respectively
#'
#' @returns An multivariate xts object consisting of the returns of the nStocks
#' @export
#'
#' @examples
#' args(randomStocks)
#' ret5 <- randomStocks(5)
#' names(ret5)
randomStocks <- function(nStocks, dateRange = c("2008-01-31","2012-12-31"),
                         seed = 933, capGroup = "MicroCap") 
{
  ## Requires PCRA, data.table, and xts packages
  
  stockItems <-  c("Date", "TickerLast", "CapGroupLast", "Return")
  
  datAll <- PCRA::selectCRSPandSPGMI("monthly",
                                     dateRange = dateRange, 
                                     stockItems = stockItems,
                                     factorItems = NULL,
                                     subsetType = "CapGroupLast",
                                     subsetValues = capGroup,
                                     outputType = "data.table")
  
  setnames(datAll, old = c("TickerLast","CapGroupLast"), 
           new = c("Ticker","CapGroup"))
  
  tickers <- unique(datAll[,Ticker])
  
  ## Extract a random subset of nStocks from datAll
  
  set.seed(seed)  
  tickerSet <- sample(tickers, size = nStocks)
  dat <- datAll[Ticker %in% tickerSet]
  
  ## Get xts returns
  
  datAllxts <- PCRA::selectCRSPandSPGMI("monthly",
                                        dateRange = dateRange, 
                                        stockItems = stockItems, 
                                        outputType = "xts")
  returns <- datAllxts[ , c(tickerSet)]
  returns
}