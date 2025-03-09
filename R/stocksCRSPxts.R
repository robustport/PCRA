#' @title Select CRSP Stocks Returns
#'
#' @description A function to extract a subset of the stocksCRSP data.table
#' specified by a date range and a set of tickers, with convenient defaults,
#' and convert it to an xts object
#'
#' @param data One of the data.table objects stocksCRSP, stocksCRSPweekly,
#' stocksCRSPdaily
#' @param dateRange Character vector with two components a start date
#' and an end date using format "yyyy-mm-dd".  Default is the entire
#' stocksCRSP data dates range c("1993-01-31","2015-12-32")
#' @param tickerSet A subset of tickers of the stocks in stocksCRSP. The
#' default is tickerSet = NULL, which results in selection of all stocks
#' in stocksCRSP.
#'
#' @return A multivariate xts object of stock returns
#' @export
#'
#' @examples
#' data.table::setDTthreads(1)
#' library(PCRA)
#' library(xts)
#' library(data.table)
#' class(stocksCRSP)
#' args(stocksCRSPxts)
#' tickers4 <- c("DHR","CSL","AVP","AMWD")
#' dateRange <- c("2011-01-31","2015-12-31")
#' returns4 <- stocksCRSPxts(stocksCRSP, dateRange = dateRange,
#'                            tickerSet = tickers4) 
#' class(returns4)
#' dim(returns4)
#' names(returns4)
#' range(index(returns4))                         
stocksCRSPxts <- function(data, dateRange = c("1993-01-31","2015-12-31"),
                          tickerSet = NULL)
{
  select_cols   <- c("Date","Return","TickerLast")
  stocksAllTime <- data[, select_cols, with = FALSE]
  Date1 <- dateRange[1]
  Date2 <- dateRange[2]
  stocks <- stocksAllTime[stocksAllTime$Date >= as.Date(Date1) & 
                            stocksAllTime$Date <= as.Date(Date2), ]
  returnsMat <- tapply(stocks[["Return"]], list(stocks$Date, stocks$TickerLast), I)
  returnsTS    <- xts(returnsMat,order.by = as.Date(rownames(returnsMat)))
  if(!is.null(tickerSet)) returnsTS   <- returnsTS[, tickerSet]
  returnsTS
}