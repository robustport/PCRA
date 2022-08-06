#' stocksCRSPxts
#' 
#' Function to extract a set of stocksCRSP data specified by a date
#' range, and a set of tickers, with convenient defaults
#'
#' @param data One of data.table objects stocksCRSP, stocksCRSPweekly and
#' stocksCRSPdaily
#' @param dateRange Character vector with two components a start date
#' and an end date
#' @param tickerSet A subset of tickers of the stocks in stocksCRSP
#'
#' @return An xts time series object
#' @export
#' 
#' @details The start date and end date of the dateRange paramter must have
#' the format "yyyy-mm-dd", and the default is the entire stocksCRSP data
#' range "1993-01-31" to "2015-12-32". If the tickerSet is NULL, then the 
#' resulting xts object contains all the stocks in stocksCRSP.
#'
#' @examples
#' args(stocksCRSPxts)
#' tickers4 <- c("DHR","CSL","AVP","AMWD")
#' dateRange <- c("2011-01-31","2015-12-31")
#' returns4 <- stocksCRSPxts(stocksCRSP, dateRange = dateRange,
#'                            tickerSet = tickers4) 
#' class(returns4)
#' dim(returns4)
#' names(returns4)
#' range(index(returns4))                         
stocksCRSPxts <- function(data,
                          dateRange = c("1993-01-31","2015-12-31"),
                          tickerSet = NULL)
{
  # This is a first version. To be expanded for capgroup selection
  select_cols    <- c("Date","Return","TickerLast","CapGroupLast")
  stocksAllTime <- stocksCRSP[, ..select_cols]
  Date1 <- dateRange[1]
  Date2 <- dateRange[2]
  stocks <- stocksAllTime[stocksAllTime$Date >= as.Date(Date1) & 
                            stocksAllTime$Date <= as.Date(Date2), ]
  returnsMat <- tapply(stocks[["Return"]], list(stocks$Date, stocks$TickerLast), I)
  returnsTS    <- xts(returnsMat,order.by = as.Date(rownames(returnsMat)))
  if(!is.null(tickerSet)) returnsTS   <- returnsTS[, tickerSet]
  returnsTS
}