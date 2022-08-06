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