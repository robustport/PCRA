#' returnsCRSPxts
#' 
#' This function uses the data.table created by selectCRSPandSPGMI to create
#' a multivarite xts time series object that contains the returns of the stocks
#' with tickers in the TickerLast component of the data.table, the returns of
#' the MktIndexCRSP, and the returns of the Ret13WkBill.
#'
#' @param stocksData The data.table created by selectCRSPandSPGMI 
#'
#' @return A multivariate times series object described above
#' @export
#'
#' @examples
#' library(PCRA)
#' library(data.table)
#' stockItems <- c("Date","TickerLast","CapGroupLast","Return","MktIndexCRSP",
#'                "Ret13WkBill")
#' dateRange <- c("1997-01-31","2002-12-31")
#' stocksDT <- selectCRSPandSPGMI("monthly",dateSet = dateRange, stockItems =
#'                                  stockItems, factorItems = NULL)
#' stocksData <- stocksDT[[1]][CapGroupLast == "SmallCap"]
#' ret <- returnsCRSPxts(stocksData)
#' tickers <- unique(stocksData[,TickerLast])
#' tickers10 <- tickers[11:20]
#' colnames <- c(tickers10,"Market","RiskFree")
#' head(ret[,colnames],1)
returnsCRSPxts <- function(stocksData)
{
  returnsMat <- tapply(stocksData[["Return"]], list(stocksData$Date, 
                                                    stocksData$TickerLast), I)
  returns <- xts(returnsMat, order.by = as.Date(rownames(returnsMat)))
  MarketDT <- unique(stocksData[,c("Date","MktIndexCRSP")])
  Market <- xts(MarketDT[,MktIndexCRSP],order.by = as.Date(MarketDT[,Date]))
  RiskFreeDT <- unique(stocksData[,c("Date","Ret13WkBill")])
  RiskFree <- xts(RiskFreeDT[,Ret13WkBill],order.by = as.Date(RiskFreeDT[,Date]))
  returnsMktRF <- cbind(returns,Market,RiskFree)
  returnsMktRF
}