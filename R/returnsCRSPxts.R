#' @title Select CRSP Stocks Returns
#' 
#' @description Uses selectCRSPandSPGMI to select a subset of the stocksCRSP
#' data, and convert it to an xts object that contains the returns of a set
#' of stocks, along with those of the MktIndexCRSP and the Ret13WkBill.
#' 
#' NOTE:  For this function to work, the selectCRSPandSPGMI must include the 
#' the stockItems TickerLast, MktIndexCRSP and Ret13WkBill.
#'
#' @param stocksData The data.table created by selectCRSPandSPGMI 
#'
#' @return A multivariate xts object
#' @export
#'
#' @examples
#' library(PCRA)
#' library(data.table)
#' stockItems <- c("Date","TickerLast","CapGroupLast","Return","MktIndexCRSP",
#'                "Ret13WkBill")
#' dateRange <- c("1997-01-31","2002-12-31")
#' stocksDT <- selectCRSPandSPGMI("monthly",dateRange = dateRange, stockItems =
#'                                  stockItems, factorItems = NULL)
#' stocksDT <- stocksDT[CapGroupLast == "SmallCap"]
#' ret <- returnsCRSPxts(stocksDT)
#' tickers <- unique(stocksDT[,TickerLast])
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