#' @title Select and merge data from the stocksCRSP and factorsSPGMI data sets
#'
#' @description Select data from stocksCRSP and merge with factorsSPGMI 
#' for use in risk model estimation or returns analysis. This version of 
#' selectCRSPandSPGMI allows various options for subsetting. Users may specify 
#' a dateRange for the data as well as specifying specific lists of tickers, 
#' market capitalization groups, or sectors via the subsetType and subsetValues
#' parameters. Additionally, for data.table output,  users may select specific 
#' columns for each of stocksCRSP and factorsSPGMI to be included in the final 
#' output via the stockItems and factorItems parameters.
#'
#' @details
#' Users select a periodicity for the data (stocksCRSP is available in daily, 
#' weekly, and monthly variants). When weekly or daily data are selected, the
#' function re-samples the lower frequency factorsSPGMI data up to the 
#' chosen stocksCRSP frequency.
#' 
#' Users may select all columns from both data sets, a specified set of columns,
#' or by setting either stockItems or factorItems to "NULL", may select only 
#' items from the other data set (that is, if only the stocksCRSP data is 
#' desired, set factorItems to NULL). 
#' 
#' Users may select a specific range of dates ("dateRange") for the data.
#' 
#' Smaller sub-samples of the data (fewer rows) can be returned by
#' selecting a specific Sectors, CapGroupLast (MicroCap, SmallCap, etc.) of 
#' interest, or by specifying a list of TickerLast values for which data can be
#' returned. This is accomplished via the subsetType and subsetValues 
#' parameters.
#' 
#' @param periodicity Character "monthly","weekly","daily". Currently only 
#' "monthly" is supported.
#' @param dateRange A character vector providing a start data and an end 
#' date, having the same form as c("2006-01-31", "2010-12-31").
#' @param stockItems A character vector that is a subset of the names
#' of columns in the stocksCRSP data.table. Set to "NULL" when no data from this
#' data set is desired in the final output.
#' @param factorItems A character vector that is a subset of the names
#' of columns in the factorsSPGMI data.table. Set to "NULL" when no data from this
#' data set is desired in the final output.
#' @param subsetType Character "TickerLast", "sector" or "CapGroupLast". Default 
#' NULL for no sub-setting.
#' @param subsetValues Character vector containing either a list of TickerLast
#' values, Sector values, or CapGroup values.
#' @param outputType Character "xts" for a wide xts returns matrix or 
#' "data.table" for a long format data.table for risk model estimation. Set to
#' "xts" by default.
#'
#' @return Either an xts returns matrix augmented with the risk free rate and 
#' market return values or a data.table consisting of selected stocks and 
#' factor/stock data.
#' 
#' @examples
#' data(stocksCRSP)
#' data(factorsSPGMI)
#'
#' return_data <- selectCRSPandSPGMI(periodicity = "monthly",
#'                                     dateRange = c("2006-01-31", "2010-12-31"),
#'                                     stockItems = c("Date", "TickerLast",
#'                                     "CapGroupLast", "Sector", "Return",
#'                                     "Ret13WkBill", "MktIndexCRSP"),
#'                                     factorItems = NULL,
#'                                     subsetType = NULL,
#'                                     subsetValues = NULL,
#'                                     outputType = "xts")
#'
#' length(unique(stocksCRSP$TickerLast)) 
#' dim(return_data) #includes all tickers plus rf & market return columns
#'
#' stocks_factors <- selectCRSPandSPGMI(periodicity = "monthly",
#'                                     dateRange = c("2006-01-31", "2010-12-31"),
#'                                     stockItems = c("Date", "TickerLast",
#'                                     "CapGroupLast", "Sector", "Return",
#'                                     "Ret13WkBill", "MktIndexCRSP"),
#'                                     factorItems = c("BP", "LogMktCap", "SEV"),
#'                                     subsetType = NULL,
#'                                     subsetValues = NULL,
#'                                     outputType = "data.table")
#' names(stocks_factors)
#' str(stocks_factors)
#'
#' @export

selectCRSPandSPGMI <- function(periodicity = "monthly",
                               dateRange = c("1993-01-31","2015-12-31"), 
                               stockItems = c("Date", "TickerLast", 
                                              "CapGroupLast", "Sector", "Return",
                                              "Ret13WkBill", "MktIndexCRSP"),
                               factorItems = c("BP", "LogMktCap", "SEV"),
                               subsetType = NULL,
                               subsetValues = NULL,
                               outputType= "xts")
{
  
  ### input checking
  # stop if incorrect periodicity selected
  stopifnot(periodicity %in% c("monthly","weekly","daily")) 
  # stop if incorrect columns selected from stocksCRSP
  stopifnot(stockItems %in% colnames(stocksCRSP))
  # stop if incorrect columns selected from factorsSPGMI
  stopifnot(factorItems %in% colnames(factorsSPGMI)) 
  # stop if incorrect outputType selected
  stopifnot(outputType %in% c("xts", "data.table")) 
  # stop if incorrect subsetting varible selected
  stopifnot(subsetType %in% c("TickerLast","Sector","CapGroupLast"))
  
  # merge data (resamples factorsSPGMI to higher frequency is daily or 
  # weekly stocksCRSP data are used)
  stock_data <- switch(periodicity,
                       "monthly" = stocksCRSP,
                       "weekly" = stocksCRSPweekly, 
                       "daily" = stocksCRSPdaily)
  stock_data <- stock_data[, ..stockItems]
  stock_data$key <- substr(as.character(stock_data$Date),
                           1,nchar(as.character(stock_data$Date[1]))-3)
  factor_data <- factorsSPGMI
  fac_sel_idx <- which(colnames(stock_data) %in% colnames(factor_data))
  fac_sel <- c("Date","TickerLast",factorItems)
  factor_data <- factor_data[, ..fac_sel]
  factor_data$key <- substr(as.character(factor_data$Date),
                            1,nchar(as.character(factor_data$Date[1]))-3)
  merged_data <- merge(stock_data, factor_data, by = c("key", "TickerLast"), 
                       all.x=TRUE)
  merged_data <- merged_data[, c("key", "Date.y"):=NULL]
  colnames(merged_data)[colnames(merged_data) == "Date.x"] ="Date"
  
  # subset by column and row based on user input
  # date sub-setting
  if (!is.null(subsetType)) {
    merged_data <- merged_data[merged_data$Date >= dateRange[1] & merged_data$Date <= dateRange[2],]
  }
  
  # subsetType sub-setting
  if (!is.null(subsetType)) {
    if (subsetType == "TickerLast") {
      stopifnot(subsetValues %in% unique(stocksCRSP$TickerLast))
      merged_data <- merged_data[merged_data$TickerLast %in% subsetValues,]
    } else if (subsetType == "Sector") {
      stopifnot(subsetValues %in% unique(stocksCRSP$Sector))
      merged_data <- merged_data[merged_data$Sector %in% subsetValues,]
    } else {
      stopifnot(subsetValues %in% unique(stocksCRSP$CapGroupLast))
      merged_data <- merged_data[merged_data$CapGroupLast %in% subsetValues,]
    }
  }
  
  # get combined returns xts matrix
  
  returns_mat <- tapply(merged_data[["Return"]], list(merged_data$Date,merged_data$TickerLast), I)
  returns <- xts(returns_mat, order.by = as.Date(rownames(returns_mat)))
  
  # check whether user requested the MktIndexCRSP variable in the returns matrix
  # if so, append to xts output matrix
  if ("MktIndexCRSP" %in% stockItems) {
    market_dt <- unique(stock_data[,c("Date","MktIndexCRSP")])
    market <- xts(market_dt[,MktIndexCRSP],order.by = as.Date(market_dt[,Date]))
    colnames(market) <- "MktIndexCRSP"
    market <- market[index(returns),]
  } else {market <- NULL}
  
  # check whether user requested the Ret13WkBill variable in the returns matrix
  # if so, append to xts output matrix
  if ("Ret13WkBill" %in% stockItems) {
    rf_dt <- unique(stock_data[,c("Date","Ret13WkBill")])
    rf <- xts(rf_dt[,Ret13WkBill],order.by = as.Date(rf_dt[,Date]))
    colnames(rf) <- "Ret13WkBill"
    rf <- rf[index(returns),]
  } else {rf <- NULL}
  
  # create final combined_ret xts object with optional MktIndexCRSP and Ret13WkBill variables
  combined_ret <- cbind(returns,market,rf)
  
  # determine final output type
  if(outputType == "xts") {
    final_data <- combined_ret
  } else {
    final_data <- merged_data
  }
  
  return(final_data)
  
}