#' @title Merge and select subsets from stocksCRSP and factorsSPGMI data sets
#'
#' @description Select data from stocksCRSP (daily, weekly, and monthly) and 
#' merge with factorsSPGMI while also optionally selecting subsets by rows 
#' (selecting on date ranges, tickers, capitalization groups, and sectors or by 
#' columns (selecting a subset of the factor data for each ticker).
#'
#' @details
#' Users select a periodicity for the data (stocksCRSP is available in daily, 
#' weekly, and monthly variants) and this function will resample the monthly
#' factorsSPGMI data to the selected periodicity. 
#' Users may select all columns from both data sets, a specified set of columns,
#' or by setting either stockItems or factorItems to "NULL", may select only 
#' items from the other data set (that is, if only the stocksCRSP data is 
#' desired, set factorITEMS to NULL). 
#' Smaller sub-samples of the data (fewer rows) can be returned by
#' selecting a specific date range, sectors, CapGroups (MicroCap, SmallCap, 
#' etc.) of interest, or by specifying a list of tickers for which data can be
#' returned. Care should be taken to ensure consistency in combined 
#' restrictions when specifying CapGroup, Sector, and TickerLast values.
#' 
#' @importFrom utils globalVariables
#' @depends data.table
#' 
#' @param periodicity Character "monthly","weekly","daily".
#' @param dateSet A character vector providing a start data and an end 
#' date, having the same form as c("2006-01-31", "2010-12-31").
#' @param stockItems A character vector that is a subset of the names
#' of columns in the stocksCRSP data.table
#' @param factorItems A character vector that is a subset of the names
#' of columns in the factorsSPGMI data.table
#' @param sectorList A character vector that is a subset of the sectors in the 
#' stocksCRSP dataset. Set to NULL to return all sectors.
#' @param capGroupList A character vector that is a subset of the CapGroupLast
#' ("MicroCap","SmallCap","MidCap","LargeCap") variable. Set to NULL to return
#' all CapGroups.
#' @param tickerList A character vector that contains a subset of tickerLast 
#' items. Set to NULL to return all tickers.
#' @param returnsTS Boolean, TRUE if the user wants xts return output in wide
#' format. Any other value for this variable will return just the stock data
#' data.table.
#'
#' @return Either a list containing a two items: data.table consisting of 
#' selected stocks and factor/stock data as well as (optionally, if 
#' returnsTS==TRUE) an xts object with time series returns for each stock in 
#' the final sample. If returnsTS is not TRUE, only the data.table is returned.
#' 
#'@examples
#'data(stocksCRSP)
#'data(factorsSPGMI)
#'
#'stocks_factors <- selectCRSPandSPGMI(periodicity = "monthly",
#'                                     dateSet = c("2006-01-31", "2010-12-31"), 
#'                                     stockItems = c("Date", "TickerLast", 
#'                                     "CapGroupLast", "Sector", "Return",
#'                                     "Ret13WkBill", "MktIndexCRSP"),
#'                                     factorItems = c("BP", "LogMktCap", "SEV"),
#'                                     sectorList = NULL,
#'                                     capGroupList = NULL,
#'                                     tickerList = NULL,
#'                                     returnsTS = FALSE)
#'
#'str(stocks_factors)
#'@export

selectCRSPandSPGMI <- function(periodicity = "monthly",
                               dateSet = c("1993-01-31","2015-12-31"), 
                               stockItems = c("Date", "TickerLast", 
                                              "CapGroupLast", "Sector", "Return",
                                              "Ret13WkBill", "MktIndexCRSP"),
                               factorItems = c("BP", "LogMktCap", "SEV"), 
                               sectorList = NULL, 
                               capGroupList = NULL, 
                               tickerList= NULL, 
                               returnsTS = TRUE)
{
  
  # as recommended to clear "no visible binding for global variable" build NOTE 
  #Date <- factorsSPGMI <- stocksCRSP <- NULL
  
  ### get correct version of stocksCRSP and merge with 
  ### factorsSPGMI at highest frequency
  
  # input checking
  stopifnot(periodicity %in% c("monthly","weekly","daily"))
  
  # merge data
  stock_data <- switch(periodicity,
                       "monthly" = stocksCRSP,
                       "weekly" = stocksCRSPweekly, 
                       "daily" = stocksCRSPdaily)
  factor_data <- factorsSPGMI
  fac_sel_idx <- which(colnames(stock_data) %in% colnames(factor_data))
  stock_data$key <- substr(as.character(stock_data$Date),
                          1,nchar(as.character(stock_data$Date[1]))-3)
  factor_data$key <- substr(as.character(factor_data$Date),
                           1,nchar(as.character(factor_data$Date[1]))-3)
  fac_sel <- c("TickerLast",colnames(factor_data)[-fac_sel_idx])
  factor_data <- factor_data[,fac_sel,with=FALSE]
  merged_data <- merge(stock_data, factor_data, by = c("key", "TickerLast"), 
                       all.x=TRUE)
  
  
  ### subset by column and row based on user input

  # date subset
  if (!is.null(dateSet)) {
    merged_data <- merged_data[Date >= dateSet[1] & Date <= dateSet[2]]
  }
  
  # keep selected columns
  merged_data <- merged_data[, .SD , .SDcols = c(stockItems, factorItems)]
  
  # sectorList subset
  if (!is.null(sectorList)) {
    merged_data <- merged_data[merged_data$Sector %in% sectorList,]
  }
  
  # capGroupList subset
  if (!is.null(capGroupList)) {
    merged_data <- merged_data[merged_data$CapGroupLast %in% capGroupList,]
  }
  
  # tickerList subset
  if (!is.null(tickerList)) {
    merged_data <- merged_data[merged_data$TickerLast %in% tickerList,]
  }
  
  ### format returns as a wide matrix
  returns_mat <- tapply(merged_data[["Return"]], 
                        list(merged_data$Date, merged_data$TickerLast), I)
  returns_ts  <- xts::xts(returns_mat,order.by = as.Date(rownames(returns_mat)))

  
  ### output
  if(returnsTS == TRUE) {
    return(list(merged_data,returns_ts))
  } else {
    return(merged_data) 
  }
  
}
                              