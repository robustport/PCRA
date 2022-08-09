#' @title Select and merge stocksCRSP and factorsSPGMI
#'
#' @description Function to enable the selection of subsets of either stocksCRSP
#' or factorsSPGMI data as well as merge the two data sets for risk model 
#' estimation.
#'
#' @details
#' The data sets to be used (stocksCRSP or factorsSPGMI), desired date range, 
#' and data subsets (e.g. sector, capgroup, or a ticker list) may be changed by
#' the user.
#' 
#' @importFrom utils globalVariables
#' 
#' @param periodicity Character "m","w","d" for monthly, weekly or daily data
#' @param dateSet A character vector providing a start data and an end 
#' date, having the same form as c("2006-01-31", "2010-12-31")
#' @param stockItems A character vector that is a subset of the names
#' of stocks data.table
#' @param factorItems A character vector that is a subset of the names
#' of factors data.table
#' @param sectorList A character vector that is a subset of the sectors in the 
#' stocksCRSP dataset
#' @param capGroupList A character vector that is a subset of the CapGroupLast
#' ("MicroCap","SmallCap","MidCap","LargeCap") variable
#' @param tickerList A character vector that contains a subset of tickerLast 
#' items 
#' @param returnsTS Boolean, TRUE if the user wants
#'
#' @return A data.table consisting of selected stocks and factor/stock data 
#' items
#' 
#'@examples
#'data(stocksCRSP)
#'data(factorsSPGMI)
#'
#'stocks_factors <- selectCRSPandSPGMI(periodicity = "m",
#'                                     dateSet = c("2006-01-31", "2010-12-31"), 
#'                                     stockItems = c("Date", "TickerLast", 
#'                                                    "CapGroup", "Sector", 
#'                                                    "Return", "Ret13WkBill",
#'                                                    "mktIndexCRSP"),
#'                                     factorItems = c("BP", "LogMktCap", "SEV"),
#'                                     sectorList = ("Energy"),
#'                                     capGroupList = ("SmallCap"),
#'                                     tickerList = c("ABT","ABM","MMM","NKE"))
#'
#'str(stocks_factors)
#'@export

mergeCRSPandSPGMI <- function(periodicity = "m",
                              dateSet = c("1997-12-31","2000-12-31"), 
                              stockItems = c("Date", "TickerLast", 
                                             "CapGroupLast", "Sector", "Return",
                                             "Ret13WkBill", "MktIndexCRSP"), 
                              factorItems = c("BP", "LogMktCap", "SEV"), 
                              sectorList = NULL, 
                              capGroupList = NULL, 
                              tickerList= NULL, 
                              returnsTS = TRUE)
{
  
  ### get correct version of stocksCRSP and merge with 
  ### factorsSPGMI at highest frequency
  
  # input checking
  stopifnot(periodicity %in% c("m","w","d"))
  
  # merge data
  stock_data <- switch(periodicity,"m" = stocksCRSP,"w" = stocksCRSPweekly, 
                       "d" = stocksCRSPdaily)
  factor_data <- factorsSPGMI
  fac_sel_idx <- which(colnames(stock_data) %in% colnames(factor_data))
  stock_data$key <- substr(as.character(stock_data$Date),
                          1,nchar(as.character(stock_data$Date[1]))-3)
  factor_data$key <- substr(as.character(factor_data$Date),
                           1,nchar(as.character(factor_data$Date[1]))-3)
  fac_sel <- c("TickerLast",colnames(factor_data)[-fac_sel_idx])
  factor_data <- factor_data[,..fac_sel]
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
                              