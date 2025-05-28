#' @title Merge stocksCRSP and factorsSPGMI
#'
#' @description Select and merge a set of CRSP stocks and SPGMI factors and
#' SPGMI factors over a range of dates
#'
#' @details
#' The date range, stock data items, and factor items have default values but
#' can be changed by the user
#' 
#' @importFrom utils globalVariables
#' 
#' @param stocks A data.table of stock returns and related data 
#' @param factors A data.table of factors and related data
#' @param dateSet A character vector providing a start data and an end 
#' date, having the same form as c("2006-01-31", "2010-12-31")
#' @param stockItems A character vector that is a subset of the names
#' of stocks data.table
#' @param factorItems A character vector that is a subset of the names
#' of factors data.table
#'
#' @return A merged data.table consisting of selected stocks and factors
#' 
#'@examples
#'data(stocksCRSPmonthly)
#'data(factorsSPGMI)
#'
#'stocks_factors <- selectCRSPandSPGMI(stocks = stocksCRSPmonthly, factors = factorsSPGMI,
#'                                     dateSet = c("2006-01-31", "2010-12-31"), 
#'                                     stockItems = c("Date", "TickerLast", 
#'                                                    "CapGroup", "Sector", 
#'                                                    "Return", "Ret13WkBill",
#'                                                    "mktIndexCRSP"),
#'                                     factorItems = c("BP", "LogMktCap", "SEV")
#'                                     )
#'
#'str(stocks_factors)
#'@export

mergeMarketData <- function(stocks = stocksCRSPmonthly, factors = factorsSPGMI,
                               dateSet = c("1993-01-31","2015-12-31"), 
                               stockItems = c("Date", "TickerLast", "CapGroup",
                                              "Sector", "Return", "Ret13WkBill",
                                              "mktIndexCRSP"),
                               factorItems = c("BP", "LogMktCap", "SEV")
                               )
{ 
  
  # to do list:
    # allow certain additional forms of subsetting (sectors, capgroups)
    # merge factorsSPGMI with daily and weekly stocksCRSP data sets

  commonNames <- intersect(names(stocks), names(factors))
  facModDat <- merge(stocks, factors, by = commonNames)
  facModDat <- facModDat[, .SD , .SDcols = c(stockItems, factorItems)]
  facModDat <- facModDat[!is.na(Sector)]
  facModDat <- facModDat[Date >= dateSet[1] & Date <= dateSet[2]]
  return(facModDat)
}

