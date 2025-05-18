#' @title Function to convert from daily to weekly returns.
#'
#' @description 
#' to_monthly will convert daily returns to monthly returns.
#'
#' @details These will be added
#' 
#' @importFrom utils globalVariables
#' @import data.table
#' 
#' @param daily An xts object of daily returns.
#' @param index_last Controls whether the return date label will fall on the 
#' month end date provided in the dataset (usually the final trading date of the
#' month in most financial data sets) or the calendar month end date 
#' (potentially a non-trading date). Regardless, the returns are identical 
#' under two scenarios, but the date may differ for the "month end".
#'
#' @return monthly
#' 
#' @examples
#' args(to.monthly)
#' 
#' @export

to_monthly <- function(daily, index_last = TRUE)
{
  
  monthly <- apply.monthly(daily, PerformanceAnalytics::Return.cumulative)
  # using last calendar day of the month for index instead of the last available date in data
  if (index_last) {
    end_of_month <- function(xdate) {
      # get basic date info
      xdate <- as.Date(xdate)
      xyrmth_part <- format(xdate, "%Y-%m-")
      xyear_old <- as.numeric(format(xdate, "%Y"))
      xmonth_old <- as.numeric(format(xdate, "%m"))
      xday_old <- as.numeric(format(xdate, "%d"))
      
      if(xmonth_old == 12){
        xyear <- xyear_old + 1
        xmonth <- 1
      } else {
        xyear <- xyear_old
        xmonth <- xmonth_old + 1
      }
      xdate_new <- as.Date(paste0(xyear, "-", xmonth, "-", "01"), format = '%Y-%m-%d')-1
      
      return(xdate_new)
    }
    index(monthly) <- do.call(c, lapply(index(monthly), end_of_month))
  }
  return(monthly)
  
}
