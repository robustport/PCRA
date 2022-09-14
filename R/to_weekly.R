#' @title Function to convert from daily to weekly returns.
#'
#' @description 
#' to_weekly will convert daily returns to weekly returns, while allowing the 
#' user flexibility in the choice of which day to end the week.
#'
#' @details
#' tbc
#' 
#' @param daily An xts object of daily returns.
#' @param days_in_week Default 5.
#' @param week_ending_day_str  controls what is the week end day. If the 
#' week_ending_day is “Wednesday”, then Wednesday is the end of a week. It 
#' means the return from previous Thursday to current Wednesday will be 
#' included in current Wednesday week’s cumulative return. If the beginning 
#' of the dataset is before week_ending_day (in this case, Wednesday) , the 
#' first week’s return would be from the begining of the dataset to the first 
#' week_ending day. If the last week is short of the full week,i.e.if 
#' week_ending_day is ’’Friday” and 2015-12-31 is a Thursday, then the last 
#' four day return will be labeled as 2016-01-01.
#'
#' @return rets
#' 
#' @examples
#' my_wkly_rets <- to_weekly(dlyrets, week_ending_day_str = 'Monday')
#' 
#' @export

to_weekly <- function(daily, 
                       days_in_week = 5,
                       week_ending_day_str = c('Monday','Tuesday','Wednesday',
                                               'Thursday','Friday','Saturday',
                                               'Sunday')
                       )
{
  
  week_ending_date <- switch(
    week_ending_day_str[1],
    'Monday' = 1,
    'Tuesday' = 2,
    'Wednesday' = 3,
    'Thursday' = 4,
    'Friday' = 5,
    'Saturday' = 6,
    'Sunday' = 7)
  
  # determine the weekday for input dataset
  n_future_years <- 365*1000
  all_days <- seq.Date(from = as.Date('1900-01-01'), length.out = n_future_years*7, by = '1 day')
  xmap <- data.frame(xdate = all_days, yday = rep(1:7, n_future_years))
  daily_df <- data.frame(xdate = as.Date(index(daily)), daily)
  xmap <- xmap[xmap$xdate <= max(daily_df$xdate) & xmap$xdate >= min(daily_df$xdate),]
  
  
  master <- merge(xmap, daily_df, all.x = TRUE)
  master[is.na(master)] <- 0
  
  ##
  # determine the begin and end index for return calculation based on week_ending selection
  wedate_idx <- c(1:nrow(master))[master$yday == week_ending_date]
  if(max(wedate_idx) == nrow(master)){
    # if the data ends on selected weekend date, the date interval should be shrunk by 1
    beg_idx <- c(1, wedate_idx[1:(length(wedate_idx)-1)]+1)
    end_idx <- wedate_idx
    last_date <- master$xdate[end_idx[length(end_idx)]]
  } else {
    # else, proceed as normal
    beg_idx <- c(1, wedate_idx+1)
    end_idx <- c(wedate_idx, nrow(master))
    last_date <- master$xdate[end_idx[length(end_idx)-1]] + 7 
  }
  
  ##
  # calculate return for each return series
  master_wodates <- master[-c(1:2)]
  rets <- apply(master_wodates, 2, function(x){
    # for each column (daily return), calculate weekly return
    tmp <- sapply(1:length(beg_idx), function(i){
      ret <- prod(1+x[beg_idx[i]:end_idx[i]]) - 1
      return(ret)
    })
    return(tmp)
  })
  
  ##
  # construct final output
  xdates <- master$xdate[end_idx]
  xdates[length(xdates)] <- last_date  # adjust last date to theoretical week end day
  rets <- xts(rets, order.by = xdates)
  names(rets) <- colnames(daily)
  return(rets)
  
}
