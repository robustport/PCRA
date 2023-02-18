#' @title Portfolio Turnover
#' 
#' @description Calculates T-1 turn-over values for a times of portfolio
#' weight vectors from time t = 1 to time t = T, where the turnover from time
#' t-1 to time t is the sum of the absolute differences between the portfolio
#' weights at time t-1 and time t.
#' 
#' @param weights A multivariate xts object of portfolio weights
#'
#' @return A zoo time series object containing T-1 turnover values
#' @export
#'
#' @examples
#' args(turnOver)
#' 
turnOver <- function(weights){
  dates <- index(weights)
  weights <- coredata(weights)
  n.asset <- ncol(weights)
  n.dates <- nrow(weights)
  if(n.dates < 2){
    print("Less than 2 rebalancing dates!")
    return()
  }
  TurnOver <- rep(0,n.dates-1)
  for(i in 1:length(TurnOver)){
    TurnOver[i] <- sum(abs(weights[i+1,] - weights[i,]))
  }
  dates <- dates[-1]
  out <- zoo(TurnOver, order.by = dates)
  out
}

