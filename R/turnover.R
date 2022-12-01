#' Calculate Portfolio Turnover
#' 
#' turnOver is the function to calculate a times series of n-1 turn-over
#' values for a times of n portfolio weight vectors, where the turnover
#' from time i to time i+1 is the sum of the absolute differences 
#' between the portfolio weights at time i and time i+1
#' 
#' @param weights multivariate xts object of portfolio weights
#'
#' @return Time series of turnover values
#' @export
#'
#' @examples
#' args(turnOver)
turnOver <- function(weights){
  dates <- index(weights)
  weights <- coredata(weights)
  n.asset <- ncol(weights)
  n.dates <- nrow(weights)
  if(n.dates <2){
    print("Less than 2 rebalancing dates!")
    return()
  }
  TurnOver <- rep(0,n.dates-1)
  for(i in 1:length(TurnOver)){
    TurnOver[i] <- sum(abs(weights[i+1,] - weights[i,]))
  }
  dates <- dates[-1]
  out <- zoo(TurnOver,order.by = dates)
  out
}

