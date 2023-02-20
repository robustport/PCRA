#' @title HHI Based Diversification Index
#' 
#' @description divHHI calculates a portfolio diversification index DIV. The DIV
#' is equal to 1 minus the Herfindahl-Hirschman Index (HHI), which is defined
#' as the sum of the squared portfolio weights. The maximum HHI of a long-only
#' portfolio is 1, which occurs when all of the portfolio's investment is in a
#' single asset, and correspondingly HHI = 0.
#'
#' @param weights A numeric vector of portfolio weights
#'
#' @return a zoo time series object containing portfolio diversification values
#' @export
#'
#' @examples
#' args(divHHI)
divHHI <- function(weights){
  n.dates <- nrow(weights)
  if(n.dates < 1){
    warning("empty data set")
    return()
  }
  diversification <- rep(0, n.dates)
  for(i in 1:n.dates){
    diversification[i] <- 1 - sum(weights[i,]^2)
  }
  dates <- index(weights)
  DIV <- zoo(diversification, dates)
  return(DIV)
}
