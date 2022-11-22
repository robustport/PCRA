#' divHHI calculates a portfolio diversification index DIV
#' 
#' DIV is one minus the Herfindahl-Hirschman Index (HHI)index, which is defined
#' to be the sum of the squared portfolio weights.
#' 
#' The maximum HHI of a long-only portfolio is 1, which occurs when all of the
#' portfolio's investment is in a single asset, and correspondingly HHI = 0.
#'
#' @param weights 
#'
#' @return Time series of portfolio diversification values
#' @export
#'
#' @examples
#' args(divHHI)
divHHI <- function(weights){
  n.dates <- nrow(weights)
  if(n.dates < 1){
    print("empty data set")
    return()
  }
  diversification=rep(0, n.dates)
  for(i in 1:n.dates){
    diversification[i] <- 1 - sum(weights[i,]^2)
  }
  dates <- index(weights)
  DIV <- zoo(diversification,dates)
  return(DIV)
}
