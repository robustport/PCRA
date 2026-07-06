#' @title Minimum Variance Portfolio
#'
#' @description Given a time series of asset returns and risk-free T-Bill
#' returns, a target mean return, and a specification of whether the asset
#' weights are constrained to be long-only, this function compputes the 
#' weights of a fully-invested minimum variance portfolio
#' 
#' @param returns xts multivariate returns object that contains
#' the returns of the risk-free T-Bill in the last column 
#' @param mu0 Minimum variance portfolio mean return
#' @param LO Logical variable with default FALSE
#' @param bnd Bound on asset weights
#' @param bndRF a bound on the risk-free T-Bill weight
#' 
#' @return A list of the minimum variance portfolio numeric weights, 
#' mean value, volatility, and Sharpe Ratio.
#' 
#' @examples
#' args(minVarCashRisky)
#' @export
minVarCashRisky <- function(returns, mu0, LO = FALSE, bnd = 1000, bndRF = 100)
{
  # Returns must contain time-varying risk-free rate in last component
  p <- ncol(returns)
  p1 <- p-1
  if(LO) {
    min <- c(rep(0,p1), -bndRF)
    max <- c(rep(1,p1), +bndRF)  
  } else {
    min <- -bnd
    max <- +bnd
  }
  funds <- colnames(returns)
  pspec.base <- portfolio.spec(funds)
  pspec.fi <- add.constraint(portfolio = pspec.base, type = "full_investment")
  pspec.box <- add.constraint(portfolio = pspec.fi, type = "box", 
                              min = min, max = max)
  pspec.mu <- add.constraint(pspec.box, type = "return", return_target = mu0)
  pspec.minvar <- add.objective(portfolio = pspec.mu, type = "risk", name="var")
  opt.minvar <- optimize.portfolio(returns, portfolio = pspec.minvar, 
                                   optimize_method = "CVXR")
  RF <- mean(returns[ , p])
  x <- opt.outputMvoPCRA(opt.minvar, returns, rf = RF, annualize = FALSE,
                    digits = 4)
  return(x)
}