#' @title A Simple Minimum Variance Portfolio
#'
#' @description Given a time series of returns that includes a 
#' risk-free rate, a target mean return and set of weights constraints,
#' this function the weights of a minimum variance portfolio
#' 
#' @param returns an xts multivariate returns object 
#' @param mu0 a specified minimum variance portfolio mean return
#' @param min a minimum weight constrain
#' @param max a maximum weight constrain 
#'
#' @return A list of the minimum variance portfolio numeric weights, 
#' mean value, volatility, and Sharpe Ratio.
#' @export
#'
#' @examples
#' args(minVarCashRisky)
minVarCashRisky <- function(returns, mu0, min = -1000, max = +1000)
{
  # Returns must contain time-varying risk-free rate in last component
  funds <- colnames(returns)
  pspec.base <- portfolio.spec(funds)
  pspec.fi <- add.constraint(portfolio =pspec.base, type = "full_investment")
  pspec.lo <- add.constraint(portfolio =  pspec.fi, type = "box", min = min, 
                             max = max)
  pspec.mu <- add.constraint(pspec.lo, type = "return", return_target = mu0)
  pspec.minvar <- add.objective(portfolio = pspec.mu, type="risk", name="var")
  opt.minvar <- optimize.portfolio(midcap10andRF, portfolio = pspec.minvar,
                                   optimize_method="quadprog")
  opt.outputMvo(opt.minvar, midcap10andRF, rf = RF, digits = 4)
}