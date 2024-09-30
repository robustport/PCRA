#' Title Minimum Variance Long-Only Risky Assets Portfolio
#' 
#' @description Given a time series of risky asset returns and a target 
#' mean return, this function computes the mean and standard deviation 
#' of a fully-invested long-only minimum variance portfolio
#'
#' @param returns xts multivariate risky asset returns
#' @param mu Portfolio mean return specification
#' 
#' @details This function uses the PortfolioAnalytics function
#' optimize.portfolio.R and the PCRA function opt.outputMvoPCRA.
#' For details, see the man pages for those function.
#'
#' @return A list containing the weights, mean value, standard deviation
#' and Sharpe ratio, with default names Wgts, Mean, StdDeve, SR
#'
#' @author R. Douglas Martin
#'
#' @examples
#' args(minVarRiskyLO)
#' @export minVarRiskyLO
minVarRiskyLO <- function(returns, mu)
{
  # Returns of risky assets, and no risk-free T-Bill
  funds <- colnames(returns)
  pspec.base <- portfolio.spec(funds)
  pspec.fi <- add.constraint(portfolio = pspec.base, type = "full_investment")
  pspec.lo <- add.constraint(portfolio = pspec.fi, type = "long_only")
  pspec.mu <- add.constraint(pspec.lo, type = "return", return_target = mu)
  pspec.minvar <- add.objective(pspec.mu, type="risk", name="var")
  opt.minvar <- optimize.portfolio(returns, portfolio = pspec.minvar,
                                   optimize_method = "CVXR")
  opt.outputMvoPCRA(opt.minvar, returns, digits = 4, annualize = FALSE)
}
