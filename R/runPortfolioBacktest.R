#' Run Portfolio Backtest and Plot
#'
#' Perform a backtest for a list of portfolio specifications. Portfolio objectives,
#' constraints, rolling window, and rebalancing frequency can be customized using
#' the same conventions as used in \code{PortfolioAnalytics}.
#'
#' @param return_portfolio An \code{xts} matrix of asset returns. Column names must
#'   match the assets in each \code{portfolio.spec} object.
#' @param portfolio_list A list of \code{portfolio.spec} objects built
#'   with \code{PortfolioAnalytics}.
#' @param portfolio_names Character vector of names corresponding to each
#'   portfolio in \code{portfolio_list}.
#' @param market_return An \code{xts} single-column object of benchmark returns.
#' @param rebalance_on Character string passed to \code{optimize.portfolio.rebalancing()}. 
#'   See \code{\link[xts]{endpoints}} for valid names.
#' @param rolling_window Positive integer. Length of the rolling estimation
#'   window in periods.
#' @param optimize_method Character string specifying the solver. Default \code{"CVXR"}.
#' @param moment_list If different moment functions are passed into multiple GMV
#'   portfolios, please define each moment function via this parameter. For the
#'   portfolio that do not require moment function, please pass NULL. Example:
#'   \code{list('custom.covRob.Rocke', NULL, NULL)}.
#' @param save_plot Logical. Whether to save the plot to a PNG file. Default \code{TRUE}.
#' @param plot_path Character string. Full file path for the PNG output.
#'   Required when \code{save_plot = TRUE}.
#' @param plot_name Plot name for the PNG output. Default \code{"backtest"}.
#' @param colorSet Optional character vector of colors passed to \code{backtest.plot()}.
#' @param ltySet Optional integer vector of line types passed to \code{backtest.plot()}.
#'
#' @return A list:
#' \describe{
#'   \item{\code{returns}}{An \code{xts} matrix of period returns with one
#'     column per portfolio plus a \code{"Market"} column.}
#'   \item{\code{cumRet}}{An \code{xts} matrix of cumulative returns.}
#'   \item{\code{plot}}{The plot object returned by \code{backtest.plot()}.}
#' }
#'
#' @importFrom PortfolioAnalytics optimize.portfolio.rebalancing extractWeights backtest.plot
#' @importFrom PerformanceAnalytics Return.rebalancing
#' @importFrom xts merge.xts
#' @importFrom stats complete.cases
#' @export
#' 
#' @examples
#' args(runPortfolioBacktest)
runPortfolioBacktest <- function(
    return_portfolio,
    portfolio_list,
    portfolio_names,
    market_return = NULL,
    rebalance_on,
    rolling_window,
    optimize_method = "CVXR",
    moment_list = NULL,
    ...,
    save_plot = TRUE,
    plot_path = "./",
    plot_name = "backtest",
    colorSet = NULL,
    ltySet = NULL
) {
  # Input validation
  if (!xts::is.xts(return_portfolio)) {
    stop("'return_portfolio' must be an xts object.")
  }
  if (!is.list(portfolio_list) || length(portfolio_list) == 0) {
    stop("'portfolio_list' must be a non-empty list of portfolio.spec objects.")
  }
  if (save_plot && is.null(plot_path)) {
    stop("Please provide 'plot_path' when 'save_plot = TRUE'.")
  }
  if (is.null(portfolio_names)) {
    portfolio_names <- names(portfolio_list)
  }
  if (length(portfolio_list) != length(portfolio_names)) {
    stop("'portfolio_list' and 'portfolio_names' must have the same length.")
  }
  if (!is.null(moment_list) && (length(portfolio_list) != length(moment_list))) {
    stop("'portfolio_list' and 'moment_list' must have the same length.")
  }
  
  # Optimize each portfolio and extract returns
  bt_returns <- list()
  
  if(is.null(moment_list)){
    moment_list <- vector("list", length(portfolio_list))
  }
  
  for(i in seq_along(portfolio_list)){
    ## Optimize portfolio
    if(!is.null(moment_list[[i]])){
      bt <- PortfolioAnalytics::optimize.portfolio.rebalancing(
        R = return_portfolio,
        portfolio = portfolio_list[[i]],
        optimize_method = optimize_method,
        rebalance_on = rebalance_on,
        rolling_window = rolling_window,
        momentFun = moment_list[[i]],
        ... = ...
      )
    } else {
      bt <- PortfolioAnalytics::optimize.portfolio.rebalancing(
        R = return_portfolio,
        portfolio = portfolio_list[[i]],
        optimize_method = optimize_method,
        rebalance_on = rebalance_on,
        rolling_window = rolling_window,
        ... = ...
      )
    }
    
    ## Extract weights
    wts <- PortfolioAnalytics::extractWeights(bt)
    wts <- wts[complete.cases(wts), ]
    
    ## Portfolio returns
    bt_ret <- PerformanceAnalytics::Return.rebalancing(return_portfolio, wts)
    
    ## Save into list
    bt_returns[[portfolio_names[i]]] <- bt_ret
  }
  
  # Merge portfolio returns with market benchmark
  if (!is.null(market_return)) {
    if (!xts::is.xts(market_return)) {
      stop("'market_return' must be an xts object when provided.")
    }
    ret_comb <- do.call(merge, c(bt_returns, list(market_return), all = FALSE))
    colnames(ret_comb) <- c(portfolio_names, "Market")
  } else {
    ret_comb <- do.call(merge, c(bt_returns, all = FALSE))
    colnames(ret_comb) <- portfolio_names
  }
  
  cum_comb <- cumprod(1 + ret_comb)
  
  # Plot
  p <- backtest.plot(
    ret_comb,
    colorSet = colorSet,
    ltySet = ltySet
  )
  
  # Save plot
  if(save_plot){
    png(paste0(plot_path, plot_name, ".png"), width = 800, height = 600)
    plot(p)
    dev.off()
  }
  
  # Return results
  return(list(
    returns = ret_comb,
    cumRet = cum_comb,
    plot = p
  ))
}
