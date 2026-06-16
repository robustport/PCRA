#' Run Multiple Portfolio Backtests and Plot
#'
#' @description
#' Runs \code{n_simulations} independent portfolio backtests, each time drawing
#' a random subset of stocks from \code{stock_list}. For every simulation a full
#' backtest is executed via \code{runPortfolioBacktest()}, and an individual plot
#' can optionally be saved. After all simulations finish, the function
#' averages the cumulative return series across simulations and can save a
#' summary plot of those averaged returns.
#'
#' @param n_simulations Integer. Number of independent backtest simulations to run.
#' @param portfolio_size Integer. Number of stocks to randomly sample from
#'   \code{stock_list} for each simulation.
#' @param seed Integer. Random seed passed to \code{set.seed()} before the
#'   simulation loop for reproducibility.
#' @param return_portfolio An \code{xts} matrix of asset returns. Column names must
#'   match the assets in \code{stock_list}.
#' @param stock_list Character vector. Universe of stock tickers from which
#'   \code{portfolio_size} tickers are drawn at random in each simulation.
#' @param market_return An \code{xts} single-column object of benchmark returns.
#' @param portfolio_list A named list of \code{portfolio.spec} objects built
#'   with \code{PortfolioAnalytics}. The names of the list elements are used 
#'   as strategy labels in plot legends and all other outputs.
#' @param rebalance_on Character string passed to \code{optimize.portfolio.rebalancing()}. 
#'   See \code{\link[xts]{endpoints}} for valid names.
#' @param rolling_window Positive integer. Length of the rolling estimation
#'   window in periods.
#' @param optimize_method Character string specifying the solver. Default \code{"CVXR"}.
#' @param save_plot Logical. Whether to save the plot to a PNG file. Default \code{TRUE}.
#' @param plot_path Character string. Full file path for the each simulation output.
#'   Required when \code{save_plot = TRUE}.
#' @param plot_name Plot name for each simulation output. Default \code{"backtest"}
#' @param save_avg_plot Logical. Whether to save the the average cumulative 
#'   returns plot to a PNG file. Default \code{TRUE}.
#' @param avg_plot_path Character string. Full file path for the average simulation output.
#'   Required when \code{save_avg_plot = TRUE}.
#' @param avg_plot_name Plot name for average simulation output. Default \code{"avg_backtest"}.
#' @param colorSet Optional character vector of colors passed to \code{backtest.plot()}.
#' @param ltySet Optional integer vector of line types passed to \code{backtest.plot()}.
#' 
#'
#' @return A named list with four elements:
#' \describe{
#'   \item{\code{results}}{A list of length \code{n_simulations}. Each element
#'     contains the \code{returns} component returned by
#'     \code{runPortfolioBacktest()} for that simulation.}
#'   \item{\code{selected_stocks_all}}{A list of length \code{n_simulations}.
#'     Each element is a character vector of the tickers chosen for that
#'     simulation.}
#'   \item{\code{avg_algorithm_returns}}{An \code{xts} object containing the
#'     element-wise average of all simulations' algorithm return series.}
#'   \item{\code{avg_cumulative_return}}{An \code{xts} object containing the
#'     element-wise average of all simulations' cumulative return series.}
#' }
#' 
#' @importFrom PortfolioAnalytics backtest.plot
#' @export
#'
#' @examples
#' args(runMultipleBacktests)
runMultipleBacktests <- function(
    n_simulations,
    portfolio_size,
    seed = NULL,
    return_portfolio,
    stock_list,
    portfolio_list,
    market_return = NULL,
    rebalance_on,
    rolling_window,
    optimize_method = "CVXR",
    ...,
    save_plot = TRUE,
    plot_path = "./",
    plot_name = "backtest",
    save_avg_plot = TRUE,
    avg_plot_path = "./",
    avg_plot_name = "avg_backtest",
    colorSet = NULL,
    ltySet = NULL
){
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
  if (save_avg_plot && is.null(avg_plot_path)) {
    stop("Please provide 'avg_plot_path' when 'save_avg_plot = TRUE'.")
  }
  
  if(!is.null(seed)) set.seed(seed)
  results <- list()
  selectStockAll <- list()
  cum_list <- list()
  
  for(k in 1:n_simulations){
    # Random Stock Selection
    selectStock <- sample(stock_list, portfolio_size)
    selectStockAll[[k]] <- selectStock
    
    # Return Matrix
    retPort <- return_portfolio[, colnames(return_portfolio) %in% selectStock]
    
    # Run Backtest
    bt_result <- runPortfolioBacktest(
      retPort = retPort,
      portfolio_list = portfolio_list,
      portfolio_names = names(portfolio_list),
      market_ret = market_return,
      rebalance_on = rebalance_on,
      rolling_window = rolling_window,
      ... = ...,
      save_plot = save_plot,
      plot_path = plot_path,
      plot_name = paste(plot_name, k),
      colorSet = colorSet,
      ltySet = ltySet
    )
    results[[k]] <- bt_result$returns
    
    # Convert to cumulative returns
    cum_list[[k]] <- bt_result$cumRet
  }
  
  # Average cumulative return
  cum_avg <- Reduce("+", cum_list) / n_simulations
  ret_avg <- cum_avg / lag(cum_avg, 1) - 1
  ret_avg <- na.omit(ret_avg)
  
  if(save_avg_plot){
    p <- backtest.plot(
      ret_avg,
      colorSet = colorSet,
      ltySet = ltySet
    )
    png(paste0(avg_plot_path, avg_plot_name, ".png"), width = 800, height = 600)
    plot(p)
    dev.off()
  }
  
  # Return
  return(list(
    results = results,
    selected_stocks_all = selectStockAll,
    avg_algorithm_returns = ret_avg,
    avg_cumulative_return = cum_avg
  ))
}