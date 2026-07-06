#' Build a List of Portfolio Specifications (Default Example)
#'
#' @description
#' This function serves as the default example for the \code{buildPortfolios}
#' argument in \code{\link{runMultipleBacktests}}. It demonstrates how to
#' construct a list of portfolio specifications using
#' \code{\link[PortfolioAnalytics]{portfolio.spec}},
#' \code{\link[PortfolioAnalytics]{add.constraint}}, and
#' \code{\link[PortfolioAnalytics]{add.objective}} from the
#' \code{PortfolioAnalytics} package.
#'
#' Users are encouraged to write their own portfolio list following the same 
#' structure of this function: one input (\code{selected_stocks}), one output (a
#' named list of \code{portfolio.spec} objects), and pass it to
#' \code{runMultipleBacktests()} via the \code{buildPortfolios} argument.
#'
#' @param selected_stocks Character vector. Tickers of the assets to include
#'   in the portfolio.
#'
#' @return A list of \code{portfolio.spec} objects, one per strategy. It is
#'   strongly recommended to name each element of the list, as the names are
#'   used as labels across all outputs.
#'
#' @seealso
#' \code{\link{runMultipleBacktests}},
#' \code{\link[PortfolioAnalytics]{portfolio.spec}},
#' \code{\link[PortfolioAnalytics]{add.constraint}},
#' \code{\link[PortfolioAnalytics]{add.objective}}
#' 
#' @importFrom PortfolioAnalytics portfolio.spec add.constraint add.objective
#' @export
#'
#' @examples
#' body(buildPortfolios)
buildPortfolios <- function(selected_stocks){
  pspec <- portfolio.spec(assets = selected_stocks)
  pspec <- add.constraint(pspec, type = "full_investment")
  pspec <- add.constraint(pspec, type = "long_only")
  
  pspec_list <- list(
    pspec_GMV  <- add.objective(pspec, type = "risk", name = "var"),
    pspec_GMES <- add.objective(pspec, type = "risk", name = "ES"),
    pspec_GMEQS <- add.objective(pspec, type = "risk", name = "EQS")
  )
  
  names(pspec_list) <- c("GMV", "GMES", "GMEQS")
  
  return(pspec_list)
}