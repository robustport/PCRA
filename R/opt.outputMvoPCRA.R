#' @title Optimal Portfolio Weights and Performance
#' 
#' @description Converts output of PortfolioAnalytics function
#' optimize.portfolio, which computes a minimum variance portfolio,
#' to a list containing the portfolio weights vector, mean, volatility
#' and Sharpe Ratio.
#' 
#' @title Optimal Portfolio Weights and Performance Values
#' 
#' @description Converts output of `optimize.portfolio` to a list of the
#' portfolio weights, mean, volatility and Sharpe Ratio.
#' 
#' @param opt List output of `optimize.portfolio` 
#' @param returns Multivariate xts object of portfolio assets returns
#' @param digits Integer number of significant digits with default NULL
#' @param itemNames character vector of use-supplied names for portfolio
#' weights, mean, standard deviation and Sharpe Ratio
#' @param annualize Logical with default TRUE
#' @param frequency Returns frequency: "monthly", "weekly" or "daily", with 
#' default "monthly"
#' @param rf Numeric value of risk-free rate with default 0.0
#' 
#' @details This function uses the weights returned by optimize.portfolio,
#' along with the portfolio monthly, weekly or daily assets returns, and a
#' risk-free rate, to compute the portfolio mean return, volatility, and
#' Sharpe Ratio.  By default the latter three are annualized, but the user
#' may choose to return non-annualized performance values.
#'
#' @return A list containing the portfolio numeric weights, mean value,
#' standard deviation and Sharpe Ratio, with default names Wgts, Mean, 
#' StdDev, and SR, or user-supplied names as a character vector value for
#' the argument `itemNames`.
#' 
#' @author R. Douglas Martin
#' @export
#'
#' @examples
#' args(opt.outputMvoPCRA)
opt.outputMvoPCRA <- function(opt, returns, digits = NULL, itemNames = NULL, 
                           annualize = TRUE, frequency = "monthly", rf = 0.0) 
{
  if(class(returns)[1] == "xts"){
    returns <- coredata(returns)
  }
  Wgts <- opt$weights
  sigmasq <- as.numeric(t(Wgts) %*% var(returns) %*% Wgts)
  StdDev <- sqrt(sigmasq)
  mu.ret <- apply(returns, 2, mean)
  Mean <- as.numeric(t(Wgts) %*% mu.ret)
  SR <- (Mean - rf)/StdDev
  a <- 1; b <- 1
  if(annualize){
    if(frequency == "monthly"){
      a <- 12; b <- sqrt(12)
    } else if(frequency == "weekly"){
      a <- 52; b <- sqrt(52)
    } else {
      a <- 260; b <- sqrt(260)
    }
  }      
  Mean <- a*Mean
  StdDev <- b*StdDev
  SR <- b*SR
  
  output <- list(Wgts = Wgts, Mean = Mean, StdDev = StdDev, SR = SR)
  if(!is.null(itemNames)){
    names(output) <- itemNames
  }  
  if (!is.null(digits)) {
    output <- lapply(output, round, digits)
  }
  output
}