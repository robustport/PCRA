#' wtsGmvLS
#' 
#' A multivariate time series of global minimum variance (GMV) Long-Short 
#' portfolio weights based on  monthly returns of 20 smallcap stocks from
#' December 2001 through December 2010, using a 5-year training window with
#' initial position 1997 to 2001, with monthly rebalancing
#' 
#' @docType data
#'
#' @usage data(wtsGmvLS)
#' 
#' @format A multivariate xts object with 109 rows and 20 columns for 20 stocks
#' \itemize{
#'   \item \strong{w:} type `num`. Row vectors of weights, which sum to 1
#' }
#' 
#' @source The stocksCRSP data set
#' 
#' @examples  
#' data(wtsGmvLS)
#' round(head(wtsGmvLS,1),3)
#' round(tail(wtsGmvLS,1),3)
"wtsGmvLS"