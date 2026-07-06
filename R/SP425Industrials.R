#' @title SP425Industrials
#'
#' @description Year-end data on the S&P 425 Industrials® Index from 1946 to 1966 
#' extracted from a paper copy of the S&P Analysts' Handbook.
#' 
#' @docType data
#'
#' @usage data(SP425Industrials)
#'
#' @format A data frame with observations on the S&P 425 Industrials® index 
#' from 1946 to 1966
#' \itemize{
#'  \item \strong{Date:} type `Date`. End of year date formatted as YYYY-12-31. 
#'  Useful when creating a time series object for exploratory time series plots. 
#'  Convert the data frame to an xts object using xts::as.xts(SP500).
#'  \item \strong{Year:} type `num`. Year corresponding to Date.
#'  \item \strong{Sales:} type `num`. Revenues per share for the S&P 425 Industrials 
#'  for the calendar year.
#'  \item \strong{Operating_Profit:} type `num`. Operating Income per share 
#'  for the S&P 425 Industrials for the calendar year.
#'  \item \strong{Profit_Margin_Pct:} type `num`. Ratio of Operating_Profit to 
#'  Sales for the S&P 425 Industrials expressed as a percentage.
#'  \item \strong{Depreciation:} type `num`. Depreciation expense per share for 
#'  the S&P 425 Industrials for the calendar year.
#'  \item \strong{Federal_Income_Taxes:} type `num`. Federl Tax expense per share for 
#'  the S&P 425 Industrials for the calendar year.
#'  \item \strong{Earnings_Per_Share:} type `num`. Fully Diluted As-Reported Earnings 
#'  per share for the S&P 425 Industrials for the calendar year.
#'  \item \strong{Earnings_Pct_of_Sales:} type `num`. Ratio of Diluted_EPS to 
#'  Sales for the S&P 425 Industrials from 1993 to 2007 expressed as a percentage.
#'  Definition currently unknown for earlier years.
#'  \item \strong{Dividends_Per_Share:} type `num`. Dividends per share for the 
#'  S&P 425 Industrials for the calendar year.
#'  \item \strong{Dividends_Pct_of_Earnings:} type `num`. Ratio of Dividends_Per_Share
#'   to Diluted_EPS for the S&P 425 Industrials, expressed as a percentage.
#'  \item \strong{Price_High:} type `num`. Highest price level achieved by the 
#'  S&P 425 Industrials Index during the calendar year.
#'  \item \strong{Price_Low:} type `num`. Lowest price level achieved by the 
#'  S&P 425 Industrials Index during the calendar year.
#'  \item \strong{PE_Ratio_High:} type `num`. Ratio of Price_High to Diluted_EPS
#'   for the S&P 425 Industrials Index.
#'  \item \strong{PE_Ratio_Low:} type `num`. Ratio of Price_Low to Diluted_EPS
#'   for the S&P 425 Industrials Index.
#'  \item \strong{Dividend_Yld_High:} type `num`. Ratio of Dividends_Per_Share to
#'   Price_High for the S&P 425 Industrials Index.
#'  \item \strong{Dividend_Yld_Low:} type `num`. Ratio of Dividends_Per_Share to
#'   Price_Low for the S&P 425 Industrials Index.
#'  \item \strong{Book_Value_Per_Share:} type `num`. Year-end (12/31) Book Value 
#'  (or Shareholders' Equity) per share for the S&P 425 Industrials Index.
#'  \item \strong{Book_Value_Pct_Return:} type `num`. Definition Unknown. 
#'  \item \strong{Working_Capital:} type `num`. Definition Unknown.
#'  \item \strong{Capital_Expenditures:} type `num`. Capital Expenditures
#'  per share for the S&P 425 Industrials for the calendar year.
#' }
#' 
#' @references
#' Chapter 13 (Expected Returns) of Martin, Philips, Scherer, Stoyanov and Li, 
#' Portfolio Construction and Risk Analysis, Springer, 2024.
#' 
#' @details
#' Data for the S&P® 425 Industrials is taken from a paper copy of the 
#' S&P® Analysts' Handbook published in 1967. The average price level of the
#' index in 1941-1943 was set to 10. The index is based on 70 individual groups,
#' and price information on it was backfilled to 1918, though we do not have 
#' access to it. The original S&P® 500 index was created in late February 1957 
#' and included 425 industrial stocks, 15 rail stocks and 60 utility stocks. It 
#' maintained this composition until July 1976 when finance stocks were added 
#' to the index. See https://globalfinancialdata.com/the-sp-composite-before-1957
#' for a useful history of the various S&P® indices.
#' 
#' @source S&P Dow Jones Indices. 
#' S&P®, S&P 400 Industrials®, S&P 425 Industrials®, S&P Industrials®and S&P 500® 
#' are registered trademarks of Standard & Poor’s Financial Services LLC, 
#' and Dow Jones® is a registered trademark of Dow Jones Trademark Holdings LLC. 
#' © 2023 S&P Dow Jones Indices LLC, #' its affiliates and/or its licensors. 
#' All rights reserved.
#' Redistribution of the data is not permitted, and use of the data in
#' derivative works is not permitted without the written permission of 
#' S&P Dow Jones Indices LLC.
#' 
#' @examples  
#' data(SP425Industrials)
#' names(SP425Industrials)
#' head(SP425Industrials, 5)
#' tail(SP425Industrials, 5)
"SP425Industrials"