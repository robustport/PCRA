#' @title SP500from1967to2007
#'
#' @description Year-end data on the S&P 500® Index from 1967 to 2007 
#' extracted from a paper copy of the S&P Analysts' Handbook.  
#' The title of the page from which this data was extracted says 
#' "Historical Index - S&P 500 Composite - 500 stocks". It includes some 
#' information (e.g. Cash Flow) that is no longer provided. An extensive dataset
#' for the S&P 500® and various other S&P® indices can be downloaded from
#' https://www.spglobal.com/spdji/en/documents/additional-material/sp-500-eps-est.xlsx. 
#' Final year-end numbers are typically reported in April or May of the following year.
#' 
#' @docType data
#'
#' @usage data(SP500from1967to2007)
#'
#' @format A data frame with observations on the S&P 500® index 
#' from 1967 to 2007
#' \itemize{
#'  \item \strong{Date:} type `Date`. End of year date formatted as YYYY-12-31. 
#'  Useful when creating a time series object for exploratory time series plots. 
#'  Convert the data frame to an xts object using xts::as.xts(SP500).
#'  \item \strong{Year:} type `num`. Year corresponding to Date.
#'  \item \strong{Sales:} type `num`. Revenues per share for the S&P 500® 
#'  for the calendar year.
#'  \item \strong{Cash_Flow:} type `num`. Cash Flow per share for the S&P 500® 
#'  for the calendar year.
#'  \item \strong{Diluted_EPS:} type `num`. Fully Diluted As-Reported Earnings 
#'  per share for the S&P 500® for the calendar year.
#'  \item \strong{Dividends_Per_Share:} type `num`. Dividends per share for the 
#'  S&P 500® for the calendar year.
#'  \item \strong{Dividends_Pct_of_Earnings:} type `num`. Ratio of Dividends per 
#'   share to Fully Diluted As-Reported Earnings per share for the S&P 500® 
#'   for the calendar year, expressed as a percentage.
#'  \item \strong{Price_High:} type `num`. Highest price level achieved by the 
#'  S&P 500® during the calendar year.
#'  \item \strong{Price_Low:} type `num`. Lowest price level achieved by the 
#'  S&P 500® during the calendar year.
#'  \item \strong{Price_Close:} type `num`. Year-end (12/31) price of the 
#'   S&P® Index.
#'  \item \strong{PE_Ratio_High:} type `num`. Ratio of Price_High to Diluted_EPS
#'   for the S&P 500®.
#'  \item \strong{PE_Ratio_Low:} type `num`. Ratio of Price_Low to Diluted_EPS
#'   for the S&P 500®.
#'  \item \strong{PE_Ratio_Close:} type `num`. Ratio of Price_Close to Diluted_EPS
#'   for the S&P 500®.
#'  \item \strong{Dividend_Yld_High:} type `num`. Ratio of Dividends_Per_Share to
#'   Price_High for the S&P 500®.
#'  \item \strong{Dividend_Yld_Low:} type `num`. Ratio of Dividends_Per_Share to
#'   Price_Low for the S&P 500®.
#'  \item \strong{Dividend_Yld_Close:} type `num`. Ratio of Dividends_Per_Share to
#'   Price_Close for the S&P 500®.
#'  \item \strong{Total_Return_Index:} type `num`. Cumulative total return of the
#'   S&P 500® including both dividends and price return. Start date for the series 
#'   (when it was likely normalized to 100) is not known.
#'  \item \strong{Book_Value_Per_Share:} type `num`. Year-end (12/31) Book Value 
#'  (or Shareholders' Equity) per share for the S&P 500®.
#'  \item \strong{Book_Value_Pct_Return:} type `num`. Definition currently unknown. 
#'  \item \strong{Price_to_Book_Ratio:} type `num`. Ratio of Price_Close to 
#'   Book_Value_Per_Share for the S&P 500®.
#' }
#' 
#' @references
#' Chapter 13 (Expected Returns) of Martin, Philips, Scherer, Stoyanov and Li, 
#' Portfolio Construction and Risk Analysis, Springer, 2024.
#' 
#' @details
#' Data for the S&P® Industrials is taken from a paper copy of the 
#' S&P® Analysts' Handbook published in 2008. It includes one variable (Cash Flow)
#' that is no longer provided, and excludes many others (Operating Earnings, 
#' Capital Expenditures, Earnings Estimates, Index Divisor, % of firms that have
#' beaten estimates, sector breakdowns, projected growth rates by sector, 
#' effective tax rate etc.) that are now provided by S&P® in the spreadsheet
#' https://www.spglobal.com/spdji/en/documents/additional-material/sp-500-eps-est.xlsx. 
#' Final year-end numbers are typically reported in April or May of the following year.
#' 
#' @source S&P Dow Jones Indices. 
#' S&P®, S&P 400 Industrials®, S&P 425 Industrials®, S&P Industrials®and S&P 500® 
#' are registered trademarks of Standard & Poor’s Financial Services LLC, 
#' and Dow Jones® is a registered trademark of Dow Jones Trademark Holdings LLC. 
#' © 2023 S&P Dow Jones Indices LLC, its affiliates and/or its licensors. 
#' All rights reserved. ' Redistribution of the data is not permitted, and use of the data in
#' derivative works is not permitted without the written permission of 
#' S&P Dow Jones Indices LLC.
#' 
#' @examples  
#' data(SP500from1967to2007)
#' names(SP500from1967to2007)
#' head(SP500from1967to2007, 5)
#' tail(SP500from1967to2007, 5)
"SP500from1967to2007"