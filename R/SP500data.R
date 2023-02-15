#' SP500data
#'
#' Year-end data on the S&P 500, Nominal GDP and Consumer Prices from 
#' 1925 to the most recent year-end for which final data is available.
#' 
#' @docType data
#'
#' @usage data(SP500data)
#'
#' @format A data.frame with observations on the S&P500 from 1925 
#' to the most recent year end for which final data is available:
#' \itemize{
#'  \item \strong{Year:} type `num`.
#'  \item \strong{SP500Price:} type `num`. Year-end (12/31) price of the S&P 500
#'  \item \strong{SP500EpsAll4Q:} type `num`. As-Reported Earnings per share
#'  for the S&P 500 for the entire calendar year.
#'  \item \strong{SP500EpsBest3Q:} type `num`. 4/3 x Sum of the three highest
#'  quarterly earnings per share for the S&P 500 during the calendar year.
#'  \item \strong{SP500EpsBest2Q:} type `num`. 2 x Sum of the two highest
#'  quarterly earnings per share for the S&P 500 during the calendar year.
#'  \item \strong{SP500EpsBest1Q:} type `num`. 4 x the highest earnings per share
#'  in a quarter for the S&P 500 during the calendar year.
#'  \item \strong{SP500Revenue:} type `num`. Annual Revenues per share 
#'  for the S&P 500 during the calendar year.
#'  \item \strong{SP500DPS:} type `num`. Annual Dividends per share 
#'  for the S&P 500 during the calendar year.
#'  \item \strong{SP500OperatingEPS:} type `num`. Operating Earnings 
#'  per share for the S&P 500 for the calendar year.
#'  \item \strong{SP500Nom1YrFwdRet:} type `num`. Nominal total return including
#'  both change in price and dividends and not adjusted for inflation for the 
#'  S&P 500 for the FOLLOWING calendar year.
#'  \item \strong{CPIAUCNS:} type `num`. Consumer Price Index for All Urban 
#'  Consumers: All Items in U.S. City Average, as of year end. 
#'  \item \strong{GDPA:} type `num`. Nominal GDP at an annual frequency.
#'  Reserve's FRED database at https://fred.stlouisfed.org/series/CPIAUCNS
#' }
#' 
#' @references
#' Chapter 13 (Expected Returns) of Martin, Philips, Scherer, Stoyanov and Li, 
#' Portfolio Contstruction and Risk Analysis, Springer, 2024.
#' 
#' @details
#' CPIAUCNS is obtained from the Federal Reserve's FRED database at 
#' https://fred.stlouisfed.org/series/CPIAUCNS
#' GDPA is obtained from the Federal Reserve's FRED database at 
#' https://fred.stlouisfed.org/series/CPIAUCNS
#' Data for the S&P 500 is updated using the QUARTERLY DATA tab of
#' https://www.spglobal.com/spdji/en/documents/additional-material/sp-500-eps-est.xlsx
#' Final year-end numbers are typically reported in April or May of the following year.
#' @source S&P Dow Jones Indices. S&P® and S&P 500® are registered trademarks of 
#' Standard & Poor’s Financial Services LLC, and Dow Jones® is a registered 
#' trademark of Dow Jones Trademark Holdings LLC. © 2022 S&P Dow Jones Indices LLC, 
#' its affiliates and/or its licensors. All rights reserved.
#' Redistribution of the data is not permitted, and use of the data in
#' derivative works is not permitted without the written permission of 
#' S&P Dow Jones Indices LLC.
#' 
#' @examples  
#' data(SP500data)
#' names(SP500data)
#' unique(SP500data$Sector)
#' unique(SP500data$CapGroup)
#' head(SP500data,2)
"SP500data"