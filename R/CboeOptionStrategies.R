#' @title CboeOptionStrategies
#'
#' @description Monthly time series of total return indices from June 1986 to 
#' December 2021 for options based strategies created and maintained by Cboe 
#' Livevol, LLC along with data for the S&P 500®, as well as levels of the 
#' VIX and VXO volatility measures, and the 3-month T-Bill rate (GS3M) from the 
#' Federal Reserve Bank of St. Louis' FRED database.
#' 
#' @docType data
#'
#' @usage data(CboeOptionStrategies)
#
#' @format A data frame with monthly time series of ten total return indices for 
#' options based strategies created and maintained by Cboe Livevol, LLC 
#' along with total return and price return indices for the S&P 500®, as well as 
#' the levels of the VIX and VXO volatility measures, and the 3-month T-Bill 
#' rate (GS3M) from the Federal Reserve Bank of St. Louis' FRED database. 
#' Links are provided to the relevant websites for each of the series. Many, but
#' not all, of the total return series start with a value of 100.
#' 
#' \itemize{
#'  \item \strong{Date:} type `Date`. Last Day of Month. Many, but not all, of
#'  the time series have data from June 1986 to December 2021.
#'  \item \strong{BXM:} type `num`. Month-end closing level for the cumulative 
#'  total return index for the Cboe S&P 500 BuyWrite Index. Details of its 
#'  construction can be found at https://www.cboe.com/us/indices/dashboard/BXM/.
#'  \item \strong{BXMD:} type `num`. Month-end closing level for the cumulative 
#'  total return index for the Cboe S&P 500 30-Delta BuyWrite Index series. Details 
#'  of its construction can be found at https://www.cboe.com/us/indices/dashboard/BXMD/.
#'  \item \strong{BXY:} type `num`. Month-end closing level for the cumulative 
#'  total return index for the Cboe S&P 500 2% OTM BuyWrite Index. Details of its 
#'  construction can be found at https://www.cboe.com/us/indices/dashboard/BXY/.
#'  \item \strong{PUT:} type `num`. Month-end closing level for the cumulative 
#'  total return index for the Cboe S&P 500 PutWrite Index. Details of its 
#'  construction can be found at https://www.cboe.com/us/indices/dashboard/PUT/.
#'  \item \strong{CLL:} type `num`. Month-end closing level for the cumulative 
#'  total return index for the Cboe S&P 500 95-110 Collar Index. Details of its 
#'  construction can be found at https://www.cboe.com/us/indices/dashboard/CLL/.
#'  \item \strong{BFLY:} type `num`. Month-end closing level for the cumulative 
#'  total return index for the Cboe S&P 500 Iron Butterfly Index. Details of its 
#'  construction can be found at https://www.cboe.com/us/indices/dashboard/BFLY/.
#'  \item \strong{CLLZ:} type `num`. Month-end closing level for the cumulative 
#'  total return index for the Cboe S&P 500 Zero-Cost Put Spread Collar. Details of its 
#'  construction can be found at https://www.cboe.com/us/indices/dashboard/CLLZ/.
#'  \item \strong{CMBO:} type `num`. Month-end closing level for the cumulative 
#'  total return index for the Cboe S&P 500 Covered Combo Index. Details of its 
#'  construction can be found at https://www.cboe.com/us/indices/dashboard/CMBO/.
#'  \item \strong{CNDR:} type `num`. Month-end closing level for the cumulative 
#'  total return index for the Cboe S&P 500 Iron Condor Index. Details of its 
#'  construction can be found at https://www.cboe.com/us/indices/dashboard/CNDR/.
#'  \item \strong{PPUT:} type `num`. Month-end closing level for the cumulative 
#'  total return index for the Cboe S&P 500 5% Put Protection Index. Details of its 
#'  construction can be found at https://www.cboe.com/us/indices/dashboard/PPUT/.
#'  \item \strong{SPTR:} type `num`. Month-end closing level for the cumulative 
#'  total return index for the S&P 500® Index (SPX℠). Details of its construction 
#'  can be found at https://www.spglobal.com/spdji/en/indices/equity/sp-500/.
#'  \item \strong{SPX:} type `num`. Month-end closing level for the cumulative 
#'  price return index for the S&P 500® Index (SPX℠). Details of its construction 
#'  can be found at https://www.spglobal.com/spdji/en/indices/equity/sp-500/.
#'  \item \strong{VIX:} type `num`. Month-end closing level for the Cboe Volatilty 
#'  Index®, a measure of constant, 30-day expected volatility of the U.S. stock market 
#'  derived from real-time, mid-quote prices of S&P 500® Index (SPX℠). Details of 
#'  its construction can be found at https://www.cboe.com/tradable_products/vix/.
#'  \item \strong{VXO:} type `num`. Month-end closing level for the Cboe S&P 100
#'  Volatility Index. The index was discontinued on 9/23/2021. Historical daily
#'  data can be downloaded from https://fred.stlouisfed.org/series/VXOCLS.
#'  \item \strong{GS3M:} type `num`. Average daily closing 3 month constant
#'  maturity T-Bill yield, averaged over all business days in a month. Details 
#'  of its construction can be found at https://fred.stlouisfed.org/series/GS3M.
#' }
#' 
#' @references
#' Chapter 12 (Performance Analysis) of Martin, Philips, Scherer, 
#' Stoyanov and Li, Portfolio Construction and Risk Analysis, Springer, 2024.
#' 
#' @details This data set provides monthly time series of ten total return indices 
#' for options based strategies created and maintained by Cboe Livevol, LLC, 
#' along with total return and price return indices for the S&P 500®, as well as 
#' the levels of the VIX and VXO volatility measures, and the 3-month T-Bill rate 
#' (GS3M) from the Federal Reserve Bank of St. Louis' FRED database. Links are 
#' provided to the relevant websites for each of the series.  Many, but
#' not all, of the total return series start with a value of 100, so that their 
#' total return in any given month is the ratio of the value for that month to the
#' value for the prior month -1.
#' 
#' @source CBOE LIVEVOL, LLC. CBOE LIVEVOL DATA IS PROVIDED "AS IS" WITHOUT 
#' WARRANTY OF ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING, WITHOUT LIMITATION, 
#' ANY WARRANTY WITH RESPECT TO ACCURACY, COMPLETENESS, TIMELINESS, NONINFRINGEMENT, 
#' MERCHANTABILITY, OR FITNESS FOR A PARTICULAR PURPOSE. NEITHER LIVEVOL, NOR ANY 
#' PROVIDER OF DATA TO LIVEVOL, NOR ANY OF THEIR RESPECTIVE AFFILIATES, NOR THEIR 
#' RESPECTIVE DIRECTORS, OFFICERS, EMPLOYEES, CONTRACTORS, AND AGENTS SHALL HAVE 
#' ANY LIABILITY OF ANY KIND (INCLUDING, BUT NOT LIMITED TO, FOR ANY DIRECT, 
#' INDIRECT, INCIDENTAL, SPECIAL, CONSEQUENTIAL, OR PUNITIVE DAMAGES OR ANY DAMAGES 
#' FOR LOST PROFITS OR LOST OPPORTUNITIES AND WHETHER BASED UPON CONTRACT, TORT, 
#' WARRANTY, OR OTHERWISE) FOR ANY INACCURACIES, OMISSIONS, HUMAN OR MACHINE ERRORS, 
#' OR OTHER IRREGULARITIES IN THE DATA OR FOR ANY CESSATION, DISCONTINUANCE, 
#' FAILURE, MALFUNCTION, DELAY, SUSPENSION, INTERRUPTION, OR TERMINATION OF, OR 
#' WITH RESPECT TO, THE PROVISION OF THE DATA TO SUBSCRIBER. THE DATA IS NOT, AND 
#' SHOULD NOT BE CONSTRUED AS FINANCIAL, LEGAL OR OTHER ADVICE OF ANY KIND, NOR 
#' SHOULD IT BE REGARDED AS AN OFFER OR AS A SOLICITATION OF AN OFFER TO BUY, 
#' SELL OR OTHERWISE DEAL IN ANY INVESTMENT. ALL RIGHTS RESERVED. 
#' REDISTRIBUTION OF THE DATA IS NOT PERMITTED, AND USE OF THE DATA IN DERIVATIVE
#' WORKS IS NOT PERMITTED WITHOUT THE WRITTEN PERMISSION OF CBOE LIVEVOL, LLC.
#' 
#' SPTR℠ and SPX℠ are provided by S&P Dow Jones Indices. S&P® and S&P 500® 
#' are registered trademarks of Standard & Poor’s Financial Services LLC, 
#' and Dow Jones® is a registered trademark of Dow Jones Trademark Holdings LLC. 
#' © 2023 S&P Dow Jones Indices LLC, its affiliates and/or its licensors. 
#' All rights reserved. Redistribution of the data is not permitted, and use of 
#' the data in derivative works is not permitted without the written permission of 
#' S&P Dow Jones Indices LLC.
#' 
#' GS3M is obtained from the Federal Reserve Bank of St. Louis' FRED database at 
#' https://fred.stlouisfed.org/series/GS3M
#' 
#' @examples  
#' data(CboeOptionStrategies)
#' names(CboeOptionStrategies)
#' head(CboeOptionStrategies, 5)
#' tail(CboeOptionStrategies, 5)
"CboeOptionStrategies"