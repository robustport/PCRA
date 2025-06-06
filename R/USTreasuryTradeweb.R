#' @title USTreasuryTradeweb
#'
#' @description A data frame with end-of-day quotes from Tradeweb Markets LLC of 
#' bid and ask prices and yields on the On-the-Run and First-Off-the-Run 
#' US Treasury Notes and Bonds at 4 maturities: 2 Years, 5 Years, 10 Years and 30 Years.
#' 
#' @docType data
#'
#' @usage data(USTreasuryTradeweb)
#
#' @format A data frame with end-of-day quotes from Tradeweb Markets LLC of 
#' bid and ask prices and yields on the On-the-Run and First-Off-the-Run 
#' US Treasury Notes and Bonds at 4 maturities: 2 Years, 5 Years, 10 Years and 30 Years.
#' Prices are reported to the nearest 1/256 (0.00390625), while yields are 
#' reported to 3 decimal places. Yields are percentages, i.e. a yield of 2.125 
#' should be interpreted as 2.125% or 0.02124.
#' 
#' \itemize{
#'  \item \strong{Date:} type `Date`. Trade Date. Not all bonds have data on all days.
#'  \item \strong{US2Y_OTR_BidPrice:} type `num`. Closing Bid Price for the 
#'  2 Year On-the-Run bond.
#'  \item \strong{US2Y_OTR_AskPrice:} type `num`. Closing Ask Price for the 
#'  2 Year On-the-Run bond.
#'  \item \strong{US2Y_OTR_BidYield:} type `num`. Closing Bid Yield to Maturity 
#'  for the 2 Year On-the-Run bond.
#'  \item \strong{US2Y_OTR_AskYield:} type `num`. Closing Ask Yield to Maturity 
#'  for the 2 Year On-the-Run bond.
#'  \item \strong{US2Y_OFTR_1_BidPrice:} type `num`. Closing Bid Price for the 
#'  2 Year First-Off-the-Run bond.
#'  \item \strong{US2Y_OFTR_1_AskPrice:} type `num`. Closing Ask Price for the 
#'  2 Year First-Off-the-Run bond.
#'  \item \strong{US2Y_OFTR_1_BidYield:} type `num`. Closing Bid Yield to Maturity 
#'  for the 2 Year First-Off-the-Run bond.
#'  \item \strong{US2Y_OFTR_1_AskYield:} type `num`. Closing Ask Yield to Maturity 
#'  for the 2 Year First-Off-the-Run bond.
#'  \item \strong{US5Y_OTR_BidPrice:} type `num`. Closing Bid Price for the 
#'  5 Year On-the-Run bond.
#'  \item \strong{US5Y_OTR_AskPrice:} type `num`. Closing Ask Price for the 
#'  5 Year On-the-Run bond.
#'  \item \strong{US5Y_OTR_BidYield:} type `num`. Closing Bid Yield to Maturity 
#'  for the 5 Year On-the-Run bond.
#'  \item \strong{US5Y_OTR_AskYield:} type `num`. Closing Ask Yield to Maturity 
#'  for the 5 Year On-the-Run bond.
#'  \item \strong{US5Y_OFTR_1_BidPrice:} type `num`. Closing Bid Price for the 
#'  5 Year First-Off-the-Run bond.
#'  \item \strong{US5Y_OFTR_1_AskPrice:} type `num`. Closing Ask Price for the 
#'  5 Year First-Off-the-Run bond.
#'  \item \strong{US5Y_OFTR_1_BidYield:} type `num`. Closing Bid Yield to Maturity 
#'  for the 5 Year First-Off-the-Run bond.
#'  \item \strong{US5Y_OFTR_1_AskYield:} type `num`. Closing Ask Yield to Maturity 
#'  for the 5 Year First-Off-the-Run bond.
#'  \item \strong{US10Y_OTR_BidPrice:} type `num`. Closing Bid Price for the 
#'  10 Year On-the-Run bond.
#'  \item \strong{US10Y_OTR_AskPrice:} type `num`. Closing Ask Price for the 
#'  10 Year On-the-Run bond.
#'  \item \strong{US10Y_OTR_BidYield:} type `num`. Closing Bid Yield to Maturity 
#'  for the 10 Year On-the-Run bond.
#'  \item \strong{US10Y_OTR_AskYield:} type `num`. Closing Ask Yield to Maturity 
#'  for the 10 Year On-the-Run bond.
#'  \item \strong{US10Y_OFTR_1_BidPrice:} type `num`. Closing Bid Price for the 
#'  10 Year First-Off-the-Run bond.
#'  \item \strong{US10Y_OFTR_1_AskPrice:} type `num`. Closing Ask Price for the 
#'  10 Year First-Off-the-Run bond.
#'  \item \strong{US10Y_OFTR_1_BidYield:} type `num`. Closing Bid Yield to Maturity 
#'  for the 10 Year First-Off-the-Run bond.
#'  \item \strong{US10Y_OFTR_1_AskYield:} type `num`. Closing Ask Yield to Maturity 
#'  for the 10 Year First-Off-the-Run bond.
#'  \item \strong{US30Y_OTR_BidPrice:} type `num`. Closing Bid Price for the 
#'  30 Year On-the-Run bond.
#'  \item \strong{US30Y_OTR_AskPrice:} type `num`. Closing Ask Price for the 
#'  30 Year On-the-Run bond.
#'  \item \strong{US30Y_OTR_BidYield:} type `num`. Closing Bid Yield to Maturity 
#'  for the 30 Year On-the-Run bond.
#'  \item \strong{US30Y_OTR_AskYield:} type `num`. Closing Ask Yield to Maturity 
#'  for the 30 Year On-the-Run bond.
#'  \item \strong{US30Y_OFTR_1_BidPrice:} type `num`. Closing Bid Price for the 
#'  30 Year First-Off-the-Run bond.
#'  \item \strong{US30Y_OFTR_1_AskPrice:} type `num`. Closing Ask Price for the 
#'  30 Year First-Off-the-Run bond.
#'  \item \strong{US30Y_OFTR_1_BidYield:} type `num`. Closing Bid Yield to Maturity 
#'  for the 30 Year First-Off-the-Run bond.
#'  \item \strong{US30Y_OFTR_1_AskYield:} type `num`. Closing Ask Yield to Maturity 
#'  for the 30 Year First-Off-the-Run bond.
#' }
#' 
#' @references
#' Chapter 14 (Fixed Income Portfolio Management) of Martin, Philips, Scherer, 
#' Stoyanov and Li, Portfolio Construction and Risk Analysis, Springer, 2024.
#' 
#' @details This data set with the bid and ask prices and yields of 2, 5, 10 and 30-year
#' U.S. Treasuries is supplied by Tradeweb Markets LLC. All prices and yields are 
#' reported at the end of the trading day. Prices are reported to the nearest 1/256 
#' (0.00390625), while yields are reported to 3 decimal places.
#' 
#' @source Tradeweb OTR-OFTR Spread Data is provided by Tradeweb Markets LLC for 
#' educational purposes only. Tradeweb Markets LLC makes no representation or warranty 
#' of any kind, express or implied, including regarding the accuracy, adequacy, validity, 
#' reliability, availability or completeness of the Tradeweb OTR-OFTR Spread Data. 
#' All rights reserved. 
#' 
#' @examples  
#' data(USTreasuryTradeweb)
#' names(USTreasuryTradeweb)
#' head(USTreasuryTradeweb, 5)
#' tail(USTreasuryTradeweb, 5)
"USTreasuryTradeweb"