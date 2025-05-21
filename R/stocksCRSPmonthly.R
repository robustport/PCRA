#' @title CRSP monthly stocks data for 294 stocks
#'
#' @description CRSP monthly stocks data for 294 stocks for 1993 to 2015
#' 
#' @docType data
#'
#' @usage data(stocksCRSP)
#'
#' @format A data.table object with 82000 observations on 15 variables:
#' \itemize{
#'  \item \strong{Date:} type `Date`.
#'  \item \strong{TickerLast:} type `chr`. The ticker as of December 2015
#'  \item \strong{Ticker:} type `chr`. Monthly ticker
#'  period
#'  \item \strong{Company:} type `chr`. The name of company with TickerLast
#'  \item \strong{CapGroupLast:} type `chr`. Company market capitalization group
#'  as of December 2015, one of: MicroCap, SmallCap, MidCap or LargeCap
#'  \item \strong{CapGroup:} type `chr`. Monthly market capitalization group
#'  \item \strong{GICS:} type `chr`. 6 digit S&P GICS code
#'  \item \strong{Sector:} type `chr`. One of 10 sectors specified by the first
#'  two digits of the GICS code
#'  \item \strong{Return:} type `num`. Arithmetic stock return from one period
#'  to the next in decimal form
#'  \item \strong{RetExDiv:} type `num`.
#'  \item \strong{Price:} type `num`. Stock price at each time period in decimal
#'  form
#'  \item \strong{PrcSplitAdj:} type `num`.
#'  \item \strong{Ret4WkBill:} type `num`. Return of 4 week Treasury bill
#'  \item \strong{Ret13WkBill:} type `num`. Return of 13 week Treasury bill
#'  \item \strong{Ret1YrBill:} type `num`. Return of 1 year Treasury bill
#'  \item \strong{mktIndexCRSP:} type `num`. CRSP value weighted market return
#' }
#' 
#' @references
#' A standard corporate finance textbook:  Ross, Westerfield, Jaffe and
#' Jordan (2019). Corporate Finance, McGraw-Hill Education.
#' 
#' @details
#' The four CapGroupLast categorizations of the stocks were determined using the
#' three capitalization breakpoints $15.6B, $5.4B, $600M. Details concerning the
#' construction of the monthly CapGroup categorizations will eventually be
#' provided in a Vignette.
#' 
#' Weekly and daily versions stocksCRSPweekly and stocksCRSPdaily may be
#' obtained using the function getPCRAData() - see PCRAData.R. 
#' 
#' @source Center for Research in Security Prices (CRSP) at the University of
#' Chicago's Booth School of Business (CRSP). NOTE: CRSP data is not covered by
#' the GPL. Redistribution of the data is not permitted, and use of the data in
#' derivative works is not permitted without the written permission of CRSP.
#' 
#' @examples
#' data.table::setDTthreads(1)  
#' data(stocksCRSP)
#' names(stocksCRSP)
#' unique(stocksCRSP$Sector)
#' unique(stocksCRSP$CapGroup)
#' head(stocksCRSP,2)
"stocksCRSP"



#' @title stocksCRSPmonthly
#'
#' @description CRSP monthly stocks data for 294 stocks 1993 to 2015
#' 
#' @docType data
#'
#' @usage data(stocksCRSPmonthly)
#'
#' @format A data.table object with 82000 observations on 15 variables:
#' \itemize{
#'  \item \strong{Date:} type `Date`.
#'  \item \strong{TickerLast:} type `chr`. The ticker as of December 2015
#'  \item \strong{Ticker:} type `chr`. Monthly ticker
#'  period
#'  \item \strong{Company:} type `chr`. The name of company with TickerLast
#'  \item \strong{CapGroupLast:} type `chr`. Company market capitalization group
#'  as of December 2015, one of: MicroCap, SmallCap, MidCap or LargeCap
#'  \item \strong{CapGroup:} type `chr`. Monthly market capitalization group
#'  \item \strong{GICS:} type `chr`. 6 digit S&P GICS code
#'  \item \strong{Sector:} type `chr`. One of 10 sectors specified by the first
#'  two digits of the GICS code
#'  \item \strong{Return:} type `num`. Arithmetic stock return from one period
#'  to the next in decimal form
#'  \item \strong{RetExDiv:} type `num`.
#'  \item \strong{Price:} type `num`. Stock price at each time period in decimal
#'  form
#'  \item \strong{PrcSplitAdj:} type `num`.
#'  \item \strong{Ret4WkBill:} type `num`. Return of 4 week Treasury bill
#'  \item \strong{Ret13WkBill:} type `num`. Return of 13 week Treasury bill
#'  \item \strong{Ret1YrBill:} type `num`. Return of 1 year Treasury bill
#'  \item \strong{mktIndexCRSP:} type `num`. CRSP value weighted market return
#' }
#' 
#' @references
#' A standard corporate finance textbook:  Ross, Westerfield, Jaffe and
#' Jordan (2019). Corporate Finance, McGraw-Hill Education.
#' 
#' @details
#' The four CapGroupLast categorizations of the stocks were determined using the
#' three capitalization breakpoints $15.6B, $5.4B, $600M. Details concerning the
#' construction of the monthly CapGroup categorizations will eventually be
#' provided in a Vignette.
#' 
#' Weekly and daily versions stocksCRSPweekly and stocksCRSPdaily may be
#' obtained using the function getPCRAData() - see PCRAData.R. 
#' 
#' @source Center for Research in Security Prices (CRSP) at the University of
#' Chicago's Booth School of Business (CRSP). NOTE: CRSP data is not covered by
#' the GPL. Redistribution of the data is not permitted, and use of the data in
#' derivative works is not permitted without the written permission of CRSP.
#' 
#' @examples
#' data.table::setDTthreads(1)  
#' data(stocksCRSPmonthly)
#' names(stocksCRSPmonthly)
#' unique(stocksCRSPmonthly$Sector)
#' unique(stocksCRSPmonthly$CapGroup)
#' head(stocksCRSPmonthly,2)
"stocksCRSPmonthly"



