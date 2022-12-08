#' crsp.returns8
#' 
#' Monthly returns of 8 stocks with tickers GHI, PBCI, MODI, MGJ, MAT,
#' EMN, AMAT, AMGN from 1997 to 2001
#'
#' @docType data
#'
#' @usage data('crsp.returns8')
#'
#' @format A multivariate xts object
#' 
#' @source Center for Research in Security Prices, LLC (CRSP), an Affiliate of
#' the University of Chicago Booth School of Business. NOTE: CRSP data is not
#' covered by the GPL. Redistribution of the data in any form is not permitted,
#' and use of the data in derivative works is not permitted without the written
#' permission of CRSP.
#' 
#' @examples
#' data(crsp.returns8)
#' names(crsp.returns8)
#' dim(crsp.returns8)
#' range(index(crsp.returns8))
"crsp.returns8"