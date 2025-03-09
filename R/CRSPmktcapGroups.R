#' @title CRSPmktcapGroups
#' 
#' @description Biennial counts of bigcap, smallcap, microcap stocks among
#' liquid CRSP database stocks from 1964 to 2018 using weekly returns.  Bigcap
#' stocks consist of midcap, largecap, and megacap stocks.  For each contiguous
#' two-year interval, liquid stocks are those with no missing returns and at
#' most 4 returns with value 0. 
#'
#' @docType data
#'
#' @usage data(CRSPmktcapGroups)
#'
#' @format A multivariate xts object
#' 
#' @source The microcap, smallcap and bigcap groups were defined using the
#' using the 20th and 50th percentiles of the NYSE capitalization data as
#' breakpoints to separate these three market cap groups.  
#' 
#' @examples
#' data(CRSPmktcapGroups)
#' names(CRSPmktcapGroups)
#' dim(CRSPmktcapGroups)