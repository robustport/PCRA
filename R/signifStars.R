#' signifStars
#'
#' @param pv 
#'
#' @return
#' @export
#'
#' @examples
signifStars <- function(pv) {
  cutpoints = c(0.001, 0.01, 0.05, 0.1)
  symbols = c("***", "** ", "*  ", ".  ")
  func <- function(x) {
    ind <- which(x < cutpoints)[1]
    if( is.na(ind) ) "" else symbols[ind]
  }
  sapply(pv, func)
}
