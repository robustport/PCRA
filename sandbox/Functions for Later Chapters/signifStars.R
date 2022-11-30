#' signifStars
#'
#' @param pv 
#'
#' @return p-values with none, one, two, or three star symbols added
#' @export
#'
#' @examples
#' args(signifStars)
signifStars <- function(pv) {
  cutpoints = c(0.001, 0.01, 0.05, 0.1)
  symbols = c("***", "** ", "*  ", ".  ")
  func <- function(x) {
    ind <- which(x < cutpoints)[1]
    if( is.na(ind) ) "" else symbols[ind]
  }
  sapply(pv, func)
}
