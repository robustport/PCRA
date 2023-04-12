#' @title Winsorize Data
#' 
#' @description  This function Winsorizes a fraction gamma of a numeric data
#' set.
#'
#' @param x A numeric data set
#' @param fraction A fraction greater than 0 and less than 0.5
#' 
#' @details The Winsorized data is obtained by by setting the gamma smallest
#' data values equal to the next smallest value, and setting the gamma largest
#' data values equal to the next largest data value. 
#'
#' @return The Winsorized numeric data
#' @export
#'
#' @examples
#' x <- rt(10,8)
#' winsorize(x,0.2)
winsorize <- function(x,fraction = 0.1)
{
  n <- length(x)
  lo <- floor(n * fraction) + 1
  hi <- n + 1 - lo
  x <- sort(x)
  if (lo > 0) 
    x[1:lo] <- x[lo]
  if (hi <= n) 
    x[hi:n] <- x[hi]
  x
}