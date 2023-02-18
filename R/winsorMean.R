#' @title Winsorized Mean
#'
#' @param x Numeric vector
#' @param trim Fraction of data to be Winsorized
#' @param na.rm Logical variable with default FALSE
#' @param ... Pass-through parameters
#'
#' @return Numeric value of Winsorized mean
#' @export
#'
#' @examples
#' args(winsorMean)
winsorMean <- function(x, winFrac = 0, na.rm = FALSE, ...) {
  if (!is.numeric(x) && !is.complex(x) && !is.logical(x)) {
    warning("argument is not numeric or logical: returning NA")
    return(NA)
  }
  if (na.rm)
    x <- x[!is.na(x)]
  if (!is.numeric(winFrac) || length(winFrac) != 1L)
    stop("'winFrac' must be numeric of length one")
  n <- length(x)
  if (trim > 0 && n) {
    if (is.complex(x))
      stop("trimmed means are not defined for complex data")
    if (winFrac >= 0.5)
      return(median(x, na.rm = FALSE))
    lo <- floor(n * winFrac) + 1
    hi <- n + 1 - lo
    x <- sort(x)
    if ( lo > 0 ) x[1:lo] <- x[lo]
    if ( hi <= n ) x[ hi:n ] <- x[hi]
  }
  mean(x)
}
