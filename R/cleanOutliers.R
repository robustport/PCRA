#' @title Clean Returns Outliers
#'
#' @description Outliers are "cleaned" by shrinking or rejecting data whose
#' distance from the median (med) is larger in absolute value than a specified
#' value k multiplied by the median absolute deviation from the median (mad).
#' Outlier shrinkage results in the data value being set equal to the nearest
#' of med-k*mad and med+k*mad. Rejected data is assigned an NA. Shrinkage is
#' the default.
#' 
#' @param x A numeric vector
#' @param k A numeric value, which multiplies the mad.  Smaller values of k
#' result in greater fractions of data which is either shrunk of rejected,
#' and larger values of k result in smaller fractions of the data that are
#' shrunk or rejected.
#' 
#' @param shrink A logical variable whose default is TRUE.
#'
#' @return Outlier cleaned data
#' @export
#'
#' @examples
#' args(cleanOutliers)
cleanOutliers <- function (x, k = 3, shrink = TRUE)
{
  if(length(k) != 1 || k <= 0) {
    stop("unallowed threshold k")
  }
  med <- median(x)
  bnd <- k*mad(x)
  if(shrink) {
    x[ x > med + bnd ] <- med + bnd
    x[ x < med - bnd ] <- med - bnd
  } else{
    x[ x > med + bnd ] = NA
    x[ x < med - bnd ] = NA
    x <- x[!is.na(x)] 
  }
  return(x)
}