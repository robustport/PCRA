#' fitTsfmStats
#'
#' @param x 
#' @param rsq 
#' @param digits 
#' @param show.signif.stars 
#'
#' @return
#' @export
#'
#' @examples
fitTsfmStats = function(x,rsq = F,digits = 3, show.signif.stars=TRUE) {
  fitSum = summary(x)
  out = data.frame(fitSum$sum.list[[1]]$coefficients)
  names(out) = c("Estimate","S.E.","t-Stat","p-Value")
  out <- round(out,digits)
  if( show.signif.stars) {
    out <- cbind(out, signifStars(out[,"p-Value"]))
    names(out) = c("Estimate","S.E.","t-Stat","p-Value","")
  }
  print(out)
  if(rsq) {
    out = fitSum$sum.list[[1]]$r.squared
    out = data.frame(out,row.names = "R-Squared =")
    names(out) = ""
    print(round(out,digits))
  }
}
