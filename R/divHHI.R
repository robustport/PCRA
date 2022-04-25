#' divHHI is the function to calculate the diversification index for a set of portfolio weights. DIV is one minus the HHI index.
#'
#' @param weights 
#'
#' @return
#' @export
#'
#' @examples
divHHI = function(weights){
  n.dates=nrow(weights)
  if(n.dates<1){
    print("empty data set")
    return()
  }
  diversification=rep(0,n.dates)
  for(i in 1:n.dates){
    diversification[i]=1-sum(weights[i,]^2)
  }
  dates=index(weights)
  Div=zoo(diversification,dates)
  return(Div)
}
