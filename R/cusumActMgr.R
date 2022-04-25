#' Using Statistical Process Control to Monitor Active Management
#'
#' @description Monitor the Logarithmic Information Ratio of a portfolio relative to its benchmark 
#' and raise an alarm when sufficient evidence accrues to indicate that its current LIR is <0
#' using an optimal changepoint detection scheme (the CUSUM algorithm)
#' An object of class \code{"tsfm"} is returned.
#'
#' @details
#' Assessing the performance of active managers is hard because active returns are noisy. 
#' In addition, their risk must be taken into account, and this is commonly measured by the 
#' standard deviation of active returns. Empirical studies of active managers across a wide range 
#' of asset classes suggest that an Annualized Logarithmic Information Ratio 
#' (LIR = Active Log Return / Std. Dev.(Active Log Return)) of 0.5 over a period of 5 years or more 
#' is exceptional, as markets are very efficient. The majority of active managers deliver active
#' returns and IR close to 0, and even those with a positive IR are at constant risk of having their
#' added value dissipate. Investors, therefore, must continually estimate the current performance of
#' their active portfolios and determine when sufficient evidence has accrued to suggest that their
#' active return and IR have fallen to 0. Put differently, investors need to rapidly detect changes,
#' particularly negative changes, in the performance of their portfolios. 
#' 
#' There is a rich literature on changepoint detection, and of the many available algorithms,
#' the CUSUM (an acronym for CUmulative SUM) stands out, on account of its simplicity, 
#' its robustness to the actual distribution of active returns, and the optimal trade-off 
#' between detection time and the rate of false alarms that it offers. 
#' It is closely retlated to Wald's Sequential Probability Ratio Test (SPRT) but is much simpler 
#' to implement, and requires minimal inputs from the user. In this application, it seeks to 
#' determine when the IR of a portfolio has changed from a good level (default = 0.5 )
#' to a bad level (default = 0). An alarm is raised when the CUSUM scheme crosses a threshold, 
#' which is chosen to make the average time between false alarms 84 months (7 years). 
#' By way of comparison, the time taken to detect a transition from good performance to bad is 
#' 41 months (or 3 1/2 years). This is much faster than the traditional t-test, which would take 
#' 16 years to obtain a t-statistic of 2. The threshold can be recalibrated to meet a user's needs.
#'
#' \subsection{Data Processing}
#'
#' @param portfolioName A character string with the name of the portfolio. Required, no default
#' @param benchmarkName A character string with the name of the benchamark. Required, no default
#' @param ret_df An xts object containing the columns \code{portfolioName} and \code{benchmarkName} 
#'               of monthly returns. Required, no default.
#' @param upperIR Numeric value representing the LIR of a good portfolio, default = 0.5
#' @param lowerIR Numeric value representing the LIR of a bad portfolio,  default = 0
#' @param lambda_in Exponential weighting constant used when the data seems consistent with 
#'                  the current estimate of volatility. Default = 0.9
#' @param lambda_out Exponential weighting constant used when the data seems inconsistent with 
#'                   the current estimate of volatility. Default = 0.8
#' @param winsorize Numeric value >1, of the multiple of standard deviation at which we winsorize.
#'                  The default is set to 4.
#' @param filterStd Logical value, determines if estimated standard deviations are filtered.
#'                  The default is set to \code{FALSE}.
#'
#'
#' @return \code{cusumActMgr} returns a \code{list} containing the following xts objects:
#' \item{Log_Active_Returns}{Logarithmic active returns of the fund relative to its benchmark}
#' \item{Annual_Moving_Average}{The vector of annual moving average returns}
#' \item{Tracking_Error}{The monthly tracking error of the logarithmic excess returns}
#' \item{Information_Ratios}{The vector of monthly information ratios}
#' \item{Lindley's_Recursion}{The vector Lindley's recursion with a reset after the detection threshold (6.66) is passed.}
#' \item{Annualized_Cusum_IR}{The vector annualized CUSUM of the information ratios}
#' \item{Means}{The xts matrix of estimated means of the fund in the first columns,
#' the benchmark in the second column, and the excess logarithmic returns in the third column}
#' \item{Standard_deviations}{The xts matrix of estimated standard deviations of 
#' the fund in the first columns, the benchmark in the second column, and the 
#' logarithmic active returns in the third column.
#' It will not be filtered unless \code{filterStd = TRUE} is specified.}
#' \item{Protractor}{The xts matrix of the rays from IR = -3 in the first column
#' to IR = 3 in the seventh column used in the CUSUM IR as a protractor.}
#' \item{Excess_Volatility}{The difference between the annualized Standard deviation of the 
#' portolio and its benchmark}
#'
#' @author Chindhanai Uthaisaad.
#'
#' @references
#' Philips, T. K., Yashchin, E. and Stein, D. M. (2003). Using Statistical Process Control
#' to Monitor Active Managers, Journal of Portfolio Management, Fall 2003, pp. 86-94.
#'
#' @examples
#' #Data Preprocessing
#' library(xts)
#' library(zoo)
#' df <- read.csv("Example1.csv", header = T, sep = "," , stringsAsFactors = FALSE)
#' df$Date <- as.yearmon(df$Date, format = "%m/%d/%Y")
#' df <- as.xts(df, order.by = df$Date)  
#' results <- cusumActMgr(portfolioName = "Small Growth Portfolio", benchmarkName = "Russell 2500", ret_df = df)
#' @export

cusumActMgr <- function(portfolioName, benchmarkName, ret_df, 
                        upperIR = 0.5, lowerIR = 0, 
                        lambda_in = 0.10, lambda_out = 0.20,
                        winsorize = 4, filterStd = FALSE) {
  
  # Record the call as an element to be returned
  this.call <- match.call()
  
  # Check to ensure that all arguments are valid
  if (missing(ret_df) || !is.xts(ret_df)) {
    stop("Invalid args: ret_df must be an xts object")
  }
  
  if(missing(portfolioName) || !is.character(portfolioName)){
    stop("Invalid args: portfolioName must be a character string")
  }
  
  if(missing(benchmarkName) || !is.character(benchmarkName)){
    stop("Invalid args: benchmarkName must be a character string")
  }
  
  if(winsorize < 1){
    stop("Invalid args: the threshold for winsorization (winsorize) should be > 1")
  }
  
  if(lambda_in < 0 || lambda_in >1 || lambda_out < 0 || lambda_out > 1){
    stop("Invalid args: both lambdas must lie between 0 and 1")
  }
  
  if(!is.logical(filterStd)){
    stop("Invalid args: filterStd must be a logical value")
  }
  
  # Obtain the return and benchmark
  portfolioReturns <- ret_df[,portfolioName]
  benchmarkReturns <- ret_df[,benchmarkName]
  n = length(portfolioReturns)
  
  if (n < 2) {
    stop("Invalid args: portfolio returns and benchmark returns must have length >= 2")
  }
  
  if (n != length(benchmarkReturns)) {
    stop("Invalid args: portfolio returns and benchmark returns must have the same length")
  }
  
  # Initialize logarithmic excess returns, IR, Lindley's Recursion and TE
  priorMonth <- as.yearmon(first(index(portfolioReturns)))-1/12
  index_TE   <- c(priorMonth, index(portfolioReturns))
  Lindley    <- xts(rep(0, n+1), order.by = index_TE)
  
  # Compute the Logarithmic Excess Returns
  logExcessReturns = log((1 + portfolioReturns)/(1 + benchmarkReturns))
  
  ######< Mean and Filtered Std. Dev. of the fund and benchmark >#######
  Means = uStds = fStds = matrix(0, ncol = 3, nrow = n+1)
  Means[1, 1] <- ifelse(n>=11, mean(portfolioReturns[1:11]), mean(portfolioReturns))
  Means[1, 2] <- ifelse(n>=11, mean(benchmarkReturns[1:11]), mean(benchmarkReturns))
  Means[1, 3] <- ifelse(n>=11, mean(logExcessReturns[1:11]), mean(logExcessReturns))
  uStds[1, 1] =  fStds[1, 1] = ifelse(n>=6, 1.25 * median(abs(portfolioReturns[1:6])), 1.25 * median(abs(portfolioReturns)))
  uStds[1, 2] =  fStds[1, 2] = ifelse(n>=6, 1.25 * median(abs(benchmarkReturns[1:6])), 1.25 * median(abs(benchmarkReturns)))
  uStds[1, 3] =  fStds[1, 3] = ifelse(n>=6, 1.25 * median(abs(logExcessReturns[1:6])), 1.25 * median(abs(logExcessReturns)))
  
  
  # Update the means and unfiltered standard deviations for the fund and benchmark
  for(i in 1:n){
    Means[i+1, 1] <- muEst(coredata(portfolioReturns[i]),    Means[i, 1],   uStds[i, 1], winsorize, lambda_in)
    uStds[i+1, 1] <- sigmaEst(coredata(portfolioReturns[i]), Means[i+1, 1], uStds[i, 1], winsorize, lambda_in, lambda_out)
    Means[i+1, 2] <- muEst(coredata(benchmarkReturns[i]),    Means[i, 2],   uStds[i, 2], winsorize, lambda_in)
    uStds[i+1, 2] <- sigmaEst(coredata(benchmarkReturns[i]), Means[i+1, 2], uStds[i, 2], winsorize, lambda_in, lambda_out)
    Means[i+1, 3] <- muEst(coredata(logExcessReturns[i]),    Means[i, 3],   uStds[i, 3], winsorize, lambda_in)
    uStds[i+1, 3] <- sigmaEst(coredata(logExcessReturns[i]), Means[i+1, 3], uStds[i, 3], winsorize, lambda_in, lambda_out)
  }
  
  Stds = uStds
  
  if (filterStd){
    # Filter the standard deviations - jump quickly, use averaging when coming down
    for (i in 1:n){
      fStds[i+1, 1] <- ifelse(uStds[i+1, 1] > uStds[i, 1], uStds[i+1, 1], 0.5*(uStds[i, 1] + uStds[i+1, 1]))
      fStds[i+1, 2] <- ifelse(uStds[i+1, 2] > uStds[i, 2], uStds[i+1, 2], 0.5*(uStds[i, 2] + uStds[i+1, 2]))
      fStds[i+1, 3] <- ifelse(uStds[i+1, 3] > uStds[i, 3], uStds[i+1, 3], 0.5*(uStds[i, 3] + uStds[i+1, 3]))
    }
    Stds = fStds
  }
  
  Means <- xts(Means, order.by = index_TE)
  Stds  <- xts(Stds,  order.by = index_TE)
  colnames(Means) <- colnames(Stds) <- c("Fund", "Benchmark", "Excess")
  
  # Excess volatility: difference between vol of portfolio and benchmark
  Vol       <- matrix(0, ncol = 3, nrow = n+1)
  Vol[1, 1] <- sqrt(12) * sd(coredata(portfolioReturns))
  Vol[1, 2] <- sqrt(12) * sd(coredata(benchmarkReturns))
  Vol[2:(n+1), ] <- Stds[2:(n+1), ] * sqrt(12)
  Vol[, 3]  <- Vol[, 1] - Vol[, 2]
  colnames(Vol) <- c("FundVol", "BenchmarkVol", "ExcessVol")
  Vol <- xts(Vol, order.by = index_TE)
  
  # Average level of the upper and lower IR inputs
  avgIRLevel <- 0.5 * (upperIR + lowerIR) / sqrt(12)
  
  #####Begin looping through the new returns#####
  IR <- coredata(logExcessReturns) / coredata(Stds[-(n+1), 3])
  IR <- xts(IR, order.by = index(portfolioReturns))
  
  for (i in 1:length(portfolioReturns)) {
    # Lindley's recursion (Log-likelihood ratios)
    Lindley[i+1] <- ifelse(coredata(Lindley[i]) - coredata(IR[i]) + avgIRLevel < 0, 0, 
                           ifelse(coredata(Lindley[i]) > 6.66, max(0,avgIRLevel - IR[i]), 
                                  coredata(Lindley[i]) - coredata(IR[i]) + avgIRLevel))
  }
  
  # 12 month moving average returns
  AMA <- xts(rep(0, n), order.by = index(portfolioReturns))
  for(i in 1:n){
    AMA[i] <- ifelse(i<12, mean(logExcessReturns[1: i]), mean(logExcessReturns[(i-11) : i]))
  }
  
  # CUSUM IR
  cusumIR <- xts(cumsum(coredata(IR)), order.by = index(IR))
  
  # Information obtained from annualized IR
  annualizedIR <- sqrt(12) * coredata(cusumIR)
  lowerLimIR   <- min(annualizedIR)
  upperLimIR   <- max(annualizedIR)
  
  spreadIR <- upperLimIR - lowerLimIR
  avgIR    <- spreadIR / n
  
  upperPosIR <- which.max(annualizedIR)
  lowerPosIR <- which.min(annualizedIR)
  
  medIR  <- lowerLimIR + 0.5 * spreadIR
  peakIR <- spreadIR / (upperPosIR - lowerPosIR)
  maxIR  <- 0.5 * ceiling(abs(peakIR) + abs(avgIR))
  
  protractor_widthIR  <- ceiling(0.9 * spreadIR / (2 * maxIR))
  protractor_heightIR <- abs(protractor_widthIR*maxIR)
  
  # Slopes -3 to 3
  RaysIR <- matrix(0, ncol = 7, nrow = n+1)
  RaysIR[1, 1] <- medIR + protractor_heightIR
  for(j in 2:7){
    RaysIR[1,j] <- RaysIR[1, 1] - (j-1) * protractor_heightIR / 3
  }
  
  for(i in 2:(n+1)){
    for(j in 1:4){
      RaysIR[i, j] <- max(RaysIR[i-1, j] - ((RaysIR[1, j] - medIR) / protractor_widthIR), medIR)
    }
    for(j in 5:7){
      RaysIR[i, j] <- min(RaysIR[i-1, j] - ((RaysIR[1, j] - medIR) / protractor_widthIR), medIR)
    }
  }
  
  RaysIR <- xts(RaysIR, order.by = index_TE)
  colnames(RaysIR) <- c("Ray-3", "Ray-2", "Ray-1", "Ray0", "Ray+1", "Ray+2", "Ray+3")
  
  # CUSUM Excess Returns
  cusumER <- xts(100 * cumsum(coredata(logExcessReturns)), order.by = index(logExcessReturns))
  
  # Information obtained from annualized IR
  annualizedER <- 12 * coredata(cusumER)
  lowerLimER   <- min(annualizedER)
  upperLimER   <- max(annualizedER)
  
  spreadER <- upperLimER - lowerLimER
  avgER    <- spreadER / n
  
  upperPosER <- which.max(annualizedER)
  lowerPosER <- which.min(annualizedER)
  
  medER  <- lowerLimER + 0.5 * spreadER
  peakER <- spreadIR / (upperPosER - lowerPosER)
  maxER  <- 0.5 * ceiling(abs(peakER) + abs(avgER))
  protractor_widthER  <- ceiling(0.9 * spreadER / (2 * maxER))
  protractor_heightER <- abs(protractor_widthER * maxER)
  
  # Slopes -3 to 3
  RaysER       <- matrix(0, ncol = 7, nrow = n+1)
  RaysER[1, 1] <- medER + protractor_heightER
  for(j in 2:7){
    RaysER[1, j] <- RaysER[1, 1] - (j-1) * protractor_heightER / 3
  }
  
  for(i in 2:(n+1)){
    for(j in 1:4){
      RaysER[i, j] <- max(RaysER[i-1, j] - ((RaysER[1, j] - medER) / protractor_widthER), medER)
    }
    for(j in 5:7){
      RaysER[i, j] <- min(RaysER[i-1, j] - ((RaysER[1, j] - medER) / protractor_widthER), medER)
    }
  }
  
  RaysER = xts(RaysER, order.by = index_TE)
  colnames(RaysER) <- c("Ray-3", "Ray-2", "Ray-1", "Ray0", "Ray+1", "Ray+2", "Ray+3")
  
  
  # Return the updated likelihood ratios exceeding the threshold
  return(list("Log_Active_Returns" = logExcessReturns,
              "Annual_Moving_Average" = AMA,
              "Tracking_Error" = Stds[,3],
              "Information_Ratios" = IR,
              "Lindley's_Recursion" = Lindley,
              "Annualized_Cusum_IR" = annualizedIR,
              "Annualized_Cusum_ER" = annualizedER,
              "Means" = Means,
              "Protractor_IR" = RaysIR,
              "Protractor_ER" = RaysER,
              "Standard_Deviations" = Stds,
              "Excess_Volatility" = Vol))
}


#' muEst returns a simple weighted average of the current return and the last period's return.
#'
#' @param r Current period return
#' @param mu0 Mean return from the last period
#' @param sigma0 Estimate of volatility from the last period. If unavailable, set sigma0 = 0
#' @param win_level Number of standard deviations at which we winsorize (default: win_level =4)
#' @param lambda Exponential weighting constant (default: lambda = 0.9)
#'
#' @return
#' @export
#'
#' @examples
muEst <- function(r, mu0, sigma0, win_level = 4, lambda = 0.9){
  if (abs(r - mu0) > win_level * sigma0 ) {
    #Winsorize if the current return is exceptionally large
    r <- mu0 + sign(r - mu0) * win_level * sigma0
    }
  return(lambda *  mu0 + (1 - lambda) * r)
  }


#' sigmaEst computes a EW estimate of volatility
#'
#' @param r Current period return
#' @param mu0 Prior period mean return (often set to 0 in finance as market efficiency diminishes the mean)
#' @param sigma0 Prior period estimated volatility. If unavailable (e.g. in the first period), sigma0 = 0
#' @param win_level Number of standard deviations at which we winsorize (default: 4)
#' @param lambda_in EW constant when the data seems consistent with the current estimate of volatility (default: 0.9)
#' @param lambda_out EW constant when the data seems inconsistent with the current estimate of volatility (default: 0.8)
#'
#' @return
#' @export
#'
#' @examples
sigmaEst <- function(r, mu0, sigma0, win_level = 4, lambda_in = 0.9, lambda_out = 0.8){
    # Shift lambda down if current return is exceptionally large or exceptionally small
    lambda <- ifelse( (abs(r-mu0) > sigma0 / win_level) && (abs(r-mu0) < sigma0 * win_level), lambda_in, lambda_out)
    return(sqrt(lambda * sigma0^2 + (1 - lambda) * (r-mu0)^2))
  }




