#' @keywords internal
#' @import data.table
#' @importFrom grDevices topo.colors
#' @importFrom graphics abline arrows axis barplot box boxplot identify legend lines mtext par points polygon segments strheight strwidth text title
#' @importFrom methods cbind2
#' @importFrom stats cov dnorm integrate lm mad median pnorm qchisq sd uniroot var qnorm qqnorm
#' @importFrom utils globalVariables
#' @importFrom lattice panel.abline xyplot panel.xyplot
#' @importFrom xts xts as.xts apply.monthly
#' @importFrom zoo zoo coredata index index<-
#' @importFrom boot boot
#' @importFrom PerformanceAnalytics checkData		
#' @importFrom PortfolioAnalytics create.EfficientFrontier portfolio.spec add.constraint  add.objective optimize.portfolio
#' @importFrom corpcor make.positive.definite
#' @importFrom quadprog solve.QP
#' @importFrom RobStatTM locScaleM lmrobdet.control lmrobdetMM
#' @importFrom robustbase scaleTau2
#' @importFrom R.cache loadCache saveCache
#' @importFrom utils download.file

"_PACKAGE"

globalVariables(c("Date", "MktIndexCRSP", "Ret13WkBill","stocksCRSP","stocksCRSPweekly","stocksCRSPdaily","factorsSPGMI","..stockItems","..fac_sel","CapGroupLast"))
