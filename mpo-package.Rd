\name{mpo-package}
\alias{mpo-package}
\alias{mpo}
\docType{package}
\title{
Modern Portfolio Optimization
}
\description{
MPO is a companion package to the text Modern Portfolio Optimization with R, second edition by R. Douglas Martin, Bernd Scherer, and Guy Yollin.
}
\details{
\tabular{ll}{
Package: \tab mpo\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2013-09-03\cr
License: \tab GPL-2\cr
}
~~ An overview of how to use the package, including the most important functions ~~
}
\author{
Guy Yollin, Doug Martin\cr
Maintainer: Guy Yollin <gyollin@r-programming.org>
}
\references{
Bernd Scherer and R. Douglas Martin (2007).\cr
Introduction to Modern Portfolio Optimization with NuOPT, S-PLUS and S+Bayes
}
\keyword{ package }
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
data(midcap.ts)
returns = midcap.ts[,1:10]
mathEfrontRisky(returns, 10, display=FALSE, digits = 3)
}
