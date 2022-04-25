\name{largecapM}
\alias{largecapM}
\docType{data}
\title{
largecapM
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(largecapM)}
\format{
  The format is:
An 'xts' object on 1997-01-31/2010-12-31 containing:
  Data: num [1:168, 1:22] -0.06886 0.00965 -0.01752 0.03079 0.17296 ...
 - attr(*, "dimnames")=List of 2
  ..$ : NULL
  ..$ : chr [1:22] "ORCL" "MSFT" "HON" "EMC" ...
  Indexed by objects of class: [Date] TZ: UTC
  xts Attributes:  
 NULL
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(smallcapM)
head(smallcapM[,1:5])
}
\keyword{datasets}
