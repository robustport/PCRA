% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/factorsSPGMI.R
\docType{data}
\name{factorsSPGMIr}
\alias{factorsSPGMIr}
\title{Rounded Version of SPGMI Data 14 Factors}
\format{
A data.frame containing 14 SPGMI numeric factor exposures (alpha
factors) for each of 294 stocks from 1993 to 2015 (276 months), among a
total of 22 variables, that also include 4 categorical factor exposures.
\itemize{
 \item \strong{Date:} type `Date`.
 \item \strong{TickerLast:} type `chr`. This is the ticker as of December 2015
 \item \strong{Ticker:} type `chr`. This is the monthly ticker
 \item \strong{Company:} type `chr`. The name of the company 
 \item \strong{CapGroupLast:} type `chr`. Company market capitalization group
 as of December 2015, one of: MicroCap, SmallCap, MidCap or LargeCap
 \item \strong{CapGroup:} type `chr`. Monthly market capitalization group
 \item \strong{GICS:} type `chr`. An 8 digit S&P GICS code, the first two
 digits of which are codes for 11 GICS sectors
 \item \strong{Sector:} type `chr`. One of 8  of the 11 GICS sectors, with
 none of the 294 stocks in Financials, Real Estate or Utilities
 \item \strong{AnnVol12M:} type `num`. Annualized Volatility of Monthly
 Stock Returns (Last Twelve Months)
 \item \strong{Beta60M:} type `num`.	60 Month OLS Beta relative to the
 S&P 500 estimated using Monthly Total Returns
 \item \strong{BP:} type `num`. Most Recent Book Value of
 Common Equity divided by Market Value of Common Equity
 \item \strong{EP:} type `num`. Sum of trailing four quarters Earnings per
 Share divided by Current Price per share
 \item \strong{LogMktCap:} type `num`. Natural Logarithm of Current Market
 Capitalization in $
 \item \strong{PM12M1M:} type `num`. Price relative change from time t-12 to
 time t-1: PM12M1M(t) = (P(t-1)-P(t-12))/P(t-12) = P(t-1)/P(t-12) – 1
 \item \strong{AccrualRatioCF:} type `num`. Ratio of Accruals to Net
 Operating Assets, where Accruals = Income Before Extraordinary Items minus
 Net Operating Cash Flow minus Net Investing Cash Flow, and Net Operating
 Assets = Total Assets – Total Liabilities – Cash and Short-Term Investments 
 + Short- and Long-Term Debt.  Both numerator and denominator are computed
 over the trailing four quarters
 \item \strong{AstAdjChg1YOCF:} type `num`. One-Year Change in Trailing
 Four Quarter Operating Cash Flow divided by Trailing Four Quarter
 Average(Total Assets)
 \item \strong{CFROIC:} type `num`. Trailing Four Quarter Operating Cash Flow
 divided by Trailing Four Quarter Average of Invested Capital where Invested
 Capital = Sum of Long-Term Debt, Preferred Stock, Common Equity and Minority
 Interests – Treasury Stock 
 \item \strong{Chg1YAstTo:} type `num`. Percentage change over 12 months in
 Trailing Four Quarter Revenues divided by Trailing Four Quarter Average of
 Total Assets
 \item \strong{EBITDAEV:} type `num`. Trailing Four Quarter EBITDA divided by
 Average of Trailing Four Quarter Enterprise Value where Enterprise Value =
 Book Value of Equity + Market Value of Debt
 \item \strong{FCFP:} type `num`. Trailing Four Quarter Free Cash Flow
 divided by Trailing Four Quarter Average of Market Value of Equity
 \item \strong{PM1M:} type `num`. Trailing 1-Month Price Return.  Relative
 price change from time t-1 to t: PM1M(t) =  (P(t) – P(t-1)) / P(t-1),
 commonly called one period return R(t)
 \item \strong{SEV:} type `num`. Trailing Four Quarter Sales divided by
 Average of Trailing Four Quarter Enterprise Value, where Enterprise Value =
 Market Value of Equity + Market Value of Debt
}
}
\source{
Standard and Poors Global Market Intelligence (SPGMI). NOTE: SPGMI
data is not covered by the GPL. Redistribution of this SPGMI data is not
permitted, and use of the data in derivative works is not permitted without
the written permission of SPGMI
}
\usage{
data(factorsSPGMIr)
}
\description{
Rounded version of factorsSPGMI to 4 decimal places.
}
\details{
The term "factor exposures" is often used for the values of the 14 factors,
which SPGMI also refers to as "scores" or "alpha factors". 
Our names for the the 14 factors are identical to those used by SPGMI in
their AFL library.  For an introduction to the AFL library see:
\url{https://www.marketplace.spglobal.com/en/datasets/alpha-factor-library-(3)}

For details concerning the very small effects of rounding, see the Vignette 
"PCRA Package Overview.

The four CapGroupLast categorizations of the stocks were determined using the
three capitalization breakpoints $xxxM, $yyyM, $zzzM. Details concerning the
construction of the monthly CapGroup categorizations will eventually be
provided in a Vignette.

The factorsSPGMI data contains stocks in 8 of the 11 GICS sectors, with no
stocks in the Financials, Utilities and Real Estate sectors.  On each of the
next 11 lines we list all 11 of the two digit GICS code that defines the GICS
Sector, followed by the GICS sector name:

10 Energy
   
15 Materials
   
20 Industrials

25 Consumer Discretionary

30 Consumer Staples

35 Health Care  

40 Financials (none currently available)

45 Information Technology

50 Communication Services

55 Utilities (none currently available)

60 Real Estate (none currently available)
 
GICS is a joint product of SPGMI and MSCI.
For details, see the GICS Global Industry Classification Standard document
(The GICS MAP Book) available at \url{https://www.spglobal.com/en/}, and the MSCI
GICS Methodology 2020 document available at \url{https://www.msci.com/}.
}
\references{
A standard corporate finance textbook:  Ross, Westerfield, Jaffe and
Jordan (2019). Corporate Finance, McGraw-Hill Education.
CFA: \url{https://alphabetaprep.com/cfa-level-1/financial-ratio-analysis/}
}
\keyword{datasets}
