\name{sigmaBootLeverage}
\alias{sigmaBootLeverage}
\title{sigmaBootLeverage}
\description{Estimate uncertainty of regrssion due to leverage of starting and end points. }
\usage{sigmaBootLeverage(conc, times, fRegress = regressFluxExp, 
    probs = c(0.05, 0.5, 0.95), nSample = 80, coefStart = c(), 
    tryAutoCorr = TRUE)}
\arguments{
  \item{conc}{numeric vector of CO2 concentrations}
  \item{times}{times of conc measurements}
  \item{fRegress}{function to yield a single flux estimate}
  \item{probs}{percentiles for which the flux quantiels shoudl be returned}
  \item{nSample}{
}
  \item{coefStart}{numeric vector of starting parameters. May provide from last bootstrap to speed up fitting}
  \item{tryAutoCorr}{set to FALSE to not try to fit model with autocorrelation}
}



\author{Thomas Wutzler, Oscar Perez Priego}



\seealso{\code{\link{RespChamberProc}}}
\examples{
#data(chamberLoggerEx1s)
ds <- chamberLoggerEx1s
  sigmaBootLeverage( ds$CO2_Avg, ds$TIMESTAMP )
}
