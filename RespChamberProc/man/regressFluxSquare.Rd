\name{regressFluxSquare}
\alias{regressFluxSquare}
\title{regressFluxSquare}
\description{Estimate the initial flux and its standard deviation by polynomial regression}
\usage{regressFluxSquare(conc, times, start = c(), tryAutoCorr = TRUE)}
\arguments{
  \item{conc}{numeric vector of CO2 concentrations [ppm]}
  \item{times}{times of conc measurements	[seconds]

used predictor vector, can be used for return for plotting}
  \item{start}{numeric vector of starting parameters. Not used here.}
  \item{tryAutoCorr}{set to FALSE to not try to fit model with autocorrelation}
}



\author{Thomas Wutzler, Oscar Perez Priego}



\seealso{\code{\link{regressFluxExp}}

\code{\link{RespChamberProc}}}
\examples{
#data(chamberLoggerEx1s)
ds <- chamberLoggerEx1s
conc <- ds$CO2_dry <- corrConcDilution(ds)  
times <- ds$TIMESTAMP
res <- regressFluxSquare( conc, times  )
plot( conc ~ times)
m <- attr(res,"model")
#lines( fitted(m) ~ times )
}
