\name{regressFluxTanh}
\alias{regressFluxTanh}
\title{regressFluxTanh}
\description{Estimate the initial flux by fitting a hyperbolic tangent saturating function}
\usage{regressFluxTanh(conc, times, start = c(), cSatFac = 2, 
    tryAutoCorr = TRUE)}
\arguments{
  \item{conc}{numeric vector of CO2 concentrations []}
  \item{times}{times of conc measurements	[seconds]}
  \item{start}{numeric vector of starting parameters. May provide from last bootstrap to speed up fitting}
  \item{cSatFac}{Position of the initial estimate of saturation (0 start, 1 end, >1 outside measured range)}
  \item{tryAutoCorr}{set to FALSE to not try to fit model with autocorrelation}
}
\details{For efficiency reasons, does not check for missing values (NA).
The caller must provide conc and times all finite.}


\author{Thomas Wutzler, Oscar Perez Priego}



\seealso{\code{\link{regressFluxExp}}

\code{\link{RespChamberProc}}}
\examples{
#data(chamberLoggerEx1s)
ds <- chamberLoggerEx1s[-(1:16),]
conc <- ds$CO2_dry <- corrConcDilution(ds)  
times <- ds$TIMESTAMP
times0 <- as.numeric(times) - as.numeric(times[1])
#trace(regressFluxTanh, recover)	#untrace(regressFluxTanh)
(res <- regressFluxTanh( conc, times  ))
plot( conc ~ times)
lines( fitted(res$model) ~ times )
}
