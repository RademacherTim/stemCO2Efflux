\name{regressFluxLinear}
\alias{regressFluxLinear}
\title{regressFluxLinear}
\description{Estimate the initial flux by linear regression}
\usage{regressFluxLinear(conc, times, start = c(), tryAutoCorr = TRUE)}
\arguments{
  \item{conc}{numeric vector of CO2 concentrations []}
  \item{times}{times of conc measurements	[seconds]}
  \item{start}{numeric vector of starting parameters. May provide from last bootstrap to speed up fitting}
  \item{tryAutoCorr}{set to FALSE to not try to fit model with autocorrelation}
}

\value{list with entries
\item{stat}{numeric vector (2) with entries: flux, sdFlux, AIC, and autoCorr:}
\item{flux}{flux estimate at starting time}
\item{sdFlux}{standard deviation of flux}
\item{AIC}{model fit diagnostics}
\item{autoCorr}{coefficient of autocorrelation
or NA if model with autocorrelation could not be fitted or had higher AIC than model without autocorrelation}
\item{times}{used predictor vector, can be used for return for plotting}
\item{model}{the model-fit object (here of class gls)
}}

\author{Thomas Wutzler, Oscar Perez Priego}



\seealso{\code{\link{regressFluxExp}}

\code{\link{RespChamberProc}}}
\examples{
#data(chamberLoggerEx1s)
ds <- chamberLoggerEx1s
conc <- ds$CO2_Avg
times <- ds$TIMESTAMP
plot( conc ~ times )
regressFluxLinear( conc, times  )
}
