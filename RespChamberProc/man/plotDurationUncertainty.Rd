\name{plotDurationUncertainty}
\alias{plotDurationUncertainty}
\title{plotDurationUncertainty}
\description{plot the increase of uncertainty with decreaseing measurement duration}
\usage{plotDurationUncertainty(ds, colTime = "TIMESTAMP", 
    fRegress = c(exp = regressFluxExp, lin = regressFluxLinear, 
        tanh = regressFluxTanh), ..., durations = seq(max(65, 
        resFit0$tLag), max(times0), length.out = nDur + 
        1), nDur = 20, maxSdFlux = 1)}
\arguments{
  \item{ds}{
}
  \item{colTime}{column name of time [s]}
  \item{fRegress}{list of functions to yield
<< a single flux estimate, see details of \code{\link{calcClosedChamberFlux}}}
  \item{\dots}{further arguments to \code{\link{calcClosedChamberFlux}}}
  \item{durations}{durations to check. Default is equally spaced between tLag and maximum duration}
  \item{nDur}{number of durations to check}
  \item{maxSdFlux}{maxium allowed standard deviation of flux in [mumol / s]}
}
\details{Produces a plot with standard deviation of the flux estimate versus the duration of the measurment.
The lines correspond to the given maxium acceptable standard deviation
and the duration that matches this criterion.}
\value{tibble result of \code{\link{calcClosedChamberFlux}} for the minimum duration, with additional component 
\item{statAll}{tibble: each row a fit for a given duration
}}

\author{Thomas Wutzler, Oscar Perez Priego}




\examples{
#data(chamberLoggerEx2)
ds <- subset(chamberLoggerEx2, iChunk==99)	# very strong (and therefore precise) uptake
#plot( CO2_dry ~ TIMESTAMP, ds )
resDur <- plotDurationUncertainty( ds, colTemp="AirTemp", volume = 0.6*0.6*0.6
	, maxSdFlux = 0.8
	, nDur=10
	, durations = c(100,120,150)
)
resDur$duration
#plot( flux ~ duration, resDur$statAll[[1]] )
#plot( sdFlux ~ duration, resDur$statAll[[1]] )
}
