\name{regressFluxExp}
\alias{regressFluxExp}
\title{regressFluxExp}
\description{Estimate the initial flux by fitting an exponentially saturating function}
\usage{regressFluxExp(conc, times, start = c(), tryAutoCorr = TRUE, 
    cSatFac = 1.5)}
\arguments{
  \item{conc}{numeric vector of CO2 concentrations []}
  \item{times}{times of conc measurements	[seconds]}
  \item{start}{numeric vector of starting parameters. May provide from last bootstrap to speed up fitting}
  \item{tryAutoCorr}{set to FALSE to not try to fit model with autocorrelation}
  \item{cSatFac}{Position of the initial saturation (0 start, 1 end, >1 outside measured range)}
}
\details{The flux is calculated as the slope of the concentration change. By
changing the concentration gradient, however, the flux is disturbed. In effect the 
flux will decline over time and concentrations go towards a saturation. 

This method fits a polynomial regression to the concentrations and infers the slope at reports
the slope at the initial time. Make sure to remove lag time period before.

Other functional forms can be fitted to estimate the initial slope:
\itemize{
\item{ Linear: \code{\link{regressFluxLinear}}  }
\item{ Hyperbolic tangent saturating function: \code{\link{regressFluxTanh}}  }
\item{ Exponential: \code{\link{regressFluxExp}}  }
\item{ Quadratic polinomial: \code{\link{regressFluxSquare}}  }
}

The hyperbolic tangent form (\code{\link{regressFluxTanh}}) has the advantage that 
initially the flux is changing only very slowly. In contrast, whith the exponential form the
slope changes much at the beginning.

The exponential form, is more consistent with a theoretical model of saturating flux (Kutzbach 2006).}
\value{
\item{stat}{numeric vector with 4 entries: \code{flux}, \code{sdFlux}, \code{AIC}, and \code{autoCorr}:}
\item{flux}{flux estimate at starting time}
\item{sdFlux}{standard deviation of flux}
\item{AIC}{model fit diagnostics}
\item{autoCorr}{coefficient of autocorrelation
or NA if model with autocorrelation could not be fitted or had higher AIC than model without autocorrelation}
\item{times}{used predictor vector, can be used for return for plotting}
\item{model}{the model-fit object (here of class gnls)
}}

\author{Thomas Wutzler, Oscar Perez Priego}



\seealso{\code{\link{RespChamberProc}}}
\examples{
#data(chamberLoggerEx1s)
ds <- chamberLoggerEx1s
conc <- ds$CO2_dry <- corrConcDilution(ds)  
times <- ds$TIMESTAMP
#trace(regressFluxExp, recover)	#untrace(regressFluxExp)
(res <- regressFluxExp( conc, times  ))
plot( conc ~ times)
lines( fitted(res$model) ~ times )
}
